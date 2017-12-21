{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeOperators, TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HsDev.Client.Commands (
	runClient, runCommand
	) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Exception (displayException)
import Control.Lens hiding ((%=), (.=), anyOf, (<.>))
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import qualified Control.Monad.State as State
import Control.Monad.Catch (try, catch, bracket, SomeException(..))
import Data.Aeson hiding (Result, Error)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T (append, isInfixOf, isPrefixOf, isSuffixOf)
import System.Directory
import System.FilePath
import qualified System.Log.Simple as Log
import qualified System.Log.Simple.Base as Log
import Text.Read (readMaybe)

import qualified System.Directory.Watcher as W
import System.Directory.Paths
import Text.Format
import HsDev.Error
import HsDev.Database.SQLite as SQLite
import HsDev.Server.Message as M
import HsDev.Server.Types
import HsDev.Sandbox hiding (findSandbox)
import qualified HsDev.Sandbox as S (findSandbox)
import HsDev.Symbols
import HsDev.Symbols.Util
import qualified HsDev.Symbols.Parsed as P
import qualified HsDev.Symbols.Name as Name
import qualified HsDev.Tools.AutoFix as AutoFix
import qualified HsDev.Tools.Cabal as Cabal
import HsDev.Tools.Ghc.Session
import HsDev.Tools.Ghc.Worker (clearTargets)
import qualified HsDev.Tools.Ghc.Compat as Compat
import qualified HsDev.Tools.Ghc.Check as Check
import qualified HsDev.Tools.Ghc.Types as Types
import qualified HsDev.Tools.Hayoo as Hayoo
import qualified HsDev.Tools.HLint as HLint
import qualified HsDev.Tools.Types as Tools
import HsDev.Util
import HsDev.Watcher

import qualified HsDev.Database.Update as Update

runClient :: (ToJSON a, ServerMonadBase m) => CommandOptions -> ClientM m a -> ServerM m Result
runClient copts = mapServerM toResult . runClientM where
	toResult :: (ToJSON a, ServerMonadBase m) => ReaderT CommandOptions m a -> m Result
	toResult act = liftM (either Error (Result . toJSON)) $ runReaderT (try act) copts
	mapServerM :: (m a -> n b) -> ServerM m a -> ServerM n b
	mapServerM f = ServerM . mapReaderT f . runServerM

toValue :: (ToJSON a, Monad m) => m a -> m Value
toValue = liftM toJSON

runCommand :: ServerMonadBase m => Command -> ClientM m Value
runCommand Ping = toValue $ return $ object ["message" .= ("pong" :: String)]
runCommand (Listen (Just l)) = case Log.level (pack l) of
	Nothing -> hsdevError $ OtherError $ "invalid log level: {}" ~~ l
	Just lev -> bracket (serverSetLogLevel lev) serverSetLogLevel $ \_ -> runCommand (Listen Nothing)
runCommand (Listen Nothing) = toValue $ do
	serverListen >>= mapM_ (commandNotify . Notification . toJSON)
runCommand (SetLogLevel l) = case Log.level (pack l) of
	Nothing -> hsdevError $ OtherError $ "invalid log level: {}" ~~ l
	Just lev -> toValue $ do
		lev' <- serverSetLogLevel lev
		Log.sendLog Log.Debug $ "log level changed from '{}' to '{}'" ~~ show lev' ~~ show lev
		Log.sendLog Log.Info $ "log level updated to: {}" ~~ show lev
runCommand (Scan projs cabal sboxes fs paths' ghcs' docs' infer') = toValue $ do
	sboxes' <- getSandboxes sboxes
	updateProcess (Update.UpdateOptions [] ghcs' docs' infer') $ concat [
		[Update.scanCabal ghcs' | cabal],
		map (Update.scanSandbox ghcs') sboxes',
		map (\(FileSource f mcts) -> Update.scanFileContents ghcs' f mcts) fs,
		map (Update.scanProject ghcs') projs,
		map (Update.scanDirectory ghcs') paths']
runCommand (RefineDocs projs fs) = toValue $ do
	projects <- traverse findProject projs
	mods <- withSqlTransaction $ do
		projMods <- liftM concat $ forM projects $ \proj -> do
			ms <- loadModules "select id from modules where cabal == ? and json_extract(tags, '$.docs') is null"
				(Only $ proj ^. projectCabal)
			p <- SQLite.loadProject (proj ^. projectCabal)
			return $ set (each . moduleId . moduleLocation . moduleProject) (Just p) ms
		fileMods <- liftM concat $ forM fs $ \f ->
			loadModules "select id from modules where file == ? and json_extract(tags, '$.docs') is null"
				(Only f)
		return $ projMods ++ fileMods
	updateProcess (Update.UpdateOptions [] [] False False) [Update.scanDocs mods]
runCommand (InferTypes projs fs) = toValue $ do
	projects <- traverse findProject projs
	mods <- withSqlTransaction $ do
		projMods <- liftM concat $ forM projects $ \proj -> do
			ms <- loadModules "select id from modules where cabal == ? and json_extract(tags, '$.types') is null"
				(Only $ proj ^. projectCabal)
			p <- SQLite.loadProject (proj ^. projectCabal)
			return $ set (each . moduleId . moduleLocation . moduleProject) (Just p) ms
		fileMods <- liftM concat $ forM fs $ \f ->
			loadModules "select id from modules where file == ? and json_extract(tags, '$.types') is null"
				(Only f)
		return $ projMods ++ fileMods
	updateProcess (Update.UpdateOptions [] [] False False) [Update.inferModTypes mods]
runCommand (Remove projs cabal sboxes files) = toValue $ do
	w <- askSession sessionWatcher
	projects <- traverse findProject projs
	sboxes' <- getSandboxes sboxes
	forM_ projects $ \proj -> do
		withSqlTransaction $ do
			ms <- liftM (map fromOnly) $ query "select id from modules where cabal == ?;" (Only $ proj ^. projectCabal)
			SQLite.removeProject proj
			mapM_ SQLite.removeModule ms
		liftIO $ unwatchProject w proj

	allPdbs <- liftM (map fromOnly) $ query_ @(Only PackageDb) "select package_db from package_dbs;"
	dbPDbs <- inSessionGhc $ mapM restorePackageDbStack allPdbs
	flip State.evalStateT dbPDbs $ do
		when cabal $ removePackageDbStack userDb
		forM_ sboxes' $ \sbox -> do
			pdbs <- lift $ inSessionGhc $ sandboxPackageDbStack sbox
			removePackageDbStack pdbs

	forM_ files $ \file -> do
		withSqlTransaction $ do
			ms <- query @_ @(ModuleId :. Only Int) (toQuery $ qModuleId `mappend` select_ ["mu.id"] [] ["mu.file == ?"]) (Only file)
			forM_ ms $ \(m :. Only i) -> do
				SQLite.removeModule i
				liftIO . unwatchModule w $ (m ^. moduleLocation)
	where
		-- We can safely remove package-db from db iff doesn't used by some of other package-dbs
		-- For example, we can't remove global-db if there are any other package-dbs, because all of them uses global-db
		-- We also can't remove stack snapshot package-db if there are some local package-db not yet removed
		canRemove pdbs = do
			from' <- State.get
			return $ null $ filter (pdbs `isSubStack`) $ delete pdbs from'
		-- Remove top of package-db stack if possible
		removePackageDb' pdbs = do
			w <- lift $ askSession sessionWatcher
			can <- canRemove pdbs
			when can $ do
				State.modify (delete pdbs)
				withSqlTransaction $ do
					ms <- liftM (map fromOnly) $ query_
						"select m.id from modules as m, package_dbs as ps where m.package_name == ps.package_name and m.package_version == ps.package_version;"
					removePackageDb (topPackageDb pdbs)
					mapM_ SQLite.removeModule ms
				liftIO $ unwatchPackageDb w $ topPackageDb pdbs
		-- Remove package-db stack when possible
		removePackageDbStack = mapM_ removePackageDb' . packageDbStacks
runCommand RemoveAll = toValue $ do
	withSqlTransaction SQLite.purge
	w <- askSession sessionWatcher
	wdirs <- liftIO $ readMVar (W.watcherDirs w)
	liftIO $ forM_ (M.toList wdirs) $ \(dir, (isTree, _)) -> (if isTree then W.unwatchTree else W.unwatchDir) w dir
runCommand InfoPackages = toValue $
	query_ @ModulePackage "select package_name, package_version from package_dbs;"
runCommand InfoProjects = toValue $ do
	ps <- query_ @(Only Int :. Project) "select p.id, p.name, p.cabal, p.version from projects as p;"
	forM ps $ \(Only pid :. proj) -> do
		libs <- query @_ @Library "select l.modules, b.depends, b.language, b.extensions, b.ghc_options, b.source_dirs, b.other_modules from libraries as l, build_infos as b where (l.project_id == ?) and (l.build_info_id == b.id);"
			(Only pid)
		exes <- query @_ @Executable "select e.name, e.path, b.depends, b.language, b.extensions, b.ghc_options, b.source_dirs, b.other_modules from executables as e, build_infos as b where (e.project_id == ?) and (e.build_info_id == b.id);"
			(Only pid)
		tsts <- query @_ @Test "select t.name, t.enabled, t.main, b.depends, b.language, b.extensions, b.ghc_options, b.source_dirs, b.other_modules from tests as t, build_infos as b where (t.project_id == ?) and (t.build_info_id == b.id);"
			(Only pid)
		return $
			set (projectDescription . _Just . projectLibrary) (listToMaybe libs) .
			set (projectDescription . _Just . projectExecutables) exes .
			set (projectDescription . _Just . projectTests) tsts $
			proj
runCommand InfoSandboxes = toValue $ do
	rs <- query_ @(Only PackageDb) "select distinct package_db from package_dbs;"
	return [pdb | Only pdb <- rs]
runCommand (InfoSymbol sq fs True _) = toValue $ do
	rs <- query @_ @SymbolId (toQuery $ qSymbolId `mappend` where_ ["s.name like ?"])
		(Only $ likePattern sq)
	return rs
runCommand (InfoSymbol sq fs False _) = toValue $ do
	rs <- query @_ @Symbol (toQuery $ qSymbol `mappend` where_ ["s.name like ?"])
		(Only $ likePattern sq)
	return rs
runCommand (InfoModule sq fs h i) = toValue $ do
	rs <- query @_ @(Only Int :. ModuleId) (toQuery $ select_ ["mu.id"] [] [] `mappend` qModuleId `mappend` where_ ["mu.name like ?"])
		(Only $ likePattern sq)
	if h
		then return (toJSON $ map (\(_ :. m) -> m) rs)
		else liftM toJSON $ forM rs $ \(Only mid :. mheader) -> do
			[(docs, fixities)] <- query @_ @(Maybe Text, Maybe Value) "select m.docs, m.fixities from modules as m where (m.id == ?);"
				(Only mid)
			let
				fixities' = fromMaybe [] (fixities >>= fromJSON')
			exports' <- query @_ @Symbol (toQuery $ qSymbol `mappend` select_ []
				["exports as e"]
				["e.module_id == ?", "e.symbol_id == s.id"])
				(Only mid)
			return $ Module mheader docs exports' fixities' mempty Nothing
runCommand (InfoProject (Left projName)) = toValue $ findProject projName
runCommand (InfoProject (Right projPath)) = toValue $ liftIO $ searchProject (view path projPath)
runCommand (InfoSandbox sandbox') = toValue $ liftIO $ searchSandbox sandbox'
runCommand (Lookup nm fpath) = toValue $ do
	rs <- query @_ @Symbol (toQuery $ qSymbol `mappend` select_ []
		["projects as p", "projects_deps as pdeps", "modules as srcm"]
		[
			"p.id == pdeps.project_id",
			"m.cabal == p.cabal or m.package_name == pdeps.package_name",
			"p.cabal == srcm.cabal",
			"srcm.file == ?",
			"s.name == ?"])
		(fpath ^. path, nm)
	return rs
runCommand (Whois nm fpath) = toValue $ do
	let
		q = nameModule $ toName nm
		ident = nameIdent $ toName nm
	rs <- query @_ @Symbol (toQuery $ qSymbol `mappend` select_ []
		["modules as srcm", "scopes as sc"]
		[
			"srcm.id == sc.module_id",
			"s.id == sc.symbol_id",
			"srcm.file == ?",
			"sc.qualifier is ?",
			"sc.name == ?"])
		(fpath ^. path, q, ident)
	return rs
runCommand (Whoat l c fpath) = toValue $ do
	rs <- query @_ @Symbol (toQuery $ qSymbol `mappend` select_ []
		["names as n", "modules as srcm", "projects_modules_scope as msc"]
		[
			"srcm.id == n.module_id",
			"m.name == n.resolved_module",
			"s.name == n.resolved_name",
			"msc.cabal is srcm.cabal",
			"m.id == msc.module_id",
			"srcm.file == ?",
			"(?, ?) between (n.line, n.column) and (n.line_to, n.column_to)"])
		(fpath ^. path, l, c)
	return rs
runCommand (ResolveScopeModules sq fpath) = toValue $ do
	pids <- query @_ @(Only Int) "select m.cabal from modules as m where (m.file == ?);"
		(Only $ fpath ^. path)
	case pids of
		[] -> query @_ @ModuleId (toQuery $ qModuleId `mappend` select_ []
			["latest_packages as ps"]
			[
				"mu.package_name == ps.package_name",
				"mu.package_version == ps.package_version",
				"ps.package_db in ('user-db', 'global-db')",
				"mu.name like ?"])
			(Only $ likePattern sq)
		[Only proj] -> query @_ @ModuleId (toQuery $ qModuleId `mappend` select_ []
			["projects_modules_scope as msc"]
			[
				"msc.module_id == mu.id",
				"msc.cabal is ?",
				"mu.name like ?"])
			(proj, likePattern sq)
		_ -> fail "Impossible happened: several projects for one module"
runCommand (ResolveScope sq fpath) = toValue $ do
	rs <- query @_ @SymbolId (toQuery $ qSymbolId `mappend` select_ []
		["scopes as sc", "modules as srcm"]
		[
			"srcm.id == sc.module_id",
			"sc.symbol_id == s.id",
			"srcm.file == ?",
			"s.name like ?"])
		(fpath ^. path, likePattern sq)
	return rs
runCommand (FindUsages nm) = toValue $ do
	let
		q = nameModule $ toName nm
		ident = nameIdent $ toName nm
	rs <- query @_ @SymbolUsage (toQuery $ qSymbol `mappend` qModuleId `mappend` select_
		["n.line", "n.column"]
		["names as n"]
		[
			"m.name == n.resolved_module",
			"s.name == n.resolved_name",
			"mu.id == n.module_id",
			"n.resolved_module == ? or ? is null",
			"n.resolved_name == ?"])
		(q, q, ident)
	return rs
runCommand (Complete input True fpath) = toValue $ do
	rs <- query @_ @Symbol (toQuery $ qSymbol `mappend` select_ []
		[
			"projects_modules_scope as msc",
			"modules as srcm"]
		[
			"msc.cabal is srcm.cabal",
			"msc.module_id == m.id",
			"msrc.file == ?",
			"s.name like ?"])
		(fpath ^. path, input `T.append` "%", fpath ^. path, input `T.append` "%")
	return rs
runCommand (Complete input False fpath) = toValue $ do
	rs <- query @_ @Symbol (toQuery $ qSymbol `mappend` select_ []
		["completions as c", "modules as srcm"]
		[
			"c.module_id == srcm.id",
			"c.symbol_id == s.id",
			"srcm.file == ?",
			"c.completion like ?"])
		(fpath ^. path, input `T.append` "%")
	return rs
runCommand (Hayoo hq p ps) = toValue $ liftM concat $ forM [p .. p + pred ps] $ \i -> liftM
	(mapMaybe Hayoo.hayooAsSymbol . Hayoo.resultResult) $
	liftIO $ hsdevLift $ Hayoo.hayoo hq (Just i)
runCommand (CabalList packages') = toValue $ liftIO $ hsdevLift $ Cabal.cabalList $ map unpack packages'
runCommand (UnresolvedSymbols fs) = toValue $ liftM concat $ forM fs $ \f -> do
	rs <- query @_ @(Maybe String, String, Int, Int) "select n.qualifier, n.name, n.line, n.column from modules as m, names as n where (m.id == n.module_id) and (m.file == ?) and (n.resolve_error is not null);"
		(Only $ f ^. path)
	return $ map (\(m, nm, line, column) -> object [
		"qualifier" .= m,
		"name" .= nm,
		"line" .= line,
		"column" .= column]) rs
runCommand (Lint fs) = toValue $ do
	liftIO $ hsdevLift $ liftM concat $ mapM (\(FileSource f c) -> HLint.hlint (view path f) c) fs
runCommand (Check fs ghcs' clear) = toValue $ Log.scope "check" $ do
	-- ensureUpToDate (Update.UpdateOptions [] ghcs' False False) fs
	let
		checkSome file fn = Log.scope "checkSome" $ do
			Log.sendLog Log.Trace $ "setting file source session for {}" ~~ file
			m <- setFileSourceSession ghcs' file
			Log.sendLog Log.Trace $ "file source session set"
			inSessionGhc $ do
				when clear $ clearTargets
				fn m
	liftM concat $ mapM (\(FileSource f c) -> checkSome f (\m -> Check.check [] m c)) fs
runCommand (CheckLint fs ghcs' clear) = toValue $ do
	-- ensureUpToDate (Update.UpdateOptions [] ghcs' False False) fs
	let
		checkSome file fn = Log.scope "checkSome" $ do
			m <- setFileSourceSession ghcs' file
			inSessionGhc $ do
				when clear $ clearTargets
				fn m
	checkMsgs <- liftM concat $ mapM (\(FileSource f c) -> checkSome f (\m -> Check.check [] m c)) fs
	lintMsgs <- liftIO $ hsdevLift $ liftM concat $ mapM (\(FileSource f c) -> HLint.hlint (view path f) c) fs
	return $ checkMsgs ++ lintMsgs
runCommand (Types fs ghcs' clear) = toValue $ do
	-- ensureUpToDate (Update.UpdateOptions [] ghcs' False False) fs
	liftM concat $ forM fs $ \(FileSource file msrc) -> do
		m <- setFileSourceSession ghcs' file
		inSessionGhc $ do
			when clear $ clearTargets
			Types.fileTypes [] m msrc
runCommand (AutoFix ns) = toValue $ return $ AutoFix.corrections ns
runCommand (Refactor ns rest isPure) = toValue $ do
	files <- liftM (ordNub . sort) $ mapM findPath $ mapMaybe (preview $ Tools.noteSource . moduleFile) ns
	let
		runFix file = do
			when (not isPure) $ do
				liftIO $ readFileUtf8 (view path file) >>= writeFileUtf8 (view path file) . AutoFix.refact fixRefacts'
			return newCorrs'
			where
				findCorrs :: Path -> [Tools.Note AutoFix.Refact] -> [Tools.Note AutoFix.Refact]
				findCorrs f = filter ((== Just f) . preview (Tools.noteSource . moduleFile))
				fixCorrs' = findCorrs file ns
				upCorrs' = findCorrs file rest
				fixRefacts' = fixCorrs' ^.. each . Tools.note
				newCorrs' = AutoFix.update fixRefacts' upCorrs'
	liftM concat $ mapM runFix files
runCommand (Rename nm newName fpath) = toValue $ do
	m <- refineSourceModule fpath
	let
		mname = m ^. moduleId . moduleName
		makeNote mloc r = Tools.Note {
			Tools._noteSource = mloc,
			Tools._noteRegion = r,
			Tools._noteLevel = Nothing,
			Tools._note = AutoFix.Refact "rename" (AutoFix.replace (AutoFix.fromRegion r) newName) }

	defRenames <- do
		-- FIXME: Doesn't take scope into account. If you have modules with same names in different project, it will rename symbols from both
		defRegions <- query @_ @Region "select n.line, n.column, n.line_to, n.column_to from names as n, modules as m where m.id == n.module_id and m.name == ? and n.name == ? and def_line is not null;" (
			mname,
			nm)
		return $ map (makeNote (m ^. moduleId . moduleLocation)) defRegions

	usageRenames <- do
		-- FIXME: Same as above: doesn't take scope into account
		usageRegions <- query @_ @(Only Path :. Region) "select m.file, n.line, n.column, n.line_to, n.column_to from names as n, modules as m where n.module_id == m.id and m.file is not null and n.resolved_module == ? and n.resolved_name == ?;" (
			mname,
			nm)
		return $ map (\(Only p :. r) -> makeNote (FileModule p Nothing) r) usageRegions

	return $ defRenames ++ usageRenames
runCommand (GhcEval exprs mfile) = toValue $ do
	-- ensureUpToDate (Update.UpdateOptions [] [] False False) (maybeToList mfile)
	ghcw <- askSession sessionGhc
	case mfile of
		Nothing -> inSessionGhc ghciSession
		Just (FileSource f mcts) -> do
			m <- setFileSourceSession [] f
			inSessionGhc $ interpretModule m mcts
	async' <- liftIO $ pushTask ghcw $ do
		mapM (try . evaluate) exprs
	res <- waitAsync async'
	return $ map toValue' res
	where
		waitAsync :: CommandMonad m => Async a -> m a
		waitAsync a = liftIO (waitCatch a) >>= either (hsdevError . GhcError . displayException) return
		toValue' :: ToJSON a => Either SomeException a -> Value
		toValue' (Left (SomeException e)) = object ["fail" .= show e]
		toValue' (Right s) = toJSON s
runCommand Langs = toValue $ return $ Compat.languages
runCommand Flags = toValue $ return ["-f" ++ prefix ++ f |
	f <- Compat.flags,
	prefix <- ["", "no-"]]
runCommand (Link hold) = toValue $ commandLink >> when hold commandHold
runCommand Exit = toValue serverExit

-- TODO: Implement `targetFilter` for sql

-- targetFilter :: (CommandMonad m, Sourced a) => TargetFilter -> m (a -> Bool)
-- targetFilter f = liftM (. view sourcedModule) $ case f of
-- 	TargetProject proj -> liftM inProject $ findProject proj
-- 	TargetFile file -> liftM inFile $ refineSourceFile file
-- 	TargetModule mname -> return $ inModule mname
-- 	TargetPackageDb pdb -> do
-- 		dbval <- serverDatabase
-- 		let
-- 			pkgs = dbval ^. databasePackageDbs . ix pdb
-- 			mlocs = S.fromList $ concat $ catMaybes [M.lookup pkg (view databasePackages dbval) | pkg <- pkgs]
-- 		return $ \m -> S.member (m ^. sourcedModule . moduleLocation) mlocs
-- 	TargetCabal -> do
-- 		dbval <- serverDatabase
-- 		let
-- 			pkgs = concat $ catMaybes [dbval ^? databasePackageDbs . ix pdb | pdb <- packageDbs userDb]
-- 			mlocs = S.fromList $ concat $ catMaybes [M.lookup pkg (view databasePackages dbval) | pkg <- pkgs]
-- 		return $ \m -> S.member (m ^. sourcedModule . moduleLocation) mlocs
-- 	TargetSandbox sbox -> do
-- 		dbval <- serverDatabase
-- 		pdbs <- findSandbox sbox >>= inSessionGhc . sandboxPackageDbStack
-- 		let
-- 			pkgs = concat [dbval ^. databasePackageDbs . ix pdb | pdb <- packageDbs pdbs]
-- 			mlocs = S.fromList $ concat $ catMaybes [M.lookup pkg (view databasePackages dbval) | pkg <- pkgs]
-- 		return $ \m -> S.member (m ^. sourcedModule . moduleLocation) mlocs
-- 	TargetPackage pkg -> liftM (inPackage . mkPackage) $ refinePackage pkg
-- 	TargetSourced -> return byFile
-- 	TargetStandalone -> return standalone

likePattern :: SearchQuery -> Text
likePattern (SearchQuery input stype) = case stype of
	SearchExact -> input
	SearchPrefix -> input `T.append` "%"
	SearchInfix -> "%" `T.append` input `T.append` "%"
	SearchSuffix -> "%" `T.append` input

instance ToJSON Log.Message where
	toJSON m = object [
		"time" .= Log.messageTime m,
		"level" .= show (Log.messageLevel m),
		"component" .= show (Log.messageComponent m),
		"scope" .= show (Log.messageScope m),
		"text" .= Log.messageText m]

instance FromJSON Log.Message where
	parseJSON = withObject "log-message" $ \v -> Log.Message <$>
		(v .:: "time") <*>
		((v .:: "level") >>= maybe (fail "invalid level") return . readMaybe) <*>
		(read <$> (v .:: "component")) <*>
		(read <$> (v .:: "scope")) <*>
		(v .:: "text")

-- Helper functions

-- | Canonicalize paths
findPath :: (CommandMonad m, Paths a) => a -> m a
findPath = paths findPath' where
	findPath' :: CommandMonad m => FilePath -> m FilePath
	findPath' f = do
		r <- commandRoot
		liftIO $ canonicalizePath (normalise $ if isRelative f then r </> f else f)

-- | Find sandbox by path
findSandbox :: CommandMonad m => Path -> m Sandbox
findSandbox fpath = do
	fpath' <- findPath fpath
	sbox <- liftIO $ S.findSandbox fpath'
	maybe (hsdevError $ FileNotFound fpath') return sbox

-- | Get source file
refineSourceFile :: CommandMonad m => Path -> m Path
refineSourceFile fpath = do
	fpath' <- findPath fpath
	fs <- liftM (map fromOnly) $ query "select file from modules where file == ?;" (Only fpath')
	case fs of
		[] -> hsdevError (NotInspected $ FileModule fpath' Nothing)
		(f:_) -> do
			when (length fs > 1) $ Log.sendLog Log.Warning $ "multiple modules with same file = {}" ~~ fpath'
			return f

-- | Get module by source
refineSourceModule :: CommandMonad m => Path -> m Module
refineSourceModule fpath = do
	fpath' <- findPath fpath
	ids <- query "select id, cabal from modules where file == ?;" (Only fpath')
	case ids of
		[] -> hsdevError (NotInspected $ FileModule fpath' Nothing)
		((i, mcabal):_) -> do
			when (length ids > 1) $ Log.sendLog Log.Warning $ "multiple modules with same file = {}" ~~ fpath'
			m <- SQLite.loadModule i
			case mcabal of
				Nothing -> return m
				Just cabal' -> do
					proj' <- SQLite.loadProject cabal'
					return $ set (moduleId . moduleLocation . moduleProject) (Just proj') m

-- | Set session by source
setFileSourceSession :: CommandMonad m => [String] -> Path -> m Module
setFileSourceSession opts fpath = do
	m <- refineSourceModule fpath
	inSessionGhc $ targetSession opts m
	return m

-- | Ensure package exists
refinePackage :: CommandMonad m => Text -> m Text
refinePackage pkg = do
	[(Only exists)] <- query "select count(*) > 0 from package_dbs where package_name == ?;" (Only pkg)
	when (not exists) $ hsdevError (PackageNotFound pkg)
	return pkg

-- | Get list of enumerated sandboxes
getSandboxes :: CommandMonad m => [Path] -> m [Sandbox]
getSandboxes = traverse (findPath >=> findSandbox)

-- | Find project by name or path
findProject :: CommandMonad m => Text -> m Project
findProject proj = do
	proj' <- liftM addCabal $ findPath proj
	ps <- liftM (map fromOnly) $ query "select cabal from projects where (cabal == ?) or (name == ?);" (view path proj', proj)
	case ps of
		[] -> hsdevError $ ProjectNotFound proj
		_ -> SQLite.loadProject (head ps)
	where
		addCabal p
			| takeExtension (view path p) == ".cabal" = p
			| otherwise = over path (\p' -> p' </> (takeBaseName p' <.> "cabal")) p

-- | Run DB update action
updateProcess :: ServerMonadBase m => Update.UpdateOptions -> [Update.UpdateM m ()] -> ClientM m ()
updateProcess uopts acts = mapM_ (Update.runUpdate uopts . runAct) acts where
	runAct act = catch act onError
	onError e = Log.sendLog Log.Error $ "{}" ~~ (e :: HsDevError)

-- | Ensure file is up to date
-- ensureUpToDate :: ServerMonadBase m => Update.UpdateOptions -> [FileSource] -> ClientM m ()
-- ensureUpToDate uopts fs = updateProcess uopts [Update.scanFileContents (view Update.updateGhcOpts uopts) f mcts | FileSource f mcts <- fs]

-- | Check matching search query
matchQuery :: Sourced a => SearchQuery -> a -> Bool
matchQuery (SearchQuery sq st) s = case st of
	SearchExact -> sq == sn
	SearchPrefix -> sq `T.isPrefixOf` sn
	SearchInfix -> sq `T.isInfixOf` sn
	SearchSuffix -> sq `T.isSuffixOf` sn
	where
		sn = view sourcedName s
