{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeOperators, TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HsDev.Client.Commands (
	runClient, runCommand
	) where

import Control.Arrow (second)
import Control.Concurrent.MVar
import Control.Exception (displayException)
import Control.Lens hiding ((.=), (<.>))
import Control.Monad
import Control.Monad.Morph
import Control.Monad.Except
import Control.Monad.Reader
import qualified Control.Monad.State as State
import Control.Monad.Catch (try, catch, bracket, SomeException(..))
import Data.Aeson hiding (Result, Error)
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T (append, null)
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
import HsDev.Inspect (preload, asModule)
import HsDev.Scan (upToDate, getFileContents)
import HsDev.Server.Message as M
import HsDev.Server.Types
import HsDev.Sandbox hiding (findSandbox)
import qualified HsDev.Sandbox as S (findSandbox)
import HsDev.Symbols
import qualified HsDev.Tools.AutoFix as AutoFix
import qualified HsDev.Tools.Cabal as Cabal
import HsDev.Tools.Ghc.Session
import HsDev.Tools.Ghc.Worker (clearTargets)
import qualified HsDev.Tools.Ghc.Compat as Compat
import qualified HsDev.Tools.Ghc.Check as Check
import qualified HsDev.Tools.Ghc.Types as Types
import qualified HsDev.Tools.Hayoo as Hayoo
import qualified HsDev.Tools.HDocs as HDocs
import qualified HsDev.Tools.HLint as HLint
import qualified HsDev.Tools.Types as Tools
import HsDev.Util
import HsDev.Watcher

import qualified HsDev.Database.Update as Update

runClient :: (ToJSON a, ServerMonadBase m) => CommandOptions -> ClientM m a -> ServerM m Result
runClient copts = mapServerM toResult . runClientM where
	toResult :: (ToJSON a, ServerMonadBase m) => ReaderT CommandOptions m a -> m Result
	toResult act = liftM errorToResult $ runReaderT (try (try act)) copts
	mapServerM :: (m a -> n b) -> ServerM m a -> ServerM n b
	mapServerM f = ServerM . mapReaderT f . runServerM
	errorToResult :: ToJSON a => Either SomeException (Either HsDevError a) -> Result
	errorToResult = either (Error . UnhandledError . displayException) (either Error (Result . toJSON))

toValue :: (ToJSON a, Monad m) => m a -> m Value
toValue = fmap toJSON

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
runCommand (Scan projs cabal sboxes fs paths' btool ghcs' docs' infer') = toValue $ do
	sboxes' <- getSandboxes sboxes
	updateProcess (Update.UpdateOptions [] ghcs' docs' infer') $ concat [
		[Update.scanCabal ghcs' | cabal],
		map (Update.scanSandbox ghcs') sboxes',
		[Update.scanFiles (zip fs (repeat ghcs'))],
		map (Update.scanProject ghcs' btool) projs,
		map (Update.scanDirectory ghcs') paths']
runCommand (ScanProject proj tool deps) = toValue $ updateProcess def [
	(if deps then Update.scanProjectStack else Update.scanProject) [] tool proj]
runCommand (ScanFile fpath tool scanProj deps) = toValue $ updateProcess def [
	Update.scanFile [] fpath tool scanProj deps]
runCommand (ScanPackageDbs pdbs) = toValue $ updateProcess def [
	Update.scanPackageDbStack [] pdbs]
runCommand (SetFileContents f mcts) = toValue $ serverSetFileContents f mcts
runCommand (RefineDocs projs fs)
	| HDocs.hdocsSupported = toValue $ do
		projects <- traverse findProject projs
		mods <- do
			projMods <- liftM concat $ forM projects $ \proj -> do
				ms <- loadModules "select id from modules where cabal == ? and json_extract(tags, '$.docs') is null"
					(Only $ proj ^. projectCabal)
				p <- SQLite.loadProject (proj ^. projectCabal)
				return $ set (each . moduleId . moduleLocation . moduleProject) (Just p) ms
			fileMods <- liftM concat $ forM fs $ \f ->
				loadModules "select id from modules where file == ? and json_extract(tags, '$.docs') is null"
					(Only f)
			return $ projMods ++ fileMods
		updateProcess def [Update.scanDocs mods]
	| otherwise = hsdevError $ OtherError "docs not supported"
runCommand (InferTypes projs fs) = toValue $ do
	projects <- traverse findProject projs
	mods <- do
		projMods <- liftM concat $ forM projects $ \proj -> do
			ms <- loadModules "select id from modules where cabal == ? and json_extract(tags, '$.types') is null"
				(Only $ proj ^. projectCabal)
			p <- SQLite.loadProject (proj ^. projectCabal)
			return $ set (each . moduleId . moduleLocation . moduleProject) (Just p) ms
		fileMods <- liftM concat $ forM fs $ \f ->
			loadModules "select id from modules where file == ? and json_extract(tags, '$.types') is null"
				(Only f)
		return $ projMods ++ fileMods
	updateProcess def [Update.inferModTypes mods]
runCommand (Remove projs cabal sboxes files) = toValue $ withSqlConnection $ SQLite.transaction_ SQLite.Immediate $ do
	let
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
				ms <- liftM (map fromOnly) $ query_
					"select m.id from modules as m, package_dbs as ps where m.package_name == ps.package_name and m.package_version == ps.package_version;"
				removePackageDb (topPackageDb pdbs)
				mapM_ SQLite.removeModule ms
				liftIO $ unwatchPackageDb w $ topPackageDb pdbs
		-- Remove package-db stack when possible
		removePackageDbStack = mapM_ removePackageDb' . packageDbStacks
	w <- askSession sessionWatcher
	projects <- traverse findProject projs
	sboxes' <- getSandboxes sboxes
	forM_ projects $ \proj -> do
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
		ms <- query @_ @(ModuleId :. Only Int)
			(toQuery $ mconcat [
				qModuleId,
				select_ ["mu.id"],
				where_ ["mu.file == ?"]])
			(Only file)
		forM_ ms $ \(m :. Only i) -> do
			SQLite.removeModule i
			liftIO . unwatchModule w $ (m ^. moduleLocation)
runCommand RemoveAll = toValue $ do
	SQLite.purge
	w <- askSession sessionWatcher
	wdirs <- liftIO $ readMVar (W.watcherDirs w)
	liftIO $ forM_ (M.toList wdirs) $ \(dir, (isTree, _)) -> (if isTree then W.unwatchTree else W.unwatchDir) w dir
runCommand InfoPackages = toValue $
	query_ @ModulePackage "select package_name, package_version from package_dbs;"
runCommand InfoProjects = toValue $ do
	ps <- query_ @(Only Path) "select cabal from projects;"
	mapM (SQLite.loadProject . fromOnly) ps
runCommand InfoSandboxes = toValue $ do
	rs <- query_ @(Only PackageDb) "select distinct package_db from package_dbs;"
	return [pdb | Only pdb <- rs]
runCommand (InfoSymbol sq filters True _) = toValue $ do
	let
		(conds, params) = targetFilters "m" (Just "s") filters
	queryNamed @SymbolId
		(toQuery $ mconcat [
			qSymbolId,
			where_ ["s.name like :pattern escape '\\'"],
			where_ conds])
		([":pattern" := likePattern sq] ++ params)
runCommand (InfoSymbol sq filters False _) = toValue $ do
	let
		(conds, params) = targetFilters "m" (Just "s") filters
	queryNamed @Symbol
		(toQuery $ mconcat [
			qSymbol,
			where_ ["s.name like :pattern escape '\\'"],
			where_ conds])
		([":pattern" := likePattern sq] ++ params)
runCommand (InfoModule sq filters h _) = toValue $ do
	let
		(conds, params) = targetFilters "mu" Nothing filters
	rs <- queryNamed @(Only Int :. ModuleId)
		(toQuery $ mconcat [
			select_ ["mu.id"],
			qModuleId,
			where_ ["mu.name like :pattern escape '\\'"],
			where_ conds])
		([":pattern" := likePattern sq] ++ params)
	if h
		then return (toJSON $ map (\(_ :. m) -> m) rs)
		else liftM toJSON $ forM rs $ \(Only mid :. mheader) -> do
			[(docs, fixities)] <- query @_ @(Maybe Text, Maybe Value) "select m.docs, m.fixities from modules as m where (m.id == ?);"
				(Only mid)
			let
				fixities' = fromMaybe [] (fixities >>= fromJSON')
			imports' <- query @_ @Import (toQuery $ mconcat [
				qImport "i",
				where_ ["i.module_id = ?"]])
				(Only mid)
			exports' <- query @_ @Symbol (toQuery $ mconcat [
				qSymbol,
				from_ ["exports as e"],
				where_ ["e.module_id == ?", "e.symbol_id == s.id"]])
				(Only mid)
			return $ Module mheader docs imports' exports' fixities' mempty Nothing
runCommand (InfoProject (Left projName)) = toValue $ findProject projName
runCommand (InfoProject (Right projPath)) = toValue $ liftIO $ searchProject (view path projPath)
runCommand (InfoSandbox sandbox') = toValue $ liftIO $ searchSandboxes sandbox'
runCommand (Lookup nm fpath) = toValue $
	fmap (map (\(s :. m) -> ImportedSymbol s m)) $ query @_ @(Symbol :. ModuleId) (toQuery $ mconcat [
		qSymbol,
		qModuleId,
		from_ ["projects_modules_scope as pms", "modules as srcm", "exports as e"],
		where_ [
			"pms.cabal is srcm.cabal",
			"srcm.file = ?",
			"pms.module_id = e.module_id",
			"m.id = s.module_id",
			"s.id = e.symbol_id",
			"e.module_id = mu.id",
			"s.name = ?"]])
		(fpath ^. path, nm)
runCommand (Whois nm fpath) = toValue $ do
	let
		q = nameModule $ toName nm
		ident = nameIdent $ toName nm
	query @_ @Symbol (toQuery $ mconcat [
		qSymbol,
		from_ ["modules as srcm", "scopes as sc"],
		where_ [
			"srcm.id == sc.module_id",
			"s.id == sc.symbol_id",
			"srcm.file == ?",
			"sc.qualifier is ?",
			"sc.name == ?"]])
		(fpath ^. path, q, ident)
runCommand (Whoat l c fpath) = toValue $ do
	rs <- query @_ @Symbol (toQuery $ mconcat [
		qSymbol,
		from_ ["names as n", "modules as srcm"],
		where_ [
			"srcm.id == n.module_id",
			"m.name == n.resolved_module",
			"s.name == n.resolved_name",
			"s.what == n.resolved_what",
			"s.id == n.symbol_id",
			"srcm.file == ?",
			"(?, ?) between (n.line, n.column) and (n.line_to, n.column_to)"]])
		(fpath ^. path, l, c)
	locals <- do
		defs <- query @_ @(ModuleId :. (Text, Int, Int, Maybe Text)) (toQuery $ mconcat [
			qModuleId,
			select_ ["n.name", "n.def_line", "n.def_column", "n.inferred_type"],
			from_ ["names as n"],
			where_ [
				"mu.id == n.module_id",
				"n.def_line is not null",
				"n.def_column is not null",
				"mu.file == ?",
				"(?, ?) between (n.line, n.column) and (n.line_to, n.column_to)"]])
			(fpath ^. path, l, c)
		return [
			Symbol {
				_symbolId = SymbolId nm mid,
				_symbolDocs = Nothing,
				_symbolPosition = Just (Position defLine defColumn),
				_symbolInfo = Function ftype
			} | (mid :. (nm, defLine, defColumn, ftype)) <- defs]
	return $ rs ++ locals
runCommand (ResolveScopeModules sq fpath) = toValue $ do
	pids <- query @_ @(Only (Maybe Path)) "select m.cabal from modules as m where (m.file == ?);"
		(Only $ fpath ^. path)
	case pids of
		[] -> hsdevError $ OtherError $ "module at {} not found" ~~ fpath
		[Only proj] -> query @_ @ModuleId (toQuery $ mconcat [
			qModuleId,
			from_ ["projects_modules_scope as msc"],
			where_ [
				"msc.module_id == mu.id",
				"msc.cabal is ?",
				"mu.name like ? escape '\\'"]])
			(proj, likePattern sq)
		_ -> fail "Impossible happened: several projects for one module"
runCommand (ResolveScope sq fpath) = toValue $
	query @_ @(Scoped SymbolId) (toQuery $ mconcat [
		qSymbolId,
		select_ ["sc.qualifier"],
		from_ ["scopes as sc", "modules as srcm"],
		where_ [
			"srcm.id == sc.module_id",
			"sc.symbol_id == s.id",
			"srcm.file == ?",
			"s.name like ? escape '\\'"]])
		(fpath ^. path, likePattern sq)
runCommand (FindUsages l c fpath) = toValue $ do
	us <- do
		sids <- query @_ @(Only (Maybe Int)) (toQuery $ mconcat [
			select_ ["n.symbol_id"],
			from_ ["names as n", "modules as srcm"],
			where_ [
				"n.module_id == srcm.id",
				"(?, ?) between (n.line, n.column) and (n.line_to, n.column_to)",
				"srcm.file = ?"]])
			(l, c, fpath)
		when (length sids > 1) $ Log.sendLog Log.Warning $ "multiple symbols found at location {0}:{1}:{2}" ~~ fpath ~~ l ~~ c
		let
			msid = join $ fmap fromOnly $ listToMaybe sids
		query @_ @SymbolUsage (toQuery $ mconcat [
			qSymbol,
			select_ ["n.qualifier"],
			qModuleId,
			select_ ["n.line", "n.column", "n.line_to", "n.column_to"],
			from_ ["names as n"],
			where_ [
				"n.symbol_id == ?",
				"s.id == n.symbol_id",
				"mu.id == n.module_id"]])
			(Only msid)
	locals <- do
		defs <- query @_ @(ModuleId :. Only Text :. Position :. Only (Maybe Text) :. Region) (toQuery $ mconcat [
			qModuleId,
			select_ ["n.name", "n.def_line", "n.def_column", "n.inferred_type", "n.line", "n.column", "n.line_to", "n.column_to"],
			from_ ["names as n", "names as defn"],
			where_ [
				"n.module_id = mu.id",
				"n.def_line = defn.def_line",
				"n.def_column = defn.def_column",
				"defn.module_id = mu.id",
				"(?, ?) between (defn.line, defn.column) and (defn.line_to, defn.column_to)",
				"mu.file = ?"]])
			(l, c, fpath ^. path)
		return $ do
			(mid :. Only nm :. defPos :. Only ftype :. useRgn) <- defs
			let
				sym = Symbol {
					_symbolId = SymbolId nm mid,
					_symbolDocs = Nothing,
					_symbolPosition = Just defPos,
					_symbolInfo = Function ftype }
			return $ SymbolUsage sym Nothing mid useRgn
	return $ us ++ locals
runCommand (Complete input True fpath) = toValue $
	query @_ @Symbol (toQuery $ mconcat [
		qSymbol,
		from_ ["modules as srcm", "exports as e"],
		where_ [
			"e.module_id in (select srcm.id union select module_id from projects_modules_scope where (((cabal is null) and (srcm.cabal is null)) or (cabal == srcm.cabal)))",
			"s.id == e.symbol_id",
			"msrc.file == ?",
			"s.name like ? escape '\\'"]])
		(fpath ^. path, likePattern (SearchQuery input SearchPrefix))
runCommand (Complete input False fpath) = toValue $
	query @_ @(Scoped Symbol) (toQuery $ mconcat [
		qSymbol,
		select_ ["c.qualifier"],
		from_ ["completions as c", "modules as srcm"],
		where_ [
			"c.module_id == srcm.id",
			"c.symbol_id == s.id",
			"srcm.file == ?",
			"c.completion like ? escape '\\'"]])
		(fpath ^. path, likePattern (SearchQuery input SearchPrefix))
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
runCommand (Lint fs lints) = toValue $ liftM concat $ forM fs $ \fsrc -> do
	FileSource f c <- actualFileContents fsrc
	liftIO $ hsdevLift $ HLint.hlint lints (view path f) c
runCommand (Check fs ghcs' clear) = toValue $ Log.scope "check" $
	liftM concat $ mapM (runCheck ghcs' clear) fs
runCommand (CheckLint fs ghcs' lints clear) = toValue $ do
	fs' <- mapM actualFileContents fs
	checkMsgs <- liftM concat $ mapM (runCheck ghcs' clear) fs'
	lintMsgs <- liftIO $ hsdevLift $ liftM concat $ mapM (\(FileSource f c) -> HLint.hlint lints (view path f) c) fs'
	return $ checkMsgs ++ lintMsgs
runCommand (Types fs ghcs' clear) = toValue $ do
	liftM concat $ forM fs $ \fsrc@(FileSource file msrc) -> do
		mcached' <- getCached file msrc
		FileSource _ msrc' <- actualFileContents fsrc
		maybe (updateTypes file msrc') return mcached'
	where
		getCached :: ServerMonadBase m => Path -> Maybe Text -> ClientM m (Maybe [Tools.Note Types.TypedExpr])
		getCached _ (Just _) = return Nothing
		getCached file' Nothing = do
			actual' <- sourceUpToDate file'
			mid <- query @_ @((Bool, Int) :. ModuleId)
				(toQuery $ mconcat [
					select_ ["json_extract(tags, '$.types') is 1", "mu.id"],
					qModuleId,
					where_ ["mu.file = ?"]])
				(Only file')
			when (length mid > 1) $ Log.sendLog Log.Warning $ "multiple modules with same file = {}" ~~ file'
			when (null mid) $ hsdevError $ NotInspected $ FileModule file' Nothing
			let
				[(hasTypes', mid') :. modId] = mid
			if actual' && hasTypes'
				then do
					types' <- query @_ @(Region :. Types.TypedExpr) "select line, column, line_to, column_to, expr, type from types where module_id = ?;" (Only mid')
					liftM Just $ forM types' $ \(rgn :. texpr) -> return Tools.Note {
						Tools._noteSource = modId ^. moduleLocation,
						Tools._noteRegion = rgn,
						Tools._noteLevel = Nothing,
						Tools._note = set Types.typedExpr Nothing texpr }
				else return Nothing

		updateTypes file msrc = do
			sess <- getSession
			m <- setFileSourceSession ghcs' file
			types' <- inSessionGhc $ do
				when clear clearTargets
				Update.cacheGhcWarnings sess [m ^. moduleId . moduleLocation] $
					Types.fileTypes m msrc
			updateProcess def [Update.setModTypes (m ^. moduleId) types']
			return $ set (each . Tools.note . Types.typedExpr) Nothing types'
runCommand (AutoFix ns) = toValue $ return $ AutoFix.corrections ns
runCommand (Refactor ns rest isPure) = toValue $ do
	files <- liftM (ordNub . sort) $ mapM findPath $ mapMaybe (preview $ Tools.noteSource . moduleFile) ns
	let
		runFix file = do
			unless isPure $ do
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
	mfile' <- traverse actualFileContents mfile
	case mfile' of
		Nothing -> inSessionGhc ghciSession
		Just (FileSource f mcts) -> do
			m <- setFileSourceSession [] f
			inSessionGhc $ interpretModule m mcts
	inSessionGhc $ mapM (tryRepl . evaluate) exprs
runCommand (GhcType exprs mfile) = toValue $ do
	mfile' <- traverse actualFileContents mfile
	case mfile' of
		Nothing -> inSessionGhc ghciSession
		Just (FileSource f mcts) -> do
			m <- setFileSourceSession [] f
			inSessionGhc $ interpretModule m mcts
	inSessionGhc $ mapM (tryRepl . expressionType) exprs
runCommand Langs = toValue $ return Compat.languages
runCommand Flags = toValue $ return ["-f" ++ prefix ++ f |
	f <- Compat.flags,
	prefix <- ["", "no-"]]
runCommand (Link hold) = toValue $ commandLink >> when hold commandHold
runCommand StopGhc = toValue $ do
	inSessionGhc $ do
		ms <- findSessionBy (const True)
		forM_ ms $ \s -> do
			Log.sendLog Log.Trace $ "stopping session: {}" ~~ view sessionKey s
			deleteSession $ view sessionKey s
runCommand Exit = toValue serverExit


targetFilter :: Text -> Maybe Text -> TargetFilter -> (Text, [NamedParam])
targetFilter mtable _ (TargetProject proj) = (
	"{t}.cabal in (select cabal from projects where name == :project or cabal == :project)" ~~ ("t" ~% mtable),
	[":project" := proj])
targetFilter mtable _ (TargetFile f) = ("{t}.file == :file" ~~ ("t" ~% mtable), [":file" := f])
targetFilter mtable Nothing (TargetModule nm) = ("{t}.name == :module_name" ~~ ("t" ~% mtable), [":module_name" := nm])
targetFilter mtable (Just stable) (TargetModule nm) = (
	"({t}.name == :module_name) or ({s}.id in (select e.symbol_id from exports as e, modules as em where e.module_id == em.id and em.name == :module_name))"
		~~ ("t" ~% mtable)
		~~ ("s" ~% stable),
	[":module_name" := nm])
targetFilter mtable _ (TargetPackage p) = (tpl ~~ ("t" ~% mtable), params) where
	pkg = fromMaybe (mkPackage p) (readMaybe (unpack p))
	tpl
		| T.null (pkg ^. packageVersion) = "{t}.package_name == :package_name"
		| otherwise = "{t}.package_name == :package_name and {t}.package_version == :package_version"
	params
		| T.null (pkg ^. packageVersion) = [pname]
		| otherwise = [pname, pver]
	pname = ":package_name" := (pkg ^. packageName)
	pver = ":package_version" := (pkg ^. packageVersion)
targetFilter mtable _ TargetInstalled = ("{t}.package_name is not null" ~~ ("t" ~% mtable), [])
targetFilter mtable _ TargetSourced = ("{t}.file is not null" ~~ ("t" ~% mtable), [])
targetFilter mtable _ TargetStandalone = ("{t}.file is not null and {t}.cabal is null" ~~ ("t" ~% mtable), [])

targetFilters :: Text -> Maybe Text -> [TargetFilter] -> ([Text], [NamedParam])
targetFilters mtable stable = second concat . unzip . map (targetFilter mtable stable)

likePattern :: SearchQuery -> Text
likePattern (SearchQuery input stype) = case stype of
	SearchExact -> escapedInput
	SearchPrefix -> escapedInput `T.append` "%"
	SearchInfix -> "%" `T.append` escapedInput `T.append` "%"
	SearchSuffix -> "%" `T.append` escapedInput
	where
		escapedInput = escapeLike input

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


-- | Run check
runCheck :: CommandMonad m => [String] -> Bool -> FileSource -> m [Tools.Note Tools.OutputMessage]
runCheck ghcs' clear = actualFileContents >=> check' where
	check' (FileSource file mcts) = Log.scope "run-check" $ do
		Log.sendLog Log.Trace $ "setting file source session for {}" ~~ file
		sess <- getSession
		m <- setFileSourceSession ghcs' file
		Log.sendLog Log.Trace "file source session set"
		ns <- inSessionGhc $ do
			when clear clearTargets
			Update.cacheGhcWarnings sess [m ^. moduleId . moduleLocation] $
				Check.check m mcts
		if null ns
			then do
				ns' <- Update.cachedWarnings [m ^. moduleId . moduleLocation]
				unless (null ns') $
					Log.sendLog Log.Trace $ "returning {} cached warnings for {}" ~~ length ns' ~~ file
				return ns'
			else return ns

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

-- | Check if source file up to date
sourceUpToDate :: CommandMonad m => Path -> m Bool
sourceUpToDate fpath = do
	fpath' <- findPath fpath
	insps <- query @_ @Inspection "select inspection_time, inspection_opts from modules where file = ?;" (Only fpath')
	when (length insps > 1) $ Log.sendLog Log.Warning $ "multiple modules with same file = {}" ~~ fpath'
	maybe
		(return False)
		(upToDate (FileModule fpath' Nothing) [])
		(listToMaybe insps)

-- | Get source file
-- refineSourceFile :: CommandMonad m => Path -> m Path
-- refineSourceFile fpath = do
-- 	fpath' <- findPath fpath
-- 	fs <- liftM (map fromOnly) $ query "select file from modules where file == ?;" (Only fpath')
-- 	case fs of
-- 		[] -> hsdevError (NotInspected $ FileModule fpath' Nothing)
-- 		(f:_) -> do
-- 			when (length fs > 1) $ Log.sendLog Log.Warning $ "multiple modules with same file = {}" ~~ fpath'
-- 			return f

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
				Nothing -> do
					[insp] <- query @_ @Inspection "select inspection_time, inspection_opts from modules where id = ?;" (Only i)
					fresh' <- upToDate (m ^. moduleId . moduleLocation) [] insp
					if fresh'
						then return m
						else do
							defs <- askSession sessionDefines
							mcts <- fmap (fmap snd) $ getFileContents fpath'
							ip' <- runInspect (m ^. moduleId . moduleLocation) $ preload (m ^. moduleId . moduleName) defs [] mcts
							case ip' ^? inspected of
								Just p' -> return $ set moduleImports (p' ^. asModule . moduleImports) m
								Nothing -> return m
				Just cabal' -> do
					proj' <- SQLite.loadProject cabal'
					return $ set (moduleId . moduleLocation . moduleProject) (Just proj') m

-- | Get file contents
actualFileContents :: CommandMonad m => FileSource -> m FileSource
actualFileContents (FileSource fpath Nothing) = fmap (FileSource fpath . fmap snd) (getFileContents fpath)
actualFileContents fcts = return fcts

-- | Set session by source
setFileSourceSession :: CommandMonad m => [String] -> Path -> m Module
setFileSourceSession opts fpath = do
	m <- refineSourceModule fpath
	inSessionGhc $ targetSession opts m
	return m

-- | Ensure package exists
-- refinePackage :: CommandMonad m => Text -> m Text
-- refinePackage pkg = do
-- 	[(Only exists)] <- query "select count(*) > 0 from package_dbs where package_name == ?;" (Only pkg)
-- 	when (not exists) $ hsdevError (PackageNotFound pkg)
-- 	return pkg

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
updateProcess :: ServerMonadBase m => Update.UpdateOptions -> [Update.UpdateM IO ()] -> ClientM m ()
updateProcess uopts acts = hoist liftIO $ do
	copts <- getOptions
	inSessionUpdater $ hoist (flip runReaderT copts) $ runClientM $ mapM_ (Update.runUpdate uopts . runAct) acts
	where
		runAct act = catch act onError
		onError e = Log.sendLog Log.Error $ "{}" ~~ (e :: HsDevError)
