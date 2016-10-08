{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

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
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Data.String (fromString)
import Data.Text (unpack)
import qualified Data.Text as T (isInfixOf, isPrefixOf, isSuffixOf)
import System.Directory
import System.FilePath
import qualified System.Log.Simple as Log
import Text.Regex.PCRE ((=~))

import qualified System.Directory.Watcher as W
import qualified Data.Async as A
import System.Directory.Paths
import Text.Format
import HsDev.Cache
import HsDev.Commands
import HsDev.Error
import qualified HsDev.Database.Async as DB
import HsDev.Server.Message as M
import HsDev.Server.Types
import HsDev.Sandbox hiding (findSandbox)
import qualified HsDev.Sandbox as S (findSandbox)
import HsDev.Symbols
import HsDev.Symbols.Util
import qualified HsDev.Tools.AutoFix as AutoFix
import qualified HsDev.Tools.Cabal as Cabal
import HsDev.Tools.Ghc.Session
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
runCommand (Listen (Just r)) = bracket (serverSetLogRules ["/: {}" ~~ r]) serverSetLogRules $ \_ -> runCommand (Listen Nothing)
runCommand (Listen Nothing) = toValue $ do
	serverListen >>= mapM_ (\msg -> commandNotify (Notification $ object ["message" .= msg]))
runCommand (SetLogConfig rs) = toValue $ do
	rules' <- serverSetLogRules rs
	Log.log Log.Debug $ "log rules switched from '{}' to '{}'" ~~ intercalate ", " rules' ~~ intercalate ", " rs
	Log.log Log.Info $ "log rules updated to: {}" ~~ intercalate ", " rs
runCommand (AddData cts) = toValue $ mapM_ updateData cts where
	updateData (AddedDatabase db) = toValue $ serverUpdateDB db
	updateData (AddedModule m) = toValue $ serverUpdateDB $ fromModule m
	updateData (AddedProject p) = toValue $ serverUpdateDB $ fromProject p
runCommand (Scan projs cabal sboxes fs paths' ghcs' docs' infer') = toValue $ do
	sboxes' <- getSandboxes sboxes
	updateProcess (Update.UpdateOptions [] ghcs' docs' infer') $ concat [
		[Update.scanCabal ghcs' | cabal],
		map (Update.scanSandbox ghcs') sboxes',
		map (\(FileSource f mcts) -> Update.scanFileContents ghcs' f mcts) fs,
		map (Update.scanProject ghcs') projs,
		map (Update.scanDirectory ghcs') paths']
runCommand (RefineDocs projs fs ms) = toValue $ do
	projects <- traverse findProject projs
	dbval <- getDb
	let
		filters = anyOf $ concat [
			map inProject projects,
			map inFile fs,
			map inModule ms]
		mods = dbval ^.. databaseModules . each . filtered (maybe False filters . preview inspected)
	updateProcess (Update.UpdateOptions [] [] False False) [Update.scanDocs mods]
runCommand (InferTypes projs fs ms) = toValue $ do
	projects <- traverse findProject projs
	dbval <- getDb
	let
		filters = anyOf $ concat [
			map inProject projects,
			map inFile fs,
			map inModule ms]
		mods = dbval ^.. databaseModules . each . filtered (maybe False filters . preview inspected)
	updateProcess (Update.UpdateOptions [] [] False False) [Update.inferModTypes mods]
runCommand (Remove projs cabal sboxes files) = toValue $ do
	db <- askSession sessionDatabase
	dbval <- getDb
	w <- askSession sessionWatcher
	projects <- traverse findProject projs
	sboxes' <- getSandboxes sboxes
	forM_ projects $ \proj -> do
		DB.clear db (return $ dbval ^. projectSlice proj)
		liftIO $ unwatchProject w proj
	dbPDbs <- mapM restorePackageDbStack $ databasePackageDbs dbval
	flip State.evalStateT dbPDbs $ do
		when cabal $ removePackageDbStack userDb
		forM_ sboxes' $ \sbox -> do
			pdbs <- lift $ sandboxPackageDbStack sbox
			removePackageDbStack pdbs
	forM_ files $ \file -> do
		DB.clear db (return $ dbval ^. slice (inFile file))
		let
			mloc = fmap (view (moduleId . moduleLocation)) (dbval ^? modules . filtered (inFile file))
		maybe (return ()) (liftIO . unwatchModule w) mloc
	where
		-- We can safely remove package-db from db iff doesn't used by some of other package-dbs
		-- For example, we can't remove global-db if there are any other package-dbs, because all of them uses global-db
		-- We also can't remove stack snapshot package-db if there are some local package-db not yet removed
		canRemove pdbs = do
			from' <- State.get
			return $ null $ filter (pdbs `isSubStack`) $ delete pdbs from'
		-- Remove top of package-db stack if possible
		removePackageDb pdbs = do
			db <- lift $ askSession sessionDatabase
			dbval <- lift getDb
			w <- lift $ askSession sessionWatcher
			can <- canRemove pdbs
			when can $ do
				State.modify (delete pdbs)
				DB.clear db (return $ dbval ^. packageDbSlice (topPackageDb pdbs))
				liftIO $ unwatchPackageDb w $ topPackageDb pdbs
		-- Remove package-db stack when possible
		removePackageDbStack = mapM_ removePackageDb . packageDbStacks
runCommand RemoveAll = toValue $ do
	db <- askSession sessionDatabase
	liftIO $ A.modifyAsync db A.Clear
	w <- askSession sessionWatcher
	wdirs <- liftIO $ readMVar (W.watcherDirs w)
	liftIO $ forM_ (M.toList wdirs) $ \(dir, (isTree, _)) -> (if isTree then W.unwatchTree else W.unwatchDir) w dir
runCommand InfoPackages = toValue $ do
	dbval <- getDb
	return $ ordNub (dbval ^.. packages)
runCommand InfoProjects = toValue $ (toListOf $ databaseProjects . each) <$> getDb
runCommand InfoSandboxes = toValue $ databasePackageDbs <$> getDb
runCommand (InfoSymbol sq fs _) = toValue $ do
	dbval <- getDb
	filter' <- targetFilters fs
	return (dbval ^.. newestPackagesSlice . modules . filtered filter' . moduleSymbols . filtered (matchQuery sq))
runCommand (InfoModule sq fs h) = toValue $ do
	dbval <- getDb
	filter' <- targetFilters fs
	let
		onlyHeaders
			| h = toJSON . over each (view moduleId)
			| otherwise = toJSON
	return $ onlyHeaders (dbval ^.. newestPackagesSlice . modules . filtered filter' . filtered (matchQuery sq))
runCommand (InfoProject (Left projName)) = toValue $ findProject projName
runCommand (InfoProject (Right projPath)) = toValue $ liftIO $ searchProject projPath
runCommand (InfoSandbox sandbox') = toValue $ liftIO $ searchSandbox sandbox'
runCommand (Lookup nm fpath) = toValue $ do
	dbval <- getDb
	liftIO $ hsdevLift $ lookupSymbol dbval fpath nm
runCommand (Whois nm fpath) = toValue $ do
	dbval <- getDb
	liftIO $ hsdevLift $ whois dbval fpath nm
runCommand (ResolveScopeModules sq fpath) = toValue $ do
	dbval <- getDb
	ms <- liftIO $ hsdevLift $ scopeModules dbval fpath
	return (ms ^.. each . filtered (matchQuery sq))
runCommand (ResolveScope sq fpath) = toValue $ do
	dbval <- getDb
	ss <- liftIO $ hsdevLift $ scope dbval fpath
	return (ss ^.. each . filtered (matchQuery sq))
runCommand (Complete input True fpath) = toValue $ do
	dbval <- getDb
	liftIO $ hsdevLift $ wideCompletions dbval fpath input
runCommand (Complete input False fpath) = toValue $ do
	dbval <- getDb
	liftIO $ hsdevLift $ completions dbval fpath input
runCommand (Hayoo hq p ps) = toValue $ liftM concat $ forM [p .. p + pred ps] $ \i -> liftM
	(mapMaybe Hayoo.hayooAsSymbol . Hayoo.resultResult) $
	liftIO $ hsdevLift $ Hayoo.hayoo hq (Just i)
runCommand (CabalList packages') = toValue $ liftIO $ hsdevLift $ Cabal.cabalList packages'
runCommand (Lint fs) = toValue $ do
	liftIO $ hsdevLift $ liftM concat $ mapM (\(FileSource f c) -> HLint.hlint f c) fs
runCommand (Check fs ghcs') = toValue $ Log.scope "check" $ do
	ensureUpToDate (Update.UpdateOptions [] ghcs' False False) fs
	let
		checkSome file fn = Log.scope "checkSome" $ do
			m <- setFileSourceSession ghcs' file
			inSessionGhc $ fn m
	liftM concat $ mapM (\(FileSource f c) -> checkSome f (\m -> Check.check ghcs' m c)) fs
runCommand (CheckLint fs ghcs') = toValue $ do
	ensureUpToDate (Update.UpdateOptions [] ghcs' False False) fs
	let
		checkSome file fn = do
			m <- setFileSourceSession ghcs' file
			inSessionGhc $ fn m
	checkMsgs <- liftM concat $ mapM (\(FileSource f c) -> checkSome f (\m -> Check.check ghcs' m c)) fs
	lintMsgs <- liftIO $ hsdevLift $ liftM concat $ mapM (\(FileSource f c) -> HLint.hlint f c) fs
	return $ checkMsgs ++ lintMsgs
runCommand (Types fs ghcs') = toValue $ do
	ensureUpToDate (Update.UpdateOptions [] ghcs' False False) fs
	liftM concat $ forM fs $ \(FileSource file msrc) -> do
		m <- setFileSourceSession ghcs' file
		inSessionGhc $ Types.fileTypes ghcs' m msrc
runCommand (AutoFix (AutoFixShow ns)) = toValue $ return $ AutoFix.corrections ns
runCommand (AutoFix (AutoFixFix ns rest isPure)) = toValue $ do
	files <- liftM (ordNub . sort) $ mapM findPath $ mapMaybe (preview $ Tools.noteSource . moduleFile) ns
	let
		doFix :: FilePath -> Maybe String -> ([Tools.Note AutoFix.Correction], Maybe String)
		doFix file mcts = AutoFix.autoFix fCorrs (fUpCorrs, mcts) where
			findCorrs :: FilePath -> [Tools.Note AutoFix.Correction] -> [Tools.Note AutoFix.Correction]
			findCorrs f = filter ((== Just f) . preview (Tools.noteSource . moduleFile))
			fCorrs = findCorrs file ns
			fUpCorrs = findCorrs file rest
		runFix file
			| isPure = return $ fst $ doFix file Nothing
			| otherwise = do
				(corrs', Just cts') <- liftM (doFix file) $ liftIO $ Just <$> readFileUtf8 file
				liftIO $ writeFileUtf8 file cts'
				return corrs'
	liftM concat $ mapM runFix files
runCommand (GhcEval exprs mfile) = toValue $ do
	ensureUpToDate (Update.UpdateOptions [] [] False False) (maybeToList mfile)
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

targetFilters :: (CommandMonad m, Sourced a) => [TargetFilter] -> m (a -> Bool)
targetFilters fs = do
	fs_ <- mapM targetFilter fs
	return $ foldr (liftM2 (&&)) (const True) fs_

targetFilter :: (CommandMonad m, Sourced a) => TargetFilter -> m (a -> Bool)
targetFilter f = liftM (. view sourcedModule) $ case f of
	TargetProject proj -> liftM inProject $ findProject proj
	TargetFile file -> liftM inFile $ refineSourceFile file
	TargetModule mname -> return $ inModule mname
	TargetDepsOf dep -> liftM inDeps $ findDep dep
	TargetPackageDb pdb -> return $ inPackageDb pdb
	TargetCabal -> return $ inPackageDbStack userDb
	TargetSandbox sbox -> liftM inPackageDbStack $ findSandbox sbox >>= sandboxPackageDbStack
	TargetPackage pack -> liftM (inPackage . mkPackage) $ refinePackage pack
	TargetSourced -> return byFile
	TargetStandalone -> return standalone

-- Helper functions

-- | Canonicalize paths
findPath :: (CommandMonad m, Paths a) => a -> m a
findPath = paths findPath' where
	findPath' :: CommandMonad m => FilePath -> m FilePath
	findPath' f = do
		r <- commandRoot
		liftIO $ canonicalizePath (normalise $ if isRelative f then r </> f else f)

-- | Find sandbox by path
findSandbox :: CommandMonad m => FilePath -> m Sandbox
findSandbox fpath = do
	fpath' <- findPath fpath
	sbox <- liftIO $ S.findSandbox fpath'
	maybe (hsdevError $ FileNotFound fpath') return sbox

-- | Get source file
refineSourceFile :: CommandMonad m => FilePath -> m FilePath
refineSourceFile fpath = do
	fpath' <- findPath fpath
	db' <- getDb
	maybe (hsdevError (NotInspected $ FileModule fpath' Nothing)) return $ do
		m' <- db' ^? modules . filtered (inFile fpath')
		preview (moduleId . moduleLocation . moduleFile) m'

-- | Get module by source
refineSourceModule :: CommandMonad m => FilePath -> m Module
refineSourceModule fpath = do
	fpath' <- findPath fpath
	db' <- getDb
	maybe (hsdevError (NotInspected $ FileModule fpath' Nothing)) return (db' ^? modules . filtered (inFile fpath'))

-- | Set session by source
setFileSourceSession :: CommandMonad m => [String] -> FilePath -> m Module
setFileSourceSession opts fpath = do
	m <- refineSourceModule fpath
	inSessionGhc $ targetSession opts m
	return m

-- | Ensure package exists
refinePackage :: CommandMonad m => String -> m String
refinePackage pack = do
	db' <- getDb
	if pack `elem` ordNub (db' ^.. packages . packageName)
		then return pack
		else hsdevError (PackageNotFound pack)

-- | Get list of enumerated sandboxes
getSandboxes :: CommandMonad m => [FilePath] -> m [Sandbox]
getSandboxes = traverse (findPath >=> findSandbox)

-- | Find project by name or path
findProject :: CommandMonad m => String -> m Project
findProject proj = do
	db' <- getDb
	proj' <- liftM addCabal $ findPath proj
	let
		resultProj =
			refineProject db' (project proj') <|>
			find ((== proj) . view projectName) (M.elems $ view databaseProjects db')
	maybe (hsdevError $ ProjectNotFound proj) return resultProj
	where
		addCabal p
			| takeExtension p == ".cabal" = p
			| otherwise = p </> (takeBaseName p <.> "cabal")

-- | Find dependency: it may be source, project file or project name, also returns sandbox found
findDep :: CommandMonad m => String -> m (Project, Maybe FilePath, PackageDbStack)
findDep depName = do
	depPath <- findPath depName
	proj <- msum [
		do
			p <- liftIO (locateProject depPath)
			p' <- maybe (hsdevError $ ProjectNotFound depName) return p
			liftIO $ loadProject p',
		findProject depName]
	let
		src
			| takeExtension depPath == ".hs" = Just depPath
			| otherwise = Nothing
	pdbs <- searchPackageDbStack $ view projectPath proj
	return (proj, src, pdbs)

-- FIXME: Doesn't work for file without project
-- | Check if project or source depends from this module
inDeps :: (Project, Maybe FilePath, PackageDbStack) -> ModuleId -> Bool
inDeps (proj, src, pdbs) = liftM2 (&&) (inPackageDbStack pdbs) deps' where
	deps' = case src of
		Nothing -> inDepsOfProject proj
		Just src' -> inDepsOfFile proj src'

-- | Get actual DB state
getDb :: SessionMonad m => m Database
getDb = askSession sessionDatabase >>= liftIO . DB.readAsync

-- | Run DB update action
updateProcess :: ServerMonadBase m => Update.UpdateOptions -> [Update.UpdateM m ()] -> ClientM m ()
updateProcess uopts acts = Update.runUpdate uopts $ mapM_ runAct acts where
	runAct act = catch act onError
	onError e = Log.log Log.Error $ "{}" ~~ (e :: HsDevError)

-- | Ensure file is up to date
ensureUpToDate :: ServerMonadBase m => Update.UpdateOptions -> [FileSource] -> ClientM m ()
ensureUpToDate uopts fs = updateProcess uopts [Update.scanFileContents (view Update.updateGhcOpts uopts) f mcts | FileSource f mcts <- fs]

-- | Check matching search query
matchQuery :: Sourced a => SearchQuery -> a -> Bool
matchQuery (SearchQuery q st) s = case st of
	SearchExact -> sq == sn
	SearchPrefix -> sq `T.isPrefixOf` sn
	SearchInfix -> sq `T.isInfixOf` sn
	SearchSuffix -> sq `T.isSuffixOf` sn
	SearchRegex -> unpack sn =~ q
	where
		sq = fromString q
		sn = view sourcedName s
