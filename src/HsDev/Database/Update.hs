{-# LANGUAGE FlexibleContexts, OverloadedStrings, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Database.Update (
	Status(..), Progress(..), Task(..), isStatus,
	Settings(..), settings,

	UpdateDB,
	updateDB,

	postStatus, waiter, updater, loadCache, getCache, runTask, runTasks,
	readDB,

	scanModule, scanModules, scanFile, scanFileContents, scanCabal, scanProjectFile, scanProject, scanDirectory,
	scanDocs, inferModTypes,
	scan,
	updateEvent, processEvent,

	-- * Helpers
	liftExceptT,

	module HsDev.Watcher,

	module Control.Monad.Except
	) where

import Control.Concurrent.Lifted (fork)
import Control.DeepSeq
import Control.Lens (preview, _Just, view, _1, mapMOf_, each, (^..))
import Control.Monad.Catch
import Control.Monad.CatchIO
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Trans.Control
import Data.Aeson
import Data.Aeson.Types
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Maybe (mapMaybe, isJust, fromMaybe)
import qualified Data.Text as T (unpack)
import System.Directory (canonicalizePath, doesFileExist)
import qualified System.Log.Simple as Log
import qualified System.Log.Simple.Base as Log (scopeLog)

import Control.Concurrent.Worker (inWorker)
import qualified HsDev.Cache.Structured as Cache
import HsDev.Database
import HsDev.Database.Async hiding (Event)
import HsDev.Display
import HsDev.Inspect (inspectDocs, inspectDocsGhc, getDefines)
import HsDev.Project
import HsDev.Symbols
import HsDev.Tools.Ghc.Worker (ghcWorker)
import HsDev.Tools.Ghc.Types (inferTypes)
import HsDev.Tools.HDocs
import qualified HsDev.Tools.GhcMod as GhcMod
import qualified HsDev.Scan as S
import HsDev.Scan.Browse
import HsDev.Util (liftEIO, isParent, ordNub)
import HsDev.Database.Update.Types
import HsDev.Watcher
import Text.Format

isStatus :: Value -> Bool
isStatus = isJust . parseMaybe (parseJSON :: Value -> Parser Task)

-- | Run `UpdateDB` monad
updateDB :: (MonadBaseControl IO m, MonadCatchIO m) => Settings -> ExceptT String (UpdateDB m) () -> m ()
updateDB sets act = Log.scopeLog (settingsLogger sets) "update" $ do
	updatedMods <- execWriterT (runUpdateDB (runExceptT act' >> return ()) `runReaderT` sets)
	wait $ database sets
	dbval <- liftIO $ readAsync $ database sets
	let
		cabals = ordNub $ mapMaybe (preview moduleCabal) updatedMods
		projs = ordNub $ mapMaybe (preview $ moduleProject . _Just) updatedMods
		stand = any moduleStandalone updatedMods

		modifiedDb = mconcat $ concat [
			map (`cabalDB` dbval) cabals,
			map (`projectDB` dbval) projs,
			[standaloneDB dbval | stand]]
	liftIO $ databaseCacheWriter sets modifiedDb
	where
		act' = do
			mlocs' <- liftM (filter (isJust . preview moduleFile) . snd) $ listen act
			wait $ database sets
			let
				getMods :: (MonadIO m) => m [InspectedModule]
				getMods = do
					db' <- liftIO $ readAsync $ database sets
					return $ filter ((`elem` mlocs') . view inspectedId) $ toList $ databaseModules db'
			when (updateDocs sets) $ do
				Log.log Log.Trace "forking inspecting source docs"
				void $ fork (getMods >>= waiter . mapM_ scanDocs_)
			when (runInferTypes sets) $ do
				Log.log Log.Trace "forking inferring types"
				void $ fork (getMods >>= waiter . mapM_ inferModTypes_)
		scanDocs_ :: (MonadIO m, MonadReader Settings m, MonadWriter [ModuleLocation] m) => InspectedModule -> ExceptT String m ()
		scanDocs_ im = do
			im' <- liftExceptT $ S.scanModify (\opts _ -> inspectDocs opts) im
			updater $ return $ fromModule im'
		inferModTypes_ :: (MonadIO m, MonadReader Settings m, MonadWriter [ModuleLocation] m) => InspectedModule -> ExceptT String m ()
		inferModTypes_ im = do
			-- TODO: locate sandbox
			im' <- liftExceptT $ S.scanModify infer' im
			updater $ return $ fromModule im'
		infer' :: [String] -> Cabal -> Module -> ExceptT String IO Module
		infer' opts cabal m = case preview (moduleLocation . moduleFile) m of
			Nothing -> return m
			Just _ -> inWorkerT (settingsGhcWorker sets) $ inferTypes opts cabal m Nothing
		inWorkerT w = ExceptT . inWorker w . runExceptT 

-- | Post status
postStatus :: (MonadIO m, MonadReader Settings m) => Task -> m ()
postStatus s = do
	on' <- asks onStatus
	liftIO $ on' [s]

-- | Wait DB to complete actions
waiter :: (MonadIO m, MonadReader Settings m) => m () -> m ()
waiter act = do
	db <- asks database
	act
	wait db

-- | Update task result to database
updater :: (MonadIO m, MonadReader Settings m, MonadWriter [ModuleLocation] m) => m Database -> m ()
updater act = do
	db <- asks database
	db' <- act
	update db $ return $!! db'
	tell $!! map (view moduleLocation) $ allModules db'

-- | Clear obsolete data from database
cleaner :: (MonadIO m, MonadReader Settings m, MonadWriter [ModuleLocation] m) => m Database -> m ()
cleaner act = do
	db <- asks database
	db' <- act
	clear db $ return $!! db'

-- | Get data from cache without updating DB
loadCache :: (MonadIO m, MonadReader Settings m, MonadWriter [ModuleLocation] m) => (FilePath -> ExceptT String IO Structured) -> m Database
loadCache act = do
	cacheReader <- asks databaseCacheReader
	mdat <- liftIO $ cacheReader act
	return $ fromMaybe mempty mdat

-- | Load data from cache if not loaded yet and wait
getCache :: (MonadIO m, MonadReader Settings m, MonadWriter [ModuleLocation] m) => (FilePath -> ExceptT String IO Structured) -> (Database -> Database) -> m Database
getCache act check = do
	dbval <- liftM check readDB
	if nullDatabase dbval
		then do
			db <- loadCache act
			waiter $ updater $ return db
			return db
		else
			return dbval

-- | Run one task
runTask :: (Display t, MonadIO m, NFData a, MonadCatchIO m) => String -> t -> ExceptT String (UpdateDB m) a -> ExceptT String (UpdateDB m) a
runTask action subj act = Log.scope "task" $ do
	postStatus $ task { taskStatus = StatusWorking }
	x <- local childTask act
	x `deepseq` postStatus (task { taskStatus = StatusOk })
	return x
	`catchError`
	(\e -> postStatus (task { taskStatus = StatusError e }) >> throwError e)
	where
		task = Task {
			taskName = action,
			taskStatus = StatusWorking,
			taskSubjectType = displayType subj,
			taskSubjectName = display subj,
			taskProgress = Nothing }
		childTask st = st {
			onStatus = \t -> onStatus st (task : t) }

-- | Run many tasks with numeration
runTasks :: Monad m => [ExceptT String (UpdateDB m) ()] -> ExceptT String (UpdateDB m) ()
runTasks ts = zipWithM_ taskNum [1..] (map noErr ts) where
	total = length ts
	taskNum n = local setProgress where
		setProgress st = st {
			onStatus = \(t:tl) -> onStatus st ((t { taskProgress = Just (Progress n total) }) : tl) }
	noErr v = v `mplus` return ()

-- | Get database value
readDB :: (MonadIO m, MonadReader Settings m) => m Database
readDB = asks database >>= liftIO . readAsync

-- | Scan module
scanModule :: (MonadIO m, MonadCatch m, MonadCatchIO m) => [String] -> ModuleLocation -> Maybe String -> ExceptT String (UpdateDB m) ()
scanModule opts mloc mcts = runTask "scanning" mloc $ Log.scope "module" $ do
	defs <- asks settingsDefines
	im <- liftExceptT $ S.scanModule defs opts mloc mcts
	updater $ return $ fromModule im
	_ <- ExceptT $ return $ view inspectionResult im
	return ()

-- | Scan modules
scanModules :: (MonadIO m, MonadCatch m, MonadCatchIO m) => [String] -> [S.ModuleToScan] -> ExceptT String (UpdateDB m) ()
scanModules opts ms = runTasks $
	[scanProjectFile opts p >> return () | p <- ps] ++
	[scanModule (opts ++ mopts) m mcts | (m, mopts, mcts) <- ms]
	where
		ps = ordNub $ mapMaybe (toProj . view _1) ms
		toProj (FileModule _ p) = fmap (view projectCabal) p
		toProj _ = Nothing

-- | Scan source file
scanFile :: (MonadIO m, MonadCatch m, MonadCatchIO m) => [String] -> FilePath -> ExceptT String (UpdateDB m) ()
scanFile opts fpath = scanFileContents opts fpath Nothing

-- | Scan source file with contents
scanFileContents :: (MonadIO m, MonadCatch m, MonadCatchIO m) => [String] -> FilePath -> Maybe String -> ExceptT String (UpdateDB m) ()
scanFileContents opts fpath mcts = Log.scope "file" $ do
	dbval <- readDB
	fpath' <- liftEIO $ canonicalizePath fpath
	ex <- liftEIO $ doesFileExist fpath'
	mlocs <- if ex
		then do
			mloc <- case lookupFile fpath' dbval of
				Just m -> return $ view moduleLocation m
				Nothing -> do
					mproj <- liftEIO $ locateProject fpath'
					return $ FileModule fpath' mproj
			return [(mloc, [], mcts)]
		else return []
	mapMOf_ (each . _1) (watch . flip watchModule) mlocs
	scan
		(Cache.loadFiles (== fpath'))
		(filterDB (inFile fpath') (const False) . standaloneDB)
		mlocs
		opts
		(scanModules opts)
	where
		inFile f = maybe False (== f) . preview (moduleIdLocation . moduleFile)

-- | Scan cabal modules
scanCabal :: (MonadIO m, MonadCatch m, MonadCatchIO m) => [String] -> Cabal -> ExceptT String (UpdateDB m) ()
scanCabal opts cabalSandbox = runTask "scanning" cabalSandbox $ Log.scope "cabal" $ do
	watch (\w -> watchSandbox w cabalSandbox opts)
	mlocs <- liftExceptT $ listModules opts cabalSandbox
	scan (Cache.loadCabal cabalSandbox) (cabalDB cabalSandbox) ((,,) <$> mlocs <*> pure [] <*> pure Nothing) opts $ \mlocs' -> do
		ms <- liftExceptT $ browseModules opts cabalSandbox (mlocs' ^.. each . _1)
		docs <- liftExceptT $ hdocsCabal cabalSandbox opts
		updater $ return $ mconcat $ map (fromModule . fmap (setDocs' docs)) ms
	where
		setDocs' :: Map String (Map String String) -> Module -> Module
		setDocs' docs m = maybe m (`setDocs` m) $ M.lookup (T.unpack $ view moduleName m) docs

-- | Scan project file
scanProjectFile :: (MonadIO m, MonadCatch m, MonadCatchIO m) => [String] -> FilePath -> ExceptT String (UpdateDB m) Project
scanProjectFile opts cabal = runTask "scanning" cabal $ liftExceptT $ S.scanProjectFile opts cabal

-- | Scan project
scanProject :: (MonadIO m, MonadCatch m, MonadCatchIO m) => [String] -> FilePath -> ExceptT String (UpdateDB m) ()
scanProject opts cabal = runTask "scanning" (project cabal) $ Log.scope "project" $ do
	proj <- scanProjectFile opts cabal
	watch (\w -> watchProject w proj opts)
	(_, sources) <- liftExceptT $ S.enumProject proj
	scan (Cache.loadProject $ view projectCabal proj) (projectDB proj) sources opts $ \ms -> do
		scanModules opts ms
		updater $ return $ fromProject proj

-- | Scan directory for source files and projects
scanDirectory :: (MonadIO m, MonadCatch m, MonadCatchIO m) => [String] -> FilePath -> ExceptT String (UpdateDB m) ()
scanDirectory opts dir = runTask "scanning" dir $ Log.scope "directory" $ do
	S.ScanContents standSrcs projSrcs sboxes <- liftExceptT $ S.enumDirectory dir
	runTasks [scanProject opts (view projectCabal p) | (p, _) <- projSrcs]
	runTasks $ map (scanCabal opts) sboxes
	mapMOf_ (each . _1) (watch . flip watchModule) standSrcs
	scan (Cache.loadFiles (dir `isParent`)) (filterDB inDir (const False) . standaloneDB) standSrcs opts $ scanModules opts
	where
		inDir = maybe False (dir `isParent`) . preview (moduleIdLocation . moduleFile)

-- | Scan docs for inspected modules
scanDocs :: (MonadIO m, MonadCatchIO m) => [InspectedModule] -> ExceptT String (UpdateDB m) ()
scanDocs ims = do
	w <- liftIO $ ghcWorker ["-haddock"] (return ())
	runTasks $ map (scanDocs' w) ims
	where
		scanDocs' w im = runTask "scanning docs" (view inspectedId im) $ Log.scope "docs" $ do
			Log.log Log.Trace $ "Scanning docs for {}" ~~  view inspectedId im
			im' <- liftExceptT $ S.scanModify (\opts _ -> inWorkerT w . inspectDocsGhc opts) im
			Log.log Log.Trace $ "Docs for {} updated" ~~ view inspectedId im
			updater $ return $ fromModule im'
		inWorkerT w = ExceptT . inWorker w . runExceptT 

inferModTypes :: (MonadIO m, MonadCatchIO m) => [InspectedModule] -> ExceptT String (UpdateDB m) ()
inferModTypes = runTasks . map inferModTypes' where
	inferModTypes' im = runTask "inferring types" (view inspectedId im) $ Log.scope "docs" $ do
		w <- asks settingsGhcWorker
		Log.log Log.Trace $ "Inferring types for {}" ~~ view inspectedId im
		im' <- liftExceptT $ S.scanModify (\opts cabal m -> inWorkerT w (inferTypes opts cabal m Nothing)) im
		Log.log Log.Trace $ "Types for {} inferred" ~~ view inspectedId im
		updater $ return $ fromModule im'
	inWorkerT w = ExceptT . inWorker w . runExceptT

-- | Generic scan function. Reads cache only if data is not already loaded, removes obsolete modules and rescans changed modules.
scan :: (MonadIO m, MonadCatch m, MonadCatchIO m)
	=> (FilePath -> ExceptT String IO Structured)
	-- ^ Read data from cache
	-> (Database -> Database)
	-- ^ Get data from database
	-> [S.ModuleToScan]
	-- ^ Actual modules. Other modules will be removed from database
	-> [String]
	-- ^ Extra scan options
	-> ([S.ModuleToScan] -> ExceptT String (UpdateDB m) ())
	-- ^ Function to update changed modules
	-> ExceptT String (UpdateDB m) ()
scan cache' part' mlocs opts act = Log.scope "scan" $ do
	dbval <- getCache cache' part'
	let
		obsolete = filterDB (\m -> view moduleIdLocation m `notElem` (mlocs ^.. each . _1)) (const False) dbval
	changed <- liftExceptT $ S.changedModules dbval opts mlocs
	cleaner $ return obsolete
	act changed

updateEvent :: (MonadIO m, MonadCatch m, MonadCatchIO m) => Watched -> Event -> ExceptT String (UpdateDB m) ()
updateEvent (WatchedProject proj projOpts) e
	| isSource e = do
		Log.log Log.Info $ "File '{file}' in project {proj} changed"
			~~ ("file" %= view eventPath e)
			~~ ("proj" %= view projectName proj)
		dbval <- readDB
		let
			opts = fromMaybe [] $ do
				m <- lookupFile (view eventPath e) dbval
				preview (inspection . inspectionOpts) $ getInspected dbval m
		scanFile opts $ view eventPath e
	| isCabal e = do
		Log.log Log.Info $ "Project {proj} changed"
			~~ ("proj" %= view projectName proj)
		scanProject projOpts $ view projectCabal proj
	| otherwise = return ()
updateEvent (WatchedSandbox cabal cabalOpts) e
	| isConf e = do
		Log.log Log.Info $ "Sandbox {cabal} changed"
			~~ ("cabal" %= cabal)
		scanCabal cabalOpts cabal
	| otherwise = return ()
updateEvent WatchedModule e
	| isSource e = do
		Log.log Log.Info $ "Module {file} changed"
			~~ ("file" %= view eventPath e)
		dbval <- readDB
		let
			opts = fromMaybe [] $ do
				m <- lookupFile (view eventPath e) dbval
				preview (inspection . inspectionOpts) $ getInspected dbval m
		scanFile opts $ view eventPath e
	| otherwise = return ()

processEvent :: Settings -> Watched -> Event -> IO ()
processEvent s w e = updateDB s $ updateEvent w e

-- | Lift errors
liftExceptT :: MonadIO m => ExceptT String IO a -> ExceptT String m a
liftExceptT = mapExceptT liftIO

watch :: (MonadIO m, MonadReader Settings m) => (Watcher -> IO ()) -> m ()
watch f = do
	w <- asks settingsWatcher
	liftIO $ f w
