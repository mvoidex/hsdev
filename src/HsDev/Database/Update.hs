{-# LANGUAGE FlexibleContexts, OverloadedStrings, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Database.Update (
	Status(..), Progress(..), Task(..), isStatus,
	UpdateOptions(..),

	UpdateM(..),
	runUpdate,

	UpdateMonad,

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

import Control.Arrow
import Control.Concurrent.Lifted (fork)
import Control.DeepSeq
import Control.Lens (preview, _Just, view, over, set, _1, mapMOf_, each, (^..), _head)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Aeson
import Data.Aeson.Types
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Maybe (mapMaybe, isJust, fromMaybe)
import qualified Data.Text as T (unpack)
import System.Directory (canonicalizePath, doesFileExist)
import qualified System.Log.Simple as Log

import Control.Concurrent.Worker (inWorker)
import qualified HsDev.Cache.Structured as Cache
import HsDev.Database
import HsDev.Database.Async hiding (Event)
import HsDev.Display
import HsDev.Inspect (inspectDocs, inspectDocsGhc)
import HsDev.Project
import HsDev.Symbols
import HsDev.Tools.Ghc.Worker (ghcWorker)
import HsDev.Tools.Ghc.Types (inferTypes)
import HsDev.Tools.HDocs
import qualified HsDev.Scan as S
import HsDev.Scan.Browse
import HsDev.Util (liftE, isParent, ordNub)
import HsDev.Server.Types (commandNotify, serverWriteCache, serverReadCache, CommandError(..), commandError_)
import HsDev.Server.Message
import HsDev.Database.Update.Types
import HsDev.Watcher
import Text.Format

onStatus :: UpdateMonad m => m ()
onStatus = asks (view updateTasks) >>= commandNotify . Notification . toJSON . reverse

childTask :: UpdateMonad m => Task -> m a -> m a
childTask t = local (over updateTasks (t:))

isStatus :: Value -> Bool
isStatus = isJust . parseMaybe (parseJSON :: Value -> Parser Task)

runUpdate :: ServerMonadBase m => UpdateOptions -> UpdateM m a -> ClientM m a
runUpdate uopts act = Log.scope "update" $ do
	(r, updatedMods) <- runWriterT (runUpdateM act' `runReaderT` uopts)
	db <- askSession sessionDatabase
	wait db
	dbval <- liftIO $ readAsync db
	let
		cabals = ordNub $ mapMaybe (preview moduleCabal) updatedMods
		projs = ordNub $ mapMaybe (preview $ moduleProject . _Just) updatedMods
		stand = any moduleStandalone updatedMods

		modifiedDb = mconcat $ concat [
			map (`cabalDB` dbval) cabals,
			map (`projectDB` dbval) projs,
			[standaloneDB dbval | stand]]
	serverWriteCache modifiedDb
	return r
	where
		act' = do
			(r, mlocs') <- liftM (second $ filter (isJust . preview moduleFile)) $ listen act
			db <- askSession sessionDatabase
			wait db
			let
				getMods :: (MonadIO m) => m [InspectedModule]
				getMods = do
					db' <- liftIO $ readAsync db
					return $ filter ((`elem` mlocs') . view inspectedId) $ toList $ databaseModules db'
			when (view updateDocs uopts) $ do
				Log.log Log.Trace "forking inspecting source docs"
				void $ fork (getMods >>= waiter . mapM_ scanDocs_)
			when (view updateInfer uopts) $ do
				Log.log Log.Trace "forking inferring types"
				void $ fork (getMods >>= waiter . mapM_ inferModTypes_)
			return r
		scanDocs_ :: UpdateMonad m => InspectedModule -> m ()
		scanDocs_ im = do
			im' <- liftExceptT $ S.scanModify (\opts _ -> inspectDocs opts) im
			updater $ return $ fromModule im'
		inferModTypes_ :: UpdateMonad m => InspectedModule -> m ()
		inferModTypes_ im = do
			-- TODO: locate sandbox
			s <- getSession
			im' <- liftExceptT $ S.scanModify (infer' s) im
			updater $ return $ fromModule im'
		infer' :: Session -> [String] -> Cabal -> Module -> ExceptT String IO Module
		infer' s opts cabal m = case preview (moduleLocation . moduleFile) m of
			Nothing -> return m
			Just _ -> inWorkerT (sessionGhc s) $ inferTypes opts cabal m Nothing
		inWorkerT w = ExceptT . inWorker w . runExceptT 

-- | Post status
postStatus :: UpdateMonad m => Task -> m ()
postStatus t = childTask t onStatus

-- | Wait DB to complete actions
waiter :: UpdateMonad m => m () -> m ()
waiter act = do
	db <- askSession sessionDatabase
	act
	wait db

-- | Update task result to database
updater :: UpdateMonad m => m Database -> m ()
updater act = do
	db <- askSession sessionDatabase
	db' <- act
	update db $ return $!! db'
	tell $!! map (view moduleLocation) $ allModules db'

-- | Clear obsolete data from database
cleaner :: UpdateMonad m => m Database -> m ()
cleaner act = do
	db <- askSession sessionDatabase
	db' <- act
	clear db $ return $!! db'

-- | Get data from cache without updating DB
loadCache :: UpdateMonad m => (FilePath -> ExceptT String IO Structured) -> m Database
loadCache act = do
	mdat <- serverReadCache act
	return $ fromMaybe mempty mdat

-- | Load data from cache if not loaded yet and wait
getCache :: UpdateMonad m => (FilePath -> ExceptT String IO Structured) -> (Database -> Database) -> m Database
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
runTask :: (Display t, UpdateMonad m, NFData a) => String -> t -> m a -> m a
runTask action subj act = Log.scope "task" $ do
	postStatus $ set taskStatus StatusWorking task
	x <- childTask task act
	x `deepseq` postStatus (set taskStatus StatusOk task)
	return x
	`catchError`
	(\c@(CommandError e _) -> postStatus (set taskStatus (StatusError e) task) >> throwError c)
	where
		task = Task {
			_taskName = action,
			_taskStatus = StatusWorking,
			_taskSubjectType = displayType subj,
			_taskSubjectName = display subj,
			_taskProgress = Nothing }

-- | Run many tasks with numeration
runTasks :: UpdateMonad m => [m ()] -> m ()
runTasks ts = zipWithM_ taskNum [1..] (map noErr ts) where
	total = length ts
	taskNum n = local setProgress where
		setProgress = set (updateTasks . _head . taskProgress) (Just (Progress n total))
	noErr v = v `mplus` return ()

-- | Get database value
readDB :: SessionMonad m => m Database
readDB = askSession sessionDatabase >>= liftIO . readAsync

-- | Scan module
scanModule :: UpdateMonad m => [String] -> ModuleLocation -> Maybe String -> m ()
scanModule opts mloc mcts = runTask "scanning" mloc $ Log.scope "module" $ do
	defs <- askSession sessionDefines
	im <- liftExceptT $ S.scanModule defs opts mloc mcts
	updater $ return $ fromModule im
	_ <- return $ view inspectionResult im
	return ()

-- | Scan modules
scanModules :: UpdateMonad m => [String] -> [S.ModuleToScan] -> m ()
scanModules opts ms = runTasks $
	[scanProjectFile opts p >> return () | p <- ps] ++
	[scanModule (opts ++ mopts) m mcts | (m, mopts, mcts) <- ms]
	where
		ps = ordNub $ mapMaybe (toProj . view _1) ms
		toProj (FileModule _ p) = fmap (view projectCabal) p
		toProj _ = Nothing

-- | Scan source file
scanFile :: UpdateMonad m => [String] -> FilePath -> m ()
scanFile opts fpath = scanFileContents opts fpath Nothing

-- | Scan source file with contents
scanFileContents :: UpdateMonad m => [String] -> FilePath -> Maybe String -> m ()
scanFileContents opts fpath mcts = Log.scope "file" $ do
	dbval <- readDB
	fpath' <- liftCIO $ canonicalizePath fpath
	ex <- liftCIO $ doesFileExist fpath'
	mlocs <- if ex
		then do
			mloc <- case lookupFile fpath' dbval of
				Just m -> return $ view moduleLocation m
				Nothing -> do
					mproj <- liftCIO $ locateProject fpath'
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
scanCabal :: UpdateMonad m => [String] -> Cabal -> m ()
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
scanProjectFile :: UpdateMonad m => [String] -> FilePath -> m Project
scanProjectFile opts cabal = runTask "scanning" cabal $ liftExceptT $ S.scanProjectFile opts cabal

-- | Scan project
scanProject :: UpdateMonad m => [String] -> FilePath -> m ()
scanProject opts cabal = runTask "scanning" (project cabal) $ Log.scope "project" $ do
	proj <- scanProjectFile opts cabal
	watch (\w -> watchProject w proj opts)
	(_, sources) <- liftExceptT $ S.enumProject proj
	scan (Cache.loadProject $ view projectCabal proj) (projectDB proj) sources opts $ \ms -> do
		scanModules opts ms
		updater $ return $ fromProject proj

-- | Scan directory for source files and projects
scanDirectory :: UpdateMonad m => [String] -> FilePath -> m ()
scanDirectory opts dir = runTask "scanning" dir $ Log.scope "directory" $ do
	S.ScanContents standSrcs projSrcs sboxes <- liftExceptT $ S.enumDirectory dir
	runTasks [scanProject opts (view projectCabal p) | (p, _) <- projSrcs]
	runTasks $ map (scanCabal opts) sboxes
	mapMOf_ (each . _1) (watch . flip watchModule) standSrcs
	scan (Cache.loadFiles (dir `isParent`)) (filterDB inDir (const False) . standaloneDB) standSrcs opts $ scanModules opts
	where
		inDir = maybe False (dir `isParent`) . preview (moduleIdLocation . moduleFile)

-- | Scan docs for inspected modules
scanDocs :: UpdateMonad m => [InspectedModule] -> m ()
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

inferModTypes :: UpdateMonad m => [InspectedModule] -> m ()
inferModTypes = runTasks . map inferModTypes' where
	inferModTypes' im = runTask "inferring types" (view inspectedId im) $ Log.scope "docs" $ do
		w <- askSession sessionGhc
		Log.log Log.Trace $ "Inferring types for {}" ~~ view inspectedId im
		im' <- liftExceptT $ S.scanModify (\opts cabal m -> inWorkerT w (inferTypes opts cabal m Nothing)) im
		Log.log Log.Trace $ "Types for {} inferred" ~~ view inspectedId im
		updater $ return $ fromModule im'
	inWorkerT w = ExceptT . inWorker w . runExceptT

-- | Generic scan function. Reads cache only if data is not already loaded, removes obsolete modules and rescans changed modules.
scan :: UpdateMonad m
	=> (FilePath -> ExceptT String IO Structured)
	-- ^ Read data from cache
	-> (Database -> Database)
	-- ^ Get data from database
	-> [S.ModuleToScan]
	-- ^ Actual modules. Other modules will be removed from database
	-> [String]
	-- ^ Extra scan options
	-> ([S.ModuleToScan] -> m ())
	-- ^ Function to update changed modules
	-> m ()
scan cache' part' mlocs opts act = Log.scope "scan" $ do
	dbval <- getCache cache' part'
	let
		obsolete = filterDB (\m -> view moduleIdLocation m `notElem` (mlocs ^.. each . _1)) (const False) dbval
	changed <- liftExceptT $ S.changedModules dbval opts mlocs
	cleaner $ return obsolete
	act changed

updateEvent :: ServerMonadBase m => Watched -> Event -> UpdateM m ()
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

liftExceptT :: CommandMonad m => ExceptT String IO a -> m a
liftExceptT act = liftIO (runExceptT act) >>= either commandError_ return

liftCIO ::CommandMonad m => IO a -> m a
liftCIO = liftExceptT . liftE

processEvent :: UpdateOptions -> Watched -> Event -> ClientM IO ()
processEvent uopts w e = runUpdate uopts $ updateEvent w e

watch :: SessionMonad m => (Watcher -> IO ()) -> m ()
watch f = do
	w <- askSession sessionWatcher
	liftIO $ f w
