{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, OverloadedStrings, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Database.Update (
	Status(..), Progress(..), Task(..), isStatus,
	Settings(..),

	UpdateDB,
	updateDB,

	postStatus, waiter, updater, loadCache, getCache, runTask, runTasks,
	readDB,

	scanModule, scanModules, scanFile, scanCabal, scanProjectFile, scanProject, scanDirectory,
	scan,

	-- * Helpers
	liftExceptT
	) where

import Control.Lens (preview, _Just, view, set, _Right)
import Control.Monad.Catch
import Control.Monad.CatchIO
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Aeson
import Data.Aeson.Types
import Data.Either (rights)
import Data.Function (on)
import Data.List (sortBy, groupBy)
import qualified Data.HashMap.Strict as HM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe, isJust, fromMaybe, catMaybes)
import Data.Ord (comparing)
import qualified Data.Text as T (unpack)
import System.Directory (canonicalizePath)
import qualified System.Log.Simple as Log
import qualified System.Log.Simple.Base as Log (scopeLog)

import Control.Concurrent.Worker (Worker)
import qualified HsDev.Cache.Structured as Cache
import HsDev.Database
import HsDev.Database.Async
import HsDev.Display
import HsDev.Inspect (inspectDocsChunk, inspectDocs)
import HsDev.Project
import HsDev.Symbols
import HsDev.Tools.HDocs
import HsDev.Tools.GhcMod.InferType (inferTypes)
import HsDev.Tools.GhcMod (WorkerMap)
import qualified HsDev.Tools.GhcMod as GhcMod
import qualified HsDev.Scan as S
import HsDev.Scan.Browse
import HsDev.Util ((.::), liftEIO, isParent, ordNub)

data Status = StatusWorking | StatusOk | StatusError String

instance ToJSON Status where
	toJSON StatusWorking = toJSON ("working" :: String)
	toJSON StatusOk = toJSON ("ok" :: String)
	toJSON (StatusError e) = toJSON $ object ["error" .= e]

instance FromJSON Status where
	parseJSON v = msum $ map ($ v) [
		withText "status" $ \t -> guard (t == "working") *> return StatusWorking,
		withText "status" $ \t -> guard (t == "ok") *> return StatusOk,
		withObject "status" $ \obj -> StatusError <$> (obj .:: "error"),
		fail "invalid status"]

data Progress = Progress {
	progressCurrent :: Int,
	progressTotal :: Int }

instance ToJSON Progress where
	toJSON (Progress c t) = object [
		"current" .= c,
		"total" .= t]

instance FromJSON Progress where
	parseJSON = withObject "progress" $ \v -> Progress <$> (v .:: "current") <*> (v .:: "total")

data Task = Task {
	taskName :: String,
	taskStatus :: Status,
	taskParams :: Object,
	taskProgress :: Maybe Progress,
	taskChild :: Maybe Task }

instance ToJSON Task where
	toJSON t = object [
		"status" .= taskStatus t,
		"task" .= taskName t,
		"params" .= taskParams t,
		"progress" .= taskProgress t,
		"child" .= taskChild t]

instance FromJSON Task where
	parseJSON = withObject "task" $ \v -> Task <$>
		(v .:: "task") <*>
		(v .:: "status") <*>
		(v .:: "params") <*>
		(v .:: "progress") <*>
		(v .:: "child")

isStatus :: Value -> Bool
isStatus = isJust . parseMaybe (parseJSON :: Value -> Parser Task)

data Settings = Settings {
	database :: Async Database,
	databaseCacheReader :: (FilePath -> ExceptT String IO Structured) -> IO (Maybe Database),
	databaseCacheWriter :: Database -> IO (),
	onStatus :: Task -> IO (),
	ghcOptions :: [String],
	updateDocs :: Bool,
	runInferTypes :: Bool,
	settingsGhcModWorker :: Worker (ReaderT WorkerMap IO),
	settingsLogger :: Log.Log }

newtype UpdateDB m a = UpdateDB { runUpdateDB :: ReaderT Settings (WriterT [ModuleLocation] m) a }
	deriving (Applicative, Monad, MonadIO, MonadCatchIO, MonadThrow, MonadCatch, Functor, MonadReader Settings, MonadWriter [ModuleLocation])

instance MonadCatchIO m => MonadCatchIO (ExceptT e m) where
	catch act onError = ExceptT $ Control.Monad.CatchIO.catch (runExceptT act) (runExceptT . onError)
	block = ExceptT . block . runExceptT
	unblock = ExceptT . unblock . runExceptT

instance MonadCatchIO m => Log.MonadLog (UpdateDB m) where
	askLog = liftM settingsLogger ask

instance Log.MonadLog m => Log.MonadLog (ExceptT e m) where
	askLog = lift Log.askLog

-- | Run `UpdateDB` monad
updateDB :: MonadCatchIO m => Settings -> ExceptT String (UpdateDB m) () -> m ()
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
				getMods = do
					db' <- liftIO $ readAsync $ database sets
					return $ catMaybes [M.lookup mloc' (databaseModules db') | mloc' <- mlocs']
			when (updateDocs sets) $ do
				Log.log Log.Trace "inspecting source docs"
				(getMods >>= waiter . runTask "inspecting source docs" [] . runTasks . map scanDocs)
			when (runInferTypes sets) $ do
				Log.log Log.Trace "inferring types"
				(getMods >>= waiter . runTask "inferring types" [] . runTasks . map inferModTypes)
		scanDocs :: MonadIO m => InspectedModule -> ExceptT String (UpdateDB m) ()
		scanDocs im = runTask "scanning docs" (subject (view inspectedId im) ["module" .= view inspectedId im]) $ do
			im' <- liftExceptT $ S.scanModify (\opts _ -> inspectDocs opts) im
			updater $ return $ fromModule im'
		inferModTypes :: MonadIO m => InspectedModule -> ExceptT String (UpdateDB m) ()
		inferModTypes im = runTask "inferring types" (subject (view inspectedId im) ["module" .= view inspectedId im]) $ do
			-- TODO: locate sandbox
			im' <- liftExceptT $ S.scanModify infer' im
			updater $ return $ fromModule im'
		infer' :: [String] -> Cabal -> Module -> ExceptT String IO Module
		infer' opts cabal m = case preview (moduleLocation . moduleFile) m of
			Nothing -> return m
			Just f -> GhcMod.waitMultiGhcMod (settingsGhcModWorker sets) f $
				inferTypes opts cabal m

-- | Post status
postStatus :: (MonadIO m, MonadReader Settings m) => Task -> m ()
postStatus s = do
	on' <- asks onStatus
	liftIO $ on' s

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
	update db $ return db'
	tell $ map (view moduleLocation) $ allModules db'

-- | Clear obsolete data from database
cleaner :: (MonadIO m, MonadReader Settings m, MonadWriter [ModuleLocation] m) => m Database -> m ()
cleaner act = do
	db <- asks database
	db' <- act
	clear db $ return db'

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
runTask :: MonadIO m => String -> [Pair] -> ExceptT String (UpdateDB m) a -> ExceptT String (UpdateDB m) a
runTask action params act = do
	postStatus $ task { taskStatus = StatusWorking }
	x <- local childTask act
	postStatus $ task { taskStatus = StatusOk }
	return x
	`catchError`
	(\e -> postStatus (task { taskStatus = StatusError e }) >> throwError e)
	where
		task = Task {
			taskName = action,
			taskStatus = StatusWorking,
			taskParams = HM.fromList params,
			taskProgress = Nothing,
			taskChild = Nothing }
		childTask st = st {
			onStatus = \t -> onStatus st (task { taskChild = Just t }) }

-- | Run many tasks with numeration
runTasks :: Monad m => [ExceptT String (UpdateDB m) ()] -> ExceptT String (UpdateDB m) ()
runTasks ts = zipWithM_ taskNum [1..] (map noErr ts) where
	total = length ts
	taskNum n = local setProgress where
		setProgress st = st {
			onStatus = \t -> onStatus st (t { taskProgress = Just (Progress n total) }) }
	noErr v = v `mplus` return ()

-- | Get database value
readDB :: (MonadIO m, MonadReader Settings m) => m Database
readDB = asks database >>= liftIO . readAsync

-- | Scan module
scanModule :: (MonadIO m, MonadCatch m) => [String] -> ModuleLocation -> ExceptT String (UpdateDB m) ()
scanModule opts mloc = runTask "scanning" (subject mloc ["module" .= mloc]) $ do
	im <- liftExceptT $ S.scanModule opts mloc
	updater $ return $ fromModule im
	_ <- ExceptT $ return $ view inspectionResult im
	return ()

-- | Scan modules
scanModules :: (MonadIO m, MonadCatch m) => [String] -> [S.ModuleToScan] -> ExceptT String (UpdateDB m) ()
scanModules opts ms = runTasks $
	[scanProjectFile opts p >> return () | p <- ps] ++
	[scanModule (opts ++ snd m) (fst m) | m <- ms]
	where
		ps = ordNub $ mapMaybe (toProj . fst) ms
		toProj (FileModule _ p) = fmap (view projectCabal) p
		toProj _ = Nothing

-- | Scan source file
scanFile :: (MonadIO m, MonadCatch m) => [String] -> FilePath -> ExceptT String (UpdateDB m) ()
scanFile opts fpath = do
	dbval <- readDB
	fpath' <- liftEIO $ canonicalizePath fpath
	mloc <- case lookupFile fpath' dbval of
		Just m -> return $ view moduleLocation m
		Nothing -> do
			mproj <- liftEIO $ locateProject fpath'
			return $ FileModule fpath' mproj
	dirty <- liftExceptT $ S.changedModule dbval opts mloc
	let
		mtarget = preview (moduleProject . _Just) mloc >>= (`fileTarget` fpath')
		fileExts = maybe [] (extensionsOpts . view infoExtensions) mtarget
	when dirty $ scanModule (opts ++ fileExts) mloc

-- | Scan cabal modules
scanCabal :: (MonadIO m, MonadCatch m) => [String] -> Cabal -> ExceptT String (UpdateDB m) ()
scanCabal opts cabalSandbox = runTask "scanning" (subject cabalSandbox ["sandbox" .= cabalSandbox]) $ do
	mlocs <- runTask "getting list of cabal modules" [] $ 
		liftExceptT $ listModules opts cabalSandbox
	scan (Cache.loadCabal cabalSandbox) (cabalDB cabalSandbox) (zip mlocs $ repeat []) opts $ \mlocs' -> do
		ms <- runTask "loading modules" [] $
			liftExceptT $ browseModules opts cabalSandbox (map fst mlocs')
		docs <- runTask "loading docs" [] $
			liftExceptT $ hdocsCabal cabalSandbox opts
		updater $ return $ mconcat $ map (fromModule . fmap (setDocs' docs)) ms
	where
		setDocs' :: Map String (Map String String) -> Module -> Module
		setDocs' docs m = maybe m (`setDocs` m) $ M.lookup (T.unpack $ view moduleName m) docs

-- | Scan project file
scanProjectFile :: (MonadIO m, MonadCatch m) => [String] -> FilePath -> ExceptT String (UpdateDB m) Project
scanProjectFile opts cabal = runTask "scanning" (subject cabal ["file" .= cabal]) $ liftExceptT $ S.scanProjectFile opts cabal

-- | Scan project
scanProject :: (MonadIO m, MonadCatch m) => [String] -> FilePath -> ExceptT String (UpdateDB m) ()
scanProject opts cabal = runTask "scanning" (subject (project cabal) ["project" .= cabal]) $ do
	proj <- scanProjectFile opts cabal
	(_, sources) <- liftExceptT $ S.enumProject proj
	scan (Cache.loadProject $ view projectCabal proj) (projectDB proj) sources opts $ \ms -> do
		scanModules opts ms
		updater $ return $ fromProject proj

-- | Scan directory for source files and projects
scanDirectory :: (MonadIO m, MonadCatch m) => [String] -> FilePath -> ExceptT String (UpdateDB m) ()
scanDirectory opts dir = runTask "scanning" (subject dir ["path" .= dir]) $ do
	S.ScanContents standSrcs projSrcs sboxes <- runTask "getting list of sources" [] $
		liftExceptT $ S.enumDirectory dir
	runTasks [scanProject opts (view projectCabal p) | (p, _) <- projSrcs]
	runTasks $ map (scanCabal opts) sboxes
	scan (Cache.loadFiles (dir `isParent`)) (filterDB inDir (const False) . standaloneDB) standSrcs opts $ scanModules opts
	where
		inDir = maybe False (dir `isParent`) . preview (moduleIdLocation . moduleFile)

-- | Generic scan function. Reads cache only if data is not already loaded, removes obsolete modules and rescans changed modules.
scan :: (MonadIO m, MonadCatch m)
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
scan cache' part' mlocs opts act = do
	dbval <- getCache cache' part'
	let
		obsolete = filterDB (\m -> view moduleIdLocation m `notElem` map fst mlocs) (const False) dbval
	changed <- runTask "getting list of changed modules" [] $ liftExceptT $ S.changedModules dbval opts mlocs
	runTask "removing obsolete modules" ["modules" .= map (view moduleLocation) (allModules obsolete)] $ cleaner $ return obsolete
	act changed

-- | Lift errors
liftExceptT :: MonadIO m => ExceptT String IO a -> ExceptT String m a
liftExceptT = mapExceptT liftIO

subject :: Display a => a -> [Pair] -> [Pair]
subject x ps = ["name" .= display x, "type" .= displayType x] ++ ps
