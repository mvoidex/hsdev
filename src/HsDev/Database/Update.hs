{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, OverloadedStrings, MultiParamTypeClasses #-}

module HsDev.Database.Update (
	Status(..), Progress(..), Task(..), isStatus,
	Settings(..),

	UpdateDB,
	updateDB,

	postStatus, waiter, updater, loadCache, runTask, runTasks,
	readDB,

	scanModule, scanModules, scanFile, scanCabal, scanProjectFile, scanProject, scanDirectory,

	-- * Helpers
	liftErrorT
	) where

import Control.Applicative
import Control.Monad.CatchIO
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe, isJust)
import System.Directory (canonicalizePath)

import qualified HsDev.Cache.Structured as Cache
import HsDev.Database
import HsDev.Database.Async
import HsDev.Display
import HsDev.Project
import HsDev.Symbols
import HsDev.Tools.HDocs
import qualified HsDev.Scan as S
import HsDev.Scan.Browse
import HsDev.Util ((.::))

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
	databaseCacheReader :: (FilePath -> ErrorT String IO Structured) -> IO (Maybe Database),
	databaseCacheWriter :: Database -> IO (),
	onStatus :: Task -> IO (),
	ghcOptions :: [String] }

newtype UpdateDB m a = UpdateDB { runUpdateDB :: ReaderT Settings (WriterT [ModuleLocation] m) a }
	deriving (Applicative, Monad, MonadIO, MonadCatchIO, Functor, MonadReader Settings, MonadWriter [ModuleLocation])

-- | Run `UpdateDB` monad
updateDB :: MonadIO m => Settings -> ErrorT String (UpdateDB m) () -> m ()
updateDB sets act = do
	updatedMods <- execWriterT (runUpdateDB (runErrorT act >> return ()) `runReaderT` sets)
	wait $ database sets
	dbval <- liftIO $ readAsync $ database sets
	let
		cabals = nub $ mapMaybe moduleCabal_ updatedMods
		projs = nub $ mapMaybe moduleProject_ updatedMods
		stand = any moduleStandalone updatedMods

		modifiedDb = mconcat $ concat [
			map (`cabalDB` dbval) cabals,
			map (`projectDB` dbval) projs,
			[standaloneDB dbval | stand]]
	liftIO $ databaseCacheWriter sets modifiedDb

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
	tell $ map moduleLocation $ allModules db'

-- | Load data from cache and wait
loadCache :: (MonadIO m, MonadReader Settings m, MonadWriter [ModuleLocation] m) => (FilePath -> ErrorT String IO Structured) -> m ()
loadCache act = do
	cacheReader <- asks databaseCacheReader
	mdat <- liftIO $ cacheReader act
	case mdat of
		Nothing -> return ()
		Just dat -> waiter (updater (return dat))

-- | Run one task
runTask :: MonadIO m => String -> [Pair] -> ErrorT String (UpdateDB m) a -> ErrorT String (UpdateDB m) a
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
runTasks :: Monad m => [ErrorT String (UpdateDB m) ()] -> ErrorT String (UpdateDB m) ()
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
scanModule :: MonadCatchIO m => [String] -> ModuleLocation -> ErrorT String (UpdateDB m) ()
scanModule opts mloc = runTask "scanning" (subject mloc ["module" .= mloc]) $ do
	im <- liftErrorT $ S.scanModule opts mloc
	updater $ return $ fromModule im
	_ <- ErrorT $ return $ inspectionResult im
	return ()

-- | Scan modules
scanModules :: MonadCatchIO m => [String] -> [S.ModuleToScan] -> ErrorT String (UpdateDB m) ()
scanModules opts ms = do
	dbval <- readDB
	ms' <- liftErrorT $ filterM (\m -> S.changedModule dbval (opts ++ snd m) (fst m)) ms
	runTasks $
		[scanProjectFile opts p >> return () | p <- ps] ++
		[scanModule (opts ++ snd m) (fst m) | m <- ms']
	where
		ps = nub $ mapMaybe (toProj . fst) ms
		toProj (FileModule _ p) = fmap projectCabal p
		toProj _ = Nothing

-- | Scan source file
scanFile :: MonadCatchIO m => [String] -> FilePath -> ErrorT String (UpdateDB m) ()
scanFile opts fpath = do
	dbval <- readDB
	fpath' <- liftIO $ canonicalizePath fpath
	mloc <- case lookupFile fpath' dbval of
		Just m -> return $ moduleLocation m
		Nothing -> do
			mproj <- liftIO $ locateProject fpath'
			return $ FileModule fpath' mproj
	dirty <- liftErrorT $ S.changedModule dbval opts mloc
	let
		mtarget = moduleProject_ mloc >>= (`fileTarget` fpath')
		fileExts = maybe [] (extensionsOpts . infoExtensions) mtarget
	when dirty $ scanModule (opts ++ fileExts) mloc

-- | Scan cabal modules
scanCabal :: MonadCatchIO m => [String] -> Cabal -> ErrorT String (UpdateDB m) ()
scanCabal opts cabalSandbox = runTask "scanning" (subject cabalSandbox ["sandbox" .= cabalSandbox]) $ do
	loadCache $ Cache.loadCabal cabalSandbox
	dbval <- readDB
	ms <- runTask "loading modules" [] $
		liftErrorT $ browseFilter opts cabalSandbox (S.changedModule dbval opts)
	docs <- runTask "loading docs" [] $
		liftErrorT $ hdocsCabal cabalSandbox opts
	updater $ return $ mconcat $ map (fromModule . fmap (setDocs' docs)) ms
	where
		setDocs' :: Map String (Map String String) -> Module -> Module
		setDocs' docs m = maybe m (`setDocs` m) $ M.lookup (moduleName m) docs

-- | Scan project file
scanProjectFile :: MonadCatchIO m => [String] -> FilePath -> ErrorT String (UpdateDB m) Project
scanProjectFile opts cabal = runTask "scanning" (subject cabal ["file" .= cabal]) $ do
	proj <- liftErrorT $ S.scanProjectFile opts cabal
	updater $ return $ fromProject proj
	return proj

-- | Scan project
scanProject :: MonadCatchIO m => [String] -> FilePath -> ErrorT String (UpdateDB m) ()
scanProject opts cabal = runTask "scanning" (subject (project cabal) ["project" .= cabal]) $ do
	proj <- scanProjectFile opts cabal
	loadCache $ Cache.loadProject $ projectCabal proj
	(_, sources) <- liftErrorT $ S.enumProject proj
	scanModules opts sources

-- | Scan directory for source files and projects
scanDirectory :: MonadCatchIO m => [String] -> FilePath -> ErrorT String (UpdateDB m) ()
scanDirectory opts dir = runTask "scanning" (subject dir ["path" .= dir]) $ do
	(projSrcs, standSrcs) <- runTask "getting list of sources" [] $
		liftErrorT $ S.enumDirectory dir
	runTasks [scanProject opts (projectCabal p) | (p, _) <- projSrcs]
	loadCache $ Cache.loadFiles $ mapMaybe (moduleSource . fst) standSrcs
	scanModules opts standSrcs

-- | Lift errors
liftErrorT :: MonadIO m => ErrorT String IO a -> ErrorT String m a
liftErrorT = mapErrorT liftIO

subject :: Display a => a -> [Pair] -> [Pair]
subject x ps = ["name" .= display x, "type" .= displayType x] ++ ps
