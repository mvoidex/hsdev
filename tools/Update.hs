{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, OverloadedStrings, MultiParamTypeClasses #-}

module Update (
	Settings(..),

	UpdateDB,
	updateDB,

	setStatus, waiter, updater, loadCache, runTask, runTasks,
	status, readDB,

	scanModule, scanModules, scanFile, scanCabal, scanProject, scanDirectory,

	-- * Helpers
	liftErrorT
	) where

import Control.Applicative (Applicative(..))
import Control.Monad.CatchIO
import Control.Monad.Error
import Control.Monad.Reader
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Monoid
import Data.Traversable (traverse)
import System.Directory (canonicalizePath)

import qualified HsDev.Cache.Structured as Cache
import HsDev.Database
import HsDev.Database.Async
import HsDev.Project
import HsDev.Symbols
import HsDev.Tools.HDocs
import qualified HsDev.Scan as S
import HsDev.Scan.Browse

data Settings = Settings {
	database :: Async Database,
	databaseCacheReader :: (FilePath -> ErrorT String IO Structured) -> IO (Maybe Database),
	onStatus :: Value -> IO (),
	ghcOptions :: [String] }

newtype UpdateDB m a = UpdateDB { runUpdateDB :: ReaderT Settings m a }
	deriving (Applicative, Monad, MonadIO, MonadCatchIO, Functor, MonadReader Settings)

-- | Run `UpdateDB` monad
updateDB :: Monad m => Settings -> ErrorT String (UpdateDB m) () -> m ()
updateDB sets act = runUpdateDB (runErrorT act >> return ()) `runReaderT` sets

-- | Set partial status
setStatus :: MonadReader Settings m => Value -> m a -> m a
setStatus v act = local alter act where
	alter s = s {
		onStatus = \st -> onStatus s (v `union` st) }

-- | Wait DB to complete actions
waiter :: (MonadIO m, MonadReader Settings m) => m () -> m ()
waiter act = do
	db <- asks database
	act
	wait db

-- | Update task result to database
updater :: (MonadIO m, MonadReader Settings m) => m Database -> m ()
updater act = do
	db <- asks database
	act >>= update db . return

-- | Load data from cache and wait
loadCache :: (MonadIO m, MonadReader Settings m) => (FilePath -> ErrorT String IO Structured) -> m ()
loadCache act = do
	cacheReader <- asks databaseCacheReader
	mdat <- liftIO $ cacheReader act
	case mdat of
		Nothing -> return ()
		Just dat -> waiter (updater (return dat))

-- | Run one task
runTask :: MonadIO m => Value -> ErrorT String (UpdateDB m) a -> ErrorT String (UpdateDB m) a
runTask v act = object ["task" .= v] `setStatus` wrapAct act where
	wrapAct :: MonadIO m => ErrorT String (UpdateDB m) a -> ErrorT String (UpdateDB m) a
	wrapAct a = do
		status $ object ["status" .= toJSON ("working" :: String)]
		x <- a
		status $ object ["status" .= taskOk]
		return x
		`catchError`
		(\e -> status (object ["status" .= taskErr e]) >> throwError e)
	taskOk = toJSON ("ok" :: String)
	taskErr e = object ["error" .= e]

-- | Run many tasks with numeration
runTasks :: Monad m => [ErrorT String (UpdateDB m) ()] -> ErrorT String (UpdateDB m) ()
runTasks ts = zipWithM_ taskNum [1..] (map noErr ts) where
	total = length ts
	taskNum n t = progress `setStatus` t where
		progress = object ["progress" .= object [
			"current" .= (n :: Integer), "total" .= total]]
	noErr v = v `mplus` return ()

-- | Post status
status :: (MonadIO m, MonadReader Settings m) => Value -> m ()
status msg = do
	on' <- asks onStatus
	liftIO $ on' msg

-- | Get database value
readDB :: (MonadIO m, MonadReader Settings m) => m Database
readDB = asks database >>= liftIO . readAsync

-- | Scan module
scanModule :: MonadCatchIO m => [String] -> ModuleLocation -> ErrorT String (UpdateDB m) ()
scanModule opts mloc = runTask task' $ do
	im <- liftErrorT $ S.scanModule opts mloc
	updater $ return $ fromModule im
	ErrorT $ return $ inspectionResult im
	return ()
	where
		task' = object ["scanning" .= mloc]

-- | Scan modules
scanModules :: MonadCatchIO m => [String] -> [S.ModuleToScan] -> ErrorT String (UpdateDB m) ()
scanModules opts ms = do
	db <- asks database
	dbval <- readDB
	runTask (toJSON ("updating projects files" :: String)) $ updater $ do
		projects <- mapM (liftErrorT . S.scanProjectFile opts) ps
		return $ mconcat $ map fromProject projects
	ms' <- liftErrorT $ filterM (S.changedModule dbval opts . fst) ms
	runTasks [scanModule (opts ++ snd m) (fst m) | m <- ms']
	where
		ps = mapMaybe (toProj . fst) ms
		toProj (FileModule _ p) = fmap projectCabal p
		toProj _ = Nothing

-- | Scan source file
scanFile :: MonadCatchIO m => [String] -> FilePath -> ErrorT String (UpdateDB m) ()
scanFile opts fpath = do
	fpath' <- liftIO $ canonicalizePath fpath
	mproj <- liftIO $ locateProject fpath'
	let
		mtarget = mproj >>= (`fileTarget` fpath')
		fileExts = maybe [] (extensionsOpts . infoExtensions) mtarget

	scanModule (opts ++ fileExts) (FileModule fpath' mproj)

-- | Scan cabal modules
scanCabal :: MonadCatchIO m => [String] -> Cabal -> ErrorT String (UpdateDB m) ()
scanCabal opts sandbox = do
	loadCache $ Cache.loadCabal sandbox
	dbval <- readDB
	ms <- runTask
		(object ["action" .= ("loading modules" :: String), "sandbox" .= sandbox]) $
		liftErrorT $ browseFilter opts sandbox (S.changedModule dbval opts)
	docs <- runTask
		(object ["action" .= ("loading docs" :: String), "sandbox" .= sandbox]) $
		liftErrorT $ hdocsCabal sandbox opts
	updater $ return $ mconcat $ map (fromModule . fmap (setDocs' docs)) ms
	where
		setDocs' :: Map String (Map String String) -> Module -> Module
		setDocs' docs m = maybe m (`setDocs` m) $ M.lookup (moduleName m) docs

-- | Scan project
scanProject :: MonadCatchIO m => [String] -> FilePath -> ErrorT String (UpdateDB m) ()
scanProject opts cabal = do
	proj <- liftErrorT $ S.scanProjectFile opts cabal
	loadCache $ Cache.loadProject $ projectCabal proj
	(_, sources) <- liftErrorT $ S.enumProject proj
	scanModules opts sources

-- | Scan directory for source files and projects
scanDirectory :: MonadCatchIO m => [String] -> FilePath -> ErrorT String (UpdateDB m) ()
scanDirectory opts dir = do
	(projSrcs, standSrcs) <- runTask (toJSON ("getting list of sources" :: String)) $ liftErrorT $ S.enumDirectory dir
	forM_ projSrcs $ \(p, _) -> loadCache (Cache.loadProject $ projectCabal p)
	loadCache $ Cache.loadFiles $ mapMaybe (moduleSource . fst) standSrcs
	scanModules opts (concatMap snd projSrcs ++ standSrcs)

-- | Lift errors
liftErrorT :: MonadIO m => ErrorT String IO a -> ErrorT String m a
liftErrorT = mapErrorT liftIO

-- | Merge two JSON object
union :: Value -> Value -> Value
union (Object l) (Object r) = Object $ HM.union l r
union _ _ = error "Commands.union: impossible happened"
