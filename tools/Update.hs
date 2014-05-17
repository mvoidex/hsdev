{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, OverloadedStrings, MultiParamTypeClasses #-}

module Update (
	Status(..), isStatus,
	Settings(..),

	UpdateDB,
	updateDB,

	setStatus, postStatus, waiter, updater, loadCache, runTask, runTasks,
	readDB,

	scanModule, scanModules, scanFile, scanCabal, scanProjectFile, scanProject, scanDirectory,

	-- * Helpers
	liftErrorT
	) where

import Control.Applicative
import Control.Monad.CatchIO
import Control.Monad.Error
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe, isJust)
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
import HsDev.Util ((.::))

data Status = Status {
	status :: Value,
	statusDetails :: [Pair] }

instance ToJSON Status where
	toJSON (Status s ds) = object $ ("status" .= s) : ds

instance FromJSON Status where
	parseJSON = withObject "status" $ \v -> Status <$> (v .:: "status") <*> pure (HM.toList $ HM.delete "status" v)

isStatus :: Value -> Bool
isStatus = isJust . parseMaybe (parseJSON :: Value -> Parser Status)

data Settings = Settings {
	database :: Async Database,
	databaseCacheReader :: (FilePath -> ErrorT String IO Structured) -> IO (Maybe Database),
	onStatus :: Status -> IO (),
	ghcOptions :: [String] }

newtype UpdateDB m a = UpdateDB { runUpdateDB :: ReaderT Settings m a }
	deriving (Applicative, Monad, MonadIO, MonadCatchIO, Functor, MonadReader Settings)

-- | Run `UpdateDB` monad
updateDB :: Monad m => Settings -> ErrorT String (UpdateDB m) () -> m ()
updateDB sets act = runUpdateDB (runErrorT act >> return ()) `runReaderT` sets

-- | Set status details
setStatus :: MonadReader Settings m => [Pair] -> m a -> m a
setStatus ps act = local alter act where
	alter st = st {
		onStatus = \(Status s ds) -> onStatus st (Status s (ps ++ ds)) }

-- | Post status
postStatus :: (MonadIO m, MonadReader Settings m) => Value -> m ()
postStatus s = do
	on' <- asks onStatus
	liftIO $ on' $ Status s []

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
runTask :: MonadIO m => String -> [Pair] -> ErrorT String (UpdateDB m) a -> ErrorT String (UpdateDB m) a
runTask action params act = ["task" .= object (("action" .= action) : params)] `setStatus` wrapAct act where
	wrapAct :: MonadIO m => ErrorT String (UpdateDB m) a -> ErrorT String (UpdateDB m) a
	wrapAct a = do
		postStatus $ toJSON ("working" :: String)
		x <- a
		postStatus taskOk
		return x
		`catchError`
		(\e -> postStatus (taskErr e) >> throwError e)
	taskOk = toJSON ("ok" :: String)
	taskErr e = object ["error" .= e]

-- | Run many tasks with numeration
runTasks :: Monad m => [ErrorT String (UpdateDB m) ()] -> ErrorT String (UpdateDB m) ()
runTasks ts = zipWithM_ taskNum [1..] (map noErr ts) where
	total = length ts
	taskNum n t = progress `setStatus` t where
		progress = ["progress" .= object [
			"current" .= (n :: Integer), "total" .= total]]
	noErr v = v `mplus` return ()

-- | Get database value
readDB :: (MonadIO m, MonadReader Settings m) => m Database
readDB = asks database >>= liftIO . readAsync

-- | Scan module
scanModule :: MonadCatchIO m => [String] -> ModuleLocation -> ErrorT String (UpdateDB m) ()
scanModule opts mloc = runTask "scanning" ["object" .= mloc] $ do
	im <- liftErrorT $ S.scanModule opts mloc
	updater $ return $ fromModule im
	ErrorT $ return $ inspectionResult im
	return ()

-- | Scan modules
scanModules :: MonadCatchIO m => [String] -> [S.ModuleToScan] -> ErrorT String (UpdateDB m) ()
scanModules opts ms = do
	db <- asks database
	dbval <- readDB
	runTasks [scanProjectFile opts p >> return () | p <- ps]
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
scanCabal opts sandbox = runTask "scanning" ["sandbox" .= sandbox] $ do
	loadCache $ Cache.loadCabal sandbox
	dbval <- readDB
	ms <- runTask "loading modules" ["sandbox" .= sandbox] $
		liftErrorT $ browseFilter opts sandbox (S.changedModule dbval opts)
	docs <- runTask "loading docs" ["sandbox" .= sandbox] $
		liftErrorT $ hdocsCabal sandbox opts
	updater $ return $ mconcat $ map (fromModule . fmap (setDocs' docs)) ms
	where
		setDocs' :: Map String (Map String String) -> Module -> Module
		setDocs' docs m = maybe m (`setDocs` m) $ M.lookup (moduleName m) docs

-- | Scan project file
scanProjectFile :: MonadCatchIO m => [String] -> FilePath -> ErrorT String (UpdateDB m) Project
scanProjectFile opts cabal = runTask "scanning" ["file" .= cabal] $ do
	proj <- liftErrorT $ S.scanProjectFile opts cabal
	updater $ return $ fromProject proj
	return proj

-- | Scan project
scanProject :: MonadCatchIO m => [String] -> FilePath -> ErrorT String (UpdateDB m) ()
scanProject opts cabal = runTask "scanning" ["project" .= cabal] $ do
	proj <- scanProjectFile opts cabal
	loadCache $ Cache.loadProject $ projectCabal proj
	(_, sources) <- liftErrorT $ S.enumProject proj
	scanModules opts sources

-- | Scan directory for source files and projects
scanDirectory :: MonadCatchIO m => [String] -> FilePath -> ErrorT String (UpdateDB m) ()
scanDirectory opts dir = runTask "scanning" ["path" .= dir] $ do
	(projSrcs, standSrcs) <- runTask "getting list of sources" ["path" .= dir] $
		liftErrorT $ S.enumDirectory dir
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
