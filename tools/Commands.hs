{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Commands (
	Settings(..),

	UpdateDB,
	updateDB,

	setStatus, updater, runTask, runTasks,
	status, readDB,

	scanModule, scanModules, scanFile, scanCabal, scanProject, scanDirectory,

	-- * Helpers
	liftErrors
	) where

import Control.Arrow
import Control.Applicative (pure)
import Control.Exception (SomeException)
import Control.Monad (filterM)
import Control.Monad.CatchIO
import Control.Monad.Error
import Control.Monad.Reader
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.List (find, unfoldr, partition, isPrefixOf)
import Data.Maybe (isNothing, fromMaybe, mapMaybe, listToMaybe)
import Data.Monoid
import Data.Traversable (traverse)
import System.Directory (canonicalizePath, doesFileExist, doesDirectoryExist)
import System.FilePath (makeRelative)

import HsDev.Database
import HsDev.Database.Async
import HsDev.Inspect
import HsDev.Project
import HsDev.Symbols
import HsDev.Tools.GhcMod (list)
import HsDev.Util
import qualified HsDev.Scan as S

data Settings = Settings {
	database :: Async Database,
	onStatus :: Value -> IO (),
	ghcOptions :: [String] }

newtype UpdateDB m a = UpdateDB { runUpdateDB :: ReaderT Settings m a }
	deriving (Monad, MonadIO, MonadCatchIO, Functor, MonadReader Settings)

-- | Run `UpdateDB` monad
updateDB :: Monad m => Settings -> ErrorT String (UpdateDB m) () -> m ()
updateDB sets act = runUpdateDB (runErrorT act >> return ()) `runReaderT` sets

-- | Set partial status
setStatus :: MonadReader Settings m => Value -> m a -> m a
setStatus v act = local alter act where
	alter s = s {
		onStatus = \st -> onStatus s (v `union` st) }

-- | Update task result to database
updater :: (MonadIO m, MonadReader Settings m) => m Database -> m ()
updater act = do
	db <- asks database
	act >>= update db . return

-- | Run one task
runTask :: MonadIO m => Value -> ErrorT String (UpdateDB m) a -> ErrorT String (UpdateDB m) a
runTask v act = object ["task" .= v] `setStatus` act' where
	act' = do
		x <- act
		status (object ["status" .= taskOk])
		return x
		`catchError`
		(\e -> status (object ["status" .= taskErr e]) >> throwError e)
	taskOk = toJSON ("ok" :: String)
	taskErr e = object ["error" .= e]

-- | Run many tasks with numeration
runTasks :: Monad m => [ErrorT String (UpdateDB m) ()] -> ErrorT String (UpdateDB m) ()
runTasks ts = sequence_ $ zipWith taskNum [1..] (map noErr ts) where
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
scanModule opts mloc = runTask task' $ updater $ liftM fromModule $ liftErrors $ S.scanModule opts mloc where
	task' = object ["scanning" .= mloc]

-- | Scan modules
scanModules :: MonadCatchIO m => [String] -> [([String], ModuleLocation)] -> ErrorT String (UpdateDB m) ()
scanModules opts ms = do
	db <- asks database
	dbval <- readDB
	runTask (toJSON ("updating projects files" :: String)) $ updater $ do
		projects <- mapM (liftErrors . S.scanProjectFile opts) ps
		return $ mconcat $ map fromProject projects
	ms' <- liftErrors $ filterM (S.changedModule dbval opts . snd) ms
	runTasks [scanModule (opts ++ fst m) (snd m) | m <- ms']
	where
		ps = mapMaybe (toProj . snd) ms
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
	modules <- runTask (toJSON ("getting list of modules" :: String)) $ liftErrors $
		S.enumCabal opts sandbox
	scanModules opts [([], m) | m <- modules]

-- | Scan project
scanProject :: MonadCatchIO m => [String] -> FilePath -> ErrorT String (UpdateDB m) ()
scanProject opts cabal = do
	proj <- liftErrors $ S.scanProjectFile opts cabal
	sources <- liftErrors $ S.enumProject proj
	scanModules opts sources

-- | Scan directory for source files and projects
scanDirectory :: MonadCatchIO m => [String] -> FilePath -> ErrorT String (UpdateDB m) ()
scanDirectory opts dir = do
	sources <- runTask (toJSON ("getting list of sources" :: String)) $ liftErrors $ S.enumDirectory dir
	scanModules opts sources

-- | Lift errors
liftErrors :: MonadIO m => ErrorT String IO a -> ErrorT String m a
liftErrors = mapErrorT liftIO

-- | Merge two JSON object
union :: Value -> Value -> Value
union (Object l) (Object r) = Object $ HM.union l r
union _ _ = error "Commands.union: impossible happened"
