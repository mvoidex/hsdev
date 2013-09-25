{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}

module Commands (
	Settings(..),

	UpdateDB,
	updateDB,

	scanModule_, scanModule, scanModules,
	scanCabal, scanProject, scanDirectory,

	-- * Helpers
	liftErrors,
	runScan, runScan_
	) where

import Control.Arrow
import Control.Exception (SomeException)
import Control.DeepSeq
import Control.Monad.CatchIO
import Control.Monad.Error
import Control.Monad.Reader
import Data.List (unfoldr, partition)
import Data.Maybe (isNothing, fromMaybe, mapMaybe)
import Data.Monoid
import System.Directory (canonicalizePath, doesFileExist, doesDirectoryExist)

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
	onStatus :: String -> IO (),
	ghcOptions :: [String] }

newtype UpdateDB m a = UpdateDB { runUpdateDB :: ReaderT Settings m a }
	deriving (Monad, MonadIO, MonadCatchIO, Functor, MonadReader Settings)

-- | Run `UpdateDB` monad
updateDB :: Settings -> UpdateDB m a -> m a
updateDB sets = (`runReaderT` sets) . runUpdateDB

-- | Post status
status :: MonadIO m => String -> UpdateDB m ()
status msg = do
	on' <- asks onStatus
	liftIO $ on' msg

-- | Get database value
readDB :: MonadIO m => UpdateDB m Database
readDB = do
	db <- asks database
	liftIO $ readAsync db

-- | Scan module with logging
scanModule_ :: MonadCatchIO m => [String] -> ModuleLocation -> UpdateDB m InspectedModule
scanModule_ opts mloc = runScan mtype mname (InspectedModule InspectionNone mloc . Left) $ liftErrors $ S.scanModule opts mloc where
	(mtype, mname) = case mloc of
		FileModule f _ -> ("file", f)
		CabalModule _ _ n -> ("module", n)
		MemoryModule mn -> ("module", fromMaybe "" mn)

-- | Scan single module
scanModule :: MonadCatchIO m => [String] -> ModuleLocation -> UpdateDB m ()
scanModule opts mloc = do
	im <- scanModule_ opts mloc
	db <- asks database
	update db $ return $ fromModule im

-- | Scan modules with logging
scanModules :: MonadCatchIO m => [String] -> [ModuleLocation] -> ErrorT String (UpdateDB m) ()
scanModules opts ms = do
	db <- asks database
	dbval <- lift readDB
	projects <- mapM (liftErrors . S.scanProjectFile opts) ps
	update db $ return $ mconcat $ map fromProject projects
	ms' <- liftErrors $ S.changedModules dbval opts ms
	scanned <- lift $ mapM (scanModule_ opts) ms'
	update db $ return $ mconcat $ map fromModule scanned
	where
		ps = mapMaybe toProj ms
		toProj (FileModule _ p) = p
		toProj _ = Nothing

-- | Scan cabal modules
scanCabal :: MonadCatchIO m => [String] -> Cabal -> UpdateDB m ()
scanCabal opts sandbox = runScan_ "cabal" (show sandbox) $ do
	modules <- liftErrors $ S.enumCabal opts sandbox
	scanModules opts modules

-- | Scan project
scanProject :: MonadCatchIO m => [String] -> FilePath -> UpdateDB m ()
scanProject opts cabal = runScan_ "project" cabal $ do
	proj <- liftErrors $ S.scanProjectFile opts cabal
	sources <- liftErrors $ S.enumProject proj
	scanModules opts sources

-- | Scan directory for source files and projects
scanDirectory :: MonadCatchIO m => [String] -> FilePath -> UpdateDB m ()
scanDirectory opts dir = runScan_ "directory" dir $ do
	sources <- liftErrors $ S.enumDirectory dir
	scanModules opts sources

-- | Lift errors
liftErrors :: MonadIO m => ErrorT String IO a -> ErrorT String m a
liftErrors = mapErrorT liftIO

-- | Run scan and handle errors
runScan :: MonadIO m => String -> String -> (String -> a) -> ErrorT String (UpdateDB m) a -> UpdateDB m a
runScan scanType name def act = do
	r <- runErrorT act
	case r of
		Left e -> do
			status $ unwords ["error scanning", scanType, name, ":", e]
			return $ def e
		Right v -> do
			status $ unwords [scanType, name, "scanned"]
			return v

-- | Like `runScan`, but ignores result
runScan_ :: MonadIO m => String -> String -> ErrorT String (UpdateDB m) a -> UpdateDB m ()
runScan_ scanType name = runScan scanType name (const ()) . (>> return ())

-- | Catch all exceptions and log them to status
handleError :: MonadCatchIO m => a -> UpdateDB m a -> UpdateDB m a
handleError def act = catch act onError where
	onError e = do
		status $ "exception: " ++ show (e :: SomeException)
		return def

handleError_ :: MonadCatchIO m => UpdateDB m () -> UpdateDB m ()
handleError_ = handleError ()

-- | If then else
cond :: Bool -> a -> a -> a
cond True t _ = t
cond False _ f = f
