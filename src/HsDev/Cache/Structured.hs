module HsDev.Cache.Structured (
	dump, load,
	loadCabal, loadProject, loadFiles
	) where

import Control.Applicative
import Control.Exception
import Control.Monad.Error
import qualified Data.Map as M
import Data.Monoid
import System.Directory
import System.FilePath

import Data.Group
import qualified HsDev.Cache as Cache
import HsDev.Cabal
import HsDev.Database
import HsDev.Symbols
import HsDev.Project (project)
import HsDev.Util

-- | Write cache
dump :: FilePath -> Structured -> IO ()
dump dir db = do
	createDirectoryIfMissing True (dir </> "cabal")
	createDirectoryIfMissing True (dir </> "projects")
	forM_ (M.assocs $ structuredCabals db) $ \(c, cdb) -> Cache.dump
		(dir </> "cabal" </> Cache.cabalCache c)
		cdb
	forM_ (M.assocs $ structuredProjects db) $ \(p, pdb) -> Cache.dump
		(dir </> "projects" </> Cache.projectCache (project p))
		pdb
	files' <- either (const zero) id <$>
		handle wrapIO
		(Cache.load (dir </> Cache.standaloneCache))
	Cache.dump (dir </> Cache.standaloneCache) $
		files' `mappend` structuredFiles db
	where
		wrapIO :: SomeException -> IO (Either String Database)
		wrapIO = return . Left . show

-- | Load all cache
load :: FilePath -> IO (Either String Structured)
load dir = runErrorT $ join $ either throwError return <$> (structured <$> loadCabals <*> loadProjects <*> loadFa) where
	loadCabals = loadDir (dir </> "cabal")
	loadProjects = loadDir (dir </> "projects")
	loadFa = ErrorT $ Cache.load (dir </> Cache.standaloneCache)

	loadDir p = do
		fs <- liftIO $ liftM (filter ((== ".json") . takeExtension)) $ directoryContents p
		mapM (ErrorT . Cache.load) fs

-- | Load data from cache
loadData :: FilePath -> ErrorT String IO Database
loadData = liftExceptionM . ErrorT . Cache.load

-- | Load cabal from cache
loadCabal :: FilePath -> Cabal -> ErrorT String IO Structured
loadCabal dir c = do
	dat <- loadData (dir </> "cabal" </> Cache.cabalCache c)
	ErrorT $ return $ structured [dat] [] mempty

-- | Load project from cache
loadProject :: FilePath -> FilePath -> ErrorT String IO Structured
loadProject dir p = do
	dat <- loadData (dir </> "projects" </> Cache.projectCache (project p))
	ErrorT $ return $ structured [] [dat] mempty

-- | Load standalone files
loadFiles :: FilePath -> [FilePath] -> ErrorT String IO Structured
loadFiles dir fs = do
	dat <- loadData (dir </> Cache.standaloneCache)
	ErrorT $ return $ structured [] [] $ filterDB inFiles (const False) dat
	where
		inFiles = maybe False (`elem` fs') . moduleSource . moduleLocation
		fs' = map normalise fs
