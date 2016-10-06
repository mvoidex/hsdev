module HsDev.Cache.Structured (
	dump, load,
	loadPackageDb, loadProject, loadFiles
	) where

import Control.DeepSeq
import Control.Exception
import Control.Lens (preview, (^.))
import Control.Monad.Except
import qualified Data.Map as M (assocs)
import System.Directory
import System.FilePath

import Data.Group (Group(zero))
import qualified HsDev.Cache as Cache
import HsDev.Database
import HsDev.Symbols hiding (loadProject)
import HsDev.Util

-- | Write cache
dump :: FilePath -> Structured -> IO ()
dump dir db = do
	createDirectoryIfMissing True (dir </> "cabal")
	createDirectoryIfMissing True (dir </> "projects")
	forM_ (M.assocs $ structuredPackageDbs db) $ \(c, cdb) -> Cache.dump
		(dir </> "cabal" </> Cache.packageDbCache c)
		cdb
	forM_ (M.assocs $ structuredProjects db) $ \(p, pdb) -> Cache.dump
		(dir </> "projects" </> Cache.projectCache (project p))
		pdb
	files' <- either (const zero) id <$>
		handle wrapIO
		(Cache.load (dir </> Cache.standaloneCache))
	files' `deepseq` Cache.dump (dir </> Cache.standaloneCache) (files' `mappend` structuredFiles db)
	where
		wrapIO :: SomeException -> IO (Either String Database)
		wrapIO = return . Left . show

-- | Load all cache
load :: FilePath -> IO (Either String Structured)
load dir = runExceptT $ join $ either throwError return <$> (structured <$> loadPackageDbs <*> loadProjects <*> loadStandaloneFiles) where
	loadPackageDbs = loadDir (dir </> "cabal")
	loadProjects = loadDir (dir </> "projects")
	loadStandaloneFiles = ExceptT $ Cache.load (dir </> Cache.standaloneCache)

	loadDir p = do
		fs <- liftE $ liftM (filter ((== ".json") . takeExtension)) $ directoryContents p
		mapM (ExceptT . Cache.load) fs

-- | Load data from cache
loadData :: FilePath -> ExceptT String IO Database
loadData = liftExceptionM . ExceptT . Cache.load

-- | Load cabal from cache
loadPackageDb :: PackageDb -> FilePath -> ExceptT String IO Structured
loadPackageDb c dir = do
	dat <- loadData (dir </> "cabal" </> Cache.packageDbCache c)
	ExceptT $ return $ structured [dat] [] mempty

-- | Load project from cache
loadProject :: FilePath -> FilePath -> ExceptT String IO Structured
loadProject p dir = do
	dat <- loadData (dir </> "projects" </> Cache.projectCache (project p))
	ExceptT $ return $ structured [] [dat] mempty

-- | Load standalone files
loadFiles :: (FilePath -> Bool) -> FilePath -> ExceptT String IO Structured
loadFiles f dir = do
	dat <- loadData (dir </> Cache.standaloneCache)
	ExceptT $ return $ structured [] [] (dat ^. slice f')
	where
		f' = maybe False f . preview (moduleLocation . moduleFile)
