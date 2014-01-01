module HsDev.Cache.Structured (
	dump, load
	) where

import Control.Applicative
import Control.Exception
import Control.Monad.Error
import qualified Data.Map as M
import Data.Monoid (mappend)
import System.Directory
import System.FilePath

import Data.Group
import qualified HsDev.Cache as Cache
import HsDev.Database
import HsDev.Project (project)
import HsDev.Util (directoryContents)

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

load :: FilePath -> IO (Either String Structured)
load dir = runErrorT $ join $ either throwError return <$> (structured <$> loadCabals <*> loadProjects <*> loadFiles) where
	loadCabals = loadDir (dir </> "cabal")
	loadProjects = loadDir (dir </> "projects")
	loadFiles = ErrorT $ Cache.load (dir </> Cache.standaloneCache)

	loadDir p = do
		fs <- liftIO $ liftM (filter ((== ".json") . takeExtension)) $ directoryContents p
		mapM (ErrorT . Cache.load) fs
