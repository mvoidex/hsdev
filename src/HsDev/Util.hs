module HsDev.Util (
	locateProject,
	traverseDirectory
	) where

import Control.Monad
import Data.List
import System.Directory
import System.FilePath

import HsDev.Project

-- | Find project file is related to
locateProject :: FilePath -> IO (Maybe Project)
locateProject file = do
	file' <- canonicalizePath file
	isDir <- doesDirectoryExist file'
	if isDir then locateHere file' else locateParent (takeDirectory file')
	where
		locateHere path = do
			cts <- getDirectoryContents path
			return $ fmap (project . (path </>)) $ find ((== ".cabal") . takeExtension) cts
		locateParent dir = do
			cts <- getDirectoryContents dir
			case find ((== ".cabal") . takeExtension) cts of
				Nothing -> if isDrive dir then return Nothing else locateParent (takeDirectory dir)
				Just cabalFile -> return $ Just $ project (dir </> cabalFile)

traverseDirectory :: FilePath -> IO [FilePath]
traverseDirectory path = do
	cts <- getDirectoryContents path
	liftM concat $ forM (cts \\ [".", ".."]) $ \c -> do
		isDir <- doesDirectoryExist (path </> c)
		if isDir
			then traverseDirectory (path </> c)
			else return [path </> c]
