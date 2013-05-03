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
locateProject file = findCabal (takeDirectory file) where
	findCabal dir = do
		cts <- getDirectoryContents dir
		case find ((== ".cabal") . takeExtension) cts of
			Nothing -> if isDrive dir then return Nothing else findCabal (takeDirectory dir)
			Just cabalFile -> return $ Just $ projectByCabal cabalFile

traverseDirectory :: FilePath -> IO [FilePath]
traverseDirectory path = do
	cts <- getDirectoryContents path
	liftM concat $ forM (cts \\ [".", ".."]) $ \c -> do
		isDir <- doesDirectoryExist (path </> c)
		if isDir
			then traverseDirectory (path </> c)
			else return [path </> c]

