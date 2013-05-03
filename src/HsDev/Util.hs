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
	isDir <- doesDirectoryExist file
	if isDir then locateHere else locateParent (takeDirectory file)
	where
		locateHere = do
			cts <- getDirectoryContents file
			return $ fmap projectByCabal $ find ((== ".cabal") . takeExtension) cts
		locateParent dir = do
			cts <- getDirectoryContents dir
			case find ((== ".cabal") . takeExtension) cts of
				Nothing -> if isDrive dir then return Nothing else locateParent (takeDirectory dir)
				Just cabalFile -> return $ Just $ projectByCabal cabalFile

traverseDirectory :: FilePath -> IO [FilePath]
traverseDirectory path = do
	cts <- getDirectoryContents path
	liftM concat $ forM (cts \\ [".", ".."]) $ \c -> do
		isDir <- doesDirectoryExist (path </> c)
		if isDir
			then traverseDirectory (path </> c)
			else return [path </> c]

