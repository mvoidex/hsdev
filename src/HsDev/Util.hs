module HsDev.Util (
	locateProject
	) where

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
