{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Util (
	traverseDirectory,
	haskellSource,
	-- * String utils
	tab, tabs
	) where

import Control.Monad
import Data.List
import System.Directory
import System.FilePath

traverseDirectory :: FilePath -> IO [FilePath]
traverseDirectory path = do
	cts <- getDirectoryContents path
	liftM concat $ forM (cts \\ [".", ".."]) $ \c -> do
		isDir <- doesDirectoryExist (path </> c)
		if isDir
			then traverseDirectory (path </> c)
			else return [path </> c]

haskellSource :: FilePath -> Bool
haskellSource f = takeExtension f `elem` [".hs", ".lhs"]

tab :: Int -> String -> String
tab n s = replicate n '\t' ++ s

tabs :: Int -> String -> String
tabs n = unlines . map (tab n) . lines
