module HsDev.Util (
	traverseDirectory,
	haskellSource,
	cabalFile,
	-- * String utils
	tab, tabs
	) where

import Control.Exception
import Control.Monad
import Data.List
import System.Directory
import System.FilePath

traverseDirectory :: FilePath -> IO [FilePath]
traverseDirectory path = handle onError $ do
	cts <- getDirectoryContents path
	liftM concat $ forM (cts \\ [".", ".."]) $ \c -> do
		isDir <- doesDirectoryExist (path </> c)
		if isDir
			then traverseDirectory (path </> c)
			else return [path </> c]
	where
		onError :: IOException -> IO [FilePath]
		onError _ = return []

haskellSource :: FilePath -> Bool
haskellSource f = takeExtension f `elem` [".hs", ".lhs"]

cabalFile :: FilePath -> Bool
cabalFile f = takeExtension f == ".cabal"

tab :: Int -> String -> String
tab n s = replicate n '\t' ++ s

tabs :: Int -> String -> String
tabs n = unlines . map (tab n) . lines
