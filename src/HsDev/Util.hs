module HsDev.Util (
	traverseDirectory,
	haskellSource,
	cabalFile,
	-- * String utils
	tab, tabs,
	-- * Helper
	(.::)
	) where

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.List
import qualified Data.HashMap.Strict as HM (HashMap, toList)
import Data.Text (Text)
import System.Directory
import System.FilePath

-- | Collect all file names in directory recursively
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

-- | Is haskell source?
haskellSource :: FilePath -> Bool
haskellSource f = takeExtension f `elem` [".hs", ".lhs"]

-- | Is cabal file?
cabalFile :: FilePath -> Bool
cabalFile f = takeExtension f == ".cabal"

-- | Add N tabs to line
tab :: Int -> String -> String
tab n s = replicate n '\t' ++ s

-- | Add N tabs to multiline
tabs :: Int -> String -> String
tabs n = unlines . map (tab n) . lines

-- | Workaround, sometimes we get HM.lookup "foo" v == Nothing, but lookup "foo" (HM.toList v) == Just smth
(.::) :: FromJSON a => HM.HashMap Text Value -> Text -> Parser a
v .:: name = maybe (fail $ "key " ++ show name ++ " not present") parseJSON $ lookup name $ HM.toList v
