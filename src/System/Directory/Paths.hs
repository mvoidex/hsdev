{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module System.Directory.Paths (
	Path, path, fromFilePath, joinPaths, splitPaths,
	normPath, subPath, relPathTo,
	dirExists, fileExists, takeDir,
	isParent,
	Paths(..),
	canonicalize,
	absolutise,
	relativise,
	normalize
	) where

import Control.Lens
import Data.List
import Data.Text (Text, pack)
import Data.Text.Lens (unpacked)
import System.Directory
import System.FilePath

-- | Takes much less memory than 'FilePath'
type Path = Text

path :: Lens' Path FilePath
path = unpacked

fromFilePath :: FilePath -> Path
fromFilePath = pack

joinPaths :: [Path] -> Path
joinPaths = fromFilePath . joinPath . map (view path)

splitPaths :: Path -> [Path]
splitPaths = map fromFilePath . splitDirectories . view path

normPath :: Path -> Path
normPath = over path normalise

subPath :: Path -> Path -> Path
subPath p ch = fromFilePath (view path p </> view path ch)

-- | Make path relative
relPathTo :: Path -> Path -> Path
relPathTo base p = fromFilePath $ makeRelative (view path base) (view path p)

dirExists :: Path -> IO Bool
dirExists = doesDirectoryExist . view path

fileExists :: Path -> IO Bool
fileExists = doesFileExist . view path

takeDir :: Path -> Path
takeDir = over path takeDirectory

-- | Is one path parent of another
isParent :: Path -> Path -> Bool
isParent dir file = norm dir `isPrefixOf` norm file where
	norm = dropDot . splitDirectories . normalise . view path
	dropDot ("." : chs) = chs
	dropDot chs = chs

-- | Something with paths inside
class Paths a where
	paths :: Traversal' a FilePath

instance Paths FilePath where
	paths = id

instance Paths Path where
	paths = unpacked

-- | Canonicalize all paths
canonicalize :: Paths a => a -> IO a
canonicalize = paths canonicalizePath

-- | Absolutise paths
absolutise :: Paths a => Path -> a -> a
absolutise parent = over paths addRoot where
	addRoot p
		| isRelative p = (parent ^. path) </> p
		| otherwise = p

-- | Relativise paths
relativise :: Paths a => Path -> a -> a
relativise parent = over paths (makeRelative (parent ^. path))

-- | Normalize paths, with workaround for Windows drives
normalize :: Paths a => a -> a
normalize = over paths normalizePath' where
	normalizePath' :: FilePath -> FilePath
	normalizePath' = fixDrive . normalise
	fixDrive :: FilePath -> FilePath
	fixDrive = uncurry joinDrive . over _1 (addTrailingPathSeparator . dropWhileEnd isPathSeparator) . splitDrive
