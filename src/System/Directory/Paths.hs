{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module System.Directory.Paths (
	Paths(..),
	canonicalize,
	absolutise,
	relativise
	) where

import Control.Lens
import System.Directory
import System.FilePath

-- | Something with paths inside
class Paths a where
	paths :: Traversal' a FilePath

instance Paths FilePath where
	paths = id

-- | Canonicalize all paths
canonicalize :: Paths a => a -> IO a
canonicalize = paths canonicalizePath

-- | Absolutise paths
absolutise :: Paths a => FilePath -> a -> a
absolutise parent = over paths (parent </>)

-- | Relativise paths
relativise :: Paths a => FilePath -> a -> a
relativise parent = over paths (makeRelative parent)
