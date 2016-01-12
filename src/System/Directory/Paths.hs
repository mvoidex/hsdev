{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module System.Directory.Paths (
	Paths(..),
	canonicalize
	) where

import Control.Lens
import System.Directory

-- | Something with paths inside
class Paths a where
	paths :: Traversal' a FilePath

instance Paths FilePath where
	paths = id

-- | Canonicalize all paths
canonicalize :: Paths a => a -> IO a
canonicalize = paths canonicalizePath
