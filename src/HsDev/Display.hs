{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Display (
	Display(..)
	) where

import Control.Lens (view)

import System.Directory.Paths

class Display a where
	display :: a -> String
	displayType :: a -> String

instance Display FilePath where
	display = id
	displayType _ = "path"

instance Display Path where
	display = view path
	displayType _ = "path"
