{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module HsDev.Display (
	Display(..)
	) where

import Data.Maybe (fromMaybe)

import HsDev.Cabal
import HsDev.Symbols.Location
import HsDev.Project

class Display a where
	display :: a -> String
	displayType :: a -> String

instance Display Cabal where
	display Cabal = "cabal"
	display (Sandbox p) = "sandbox " ++ p
	displayType _ = "cabal"

instance Display ModuleLocation where
	display (FileModule f _) = f
	display (CabalModule _ _ n) = n
	display (ModuleSource s) = fromMaybe "" s
	displayType _ = "module"

instance Display Project where
	display = projectName
	displayType _ = "project"

instance Display FilePath where
	display = id
	displayType _ = "path"
