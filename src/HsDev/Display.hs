{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Display (
	Display(..)
	) where

import Control.Lens (view)
import Data.List (intercalate)
import Data.Text.Lens (unpacked)

import Text.Format

import System.Directory.Paths
import HsDev.PackageDb
import HsDev.Project
import HsDev.Sandbox
import HsDev.Symbols.Location

class Display a where
	display :: a -> String
	displayType :: a -> String

instance Display PackageDb where
	display GlobalDb = "global-db"
	display UserDb = "user-db"
	display (PackageDb p) = "package-db " ++ display p
	displayType _ = "package-db"

instance Display PackageDbStack where
	display = intercalate "/" . map display . packageDbs
	displayType _ = "package-db-stack"

instance Display ModuleLocation where
	display (FileModule f _) = display f
	display (InstalledModule _ _ n) = view unpacked n
	display (OtherLocation s) = view unpacked s
	display NoLocation = "<no-location>"
	displayType _ = "module"

instance Display Project where
	display = view (projectName . unpacked)
	displayType _ = "project"

instance Display Sandbox where
	display (Sandbox _ fpath) = display fpath
	displayType (Sandbox CabalSandbox _) = "cabal-sandbox"
	displayType (Sandbox StackWork _) = "stack-work"

instance Display FilePath where
	display = id
	displayType _ = "path"

instance Display Path where
	display = view path
	displayType _ = "path"

instance Formattable PackageDb where
	formattable = formattable . display

instance Formattable PackageDbStack where
	formattable = formattable . display

instance Formattable ModuleLocation where
	formattable = formattable . display

instance Formattable Project where
	formattable = formattable . display

instance Formattable Sandbox where
	formattable = formattable . display