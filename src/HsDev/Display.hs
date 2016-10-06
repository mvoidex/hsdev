{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Display (
	Display(..)
	) where

import Control.Lens (view)

import Text.Format

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
	display (PackageDb p) = "package-db " ++ p
	displayType _ = "package-db"

instance Display ModuleLocation where
	display (FileModule f _) = f
	display (InstalledModule _ _ n) = n
	display (OtherLocation s) = s
	display NoLocation = "<no-location>"
	displayType _ = "module"

instance Display Project where
	display = view projectName
	displayType _ = "project"

instance Display Sandbox where
	display (Sandbox _ fpath) = fpath
	displayType (Sandbox CabalSandbox _) = "cabal-sandbox"
	displayType (Sandbox StackWork _) = "stack-work"

instance Display FilePath where
	display = id
	displayType _ = "path"

instance FormatBuild PackageDb where
	formatBuild = formatBuild . display

instance FormatBuild ModuleLocation where
	formatBuild = formatBuild . display

instance FormatBuild Project where
	formatBuild = formatBuild . display

