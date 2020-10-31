{-# LANGUAGE OverloadedStrings, PackageImports #-}

module HsDev.Tools.Ghc.System (
	BuildInfo(..), buildInfo,
	examineCompilerVersion,

	formatBuildPath, buildPath
	) where

import Control.Arrow
import qualified Data.Map as M
import Data.Maybe
import Data.Version (showVersion)
import Distribution.System (buildOS)
import Distribution.Text (display)
import qualified System.Info as Sys
import Text.Format

import "ghc" DynFlags (DynFlags)
import "ghc" PackageConfig as GHC
import "ghc" GHC (getSessionDynFlags)

import HsDev.Tools.Ghc.Compat as Compat
import HsDev.Tools.Ghc.Worker (GhcM)

data BuildInfo = BuildInfo {
	targetArch :: String,
	targetOS :: String,
	cabalOS :: String,
	compilerName :: String,
	compilerVersion :: String }

buildInfo :: DynFlags -> BuildInfo
buildInfo = BuildInfo Sys.arch Sys.os (display buildOS) Sys.compilerName . examineCompilerVersion

examineCompilerVersion :: DynFlags -> String
examineCompilerVersion =
	showVersion .
	fromMaybe Sys.compilerVersion .
	M.lookup Sys.compilerName .
	M.fromList .
	map (GHC.packageNameString &&& GHC.packageVersion) .
	fromMaybe [] . Compat.pkgDatabase

-- | Can contain {arch}, {os}/{platform}, {compiler}, {version}
formatBuildPath :: String -> BuildInfo -> String
formatBuildPath f = formats f . toArgs where
	toArgs b = [
		"arch" ~% targetArch b,
		"os" ~% targetOS b,
		"os/cabal" ~% cabalOS b,
		"compiler" ~% compilerName b,
		"version" ~% compilerVersion b]

buildPath :: String -> GhcM FilePath
buildPath f = fmap (formatBuildPath f . buildInfo) getSessionDynFlags
