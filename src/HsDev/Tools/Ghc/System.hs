{-# LANGUAGE OverloadedStrings #-}

module HsDev.Tools.Ghc.System (
	BuildInfo(..), buildInfo,
	examineCompilerVersion,

	formatBuildPath, buildPath
	) where

import qualified System.Info as Sys
import Text.Format

data BuildInfo = BuildInfo {
	targetArch :: String,
	targetPlatform :: String,
	compilerName :: String,
	compilerVersion :: String }

buildInfo :: DynFlags -> BuildInfo
buildInfo = BuildInfo Sys.arch Sys.os Sys.compilerName . examineCompilerVersion

examineCompilerVersion :: DynFlags -> String
examineCompilerVersion =
	maybe Sys.compilerVersion display .
	M.lookup Sys.compilerNam .
	M.fromList .
	map (GHC.packageNameString &&& GHC.packageVersion) .
	fromMaybe [] . Compat.pkgDatabase

-- | Can contain {arch}, {os}/{platform}, {compiler}, {version}
formatBuildPath :: String -> BuildInfo -> String
formatBuildPath f = formats f . toArgs where
	toArgs b = [
		"arch" ~% targetArch b,
		"os" ~% targetPlatform b,
		"platform" ~% targetPlatform b,
		"compiler" ~% compilerName b,
		"version" ~% compilerVersion b]

buildPath :: String -> GhcM FilePath
buildPath f = fmap (formatBuildPath f . buildInfo) getSessionDynFlags
