{-# LANGUAGE OverloadedStrings #-}

module HsDev.Project.JSON (
	encodeProject
	) where

import Data.Aeson
import Data.List (intercalate)

import HsDev.Project

encodeProject :: Project -> Value
encodeProject p = object [
	"name" .= projectName p,
	"cabal" .= projectCabal p,
	"description" .= fmap encodeDescription (projectDescription p)]

encodeDescription :: ProjectDescription -> Value
encodeDescription d = object [
	"library" .= fmap encodeLibrary (projectLibrary d),
	"executables" .= fmap encodeExecutable (projectExecutables d),
	"tests" .= fmap encodeTest (projectTests d)]

encodeLibrary :: Library -> Value
encodeLibrary l = object [
	"modules" .= fmap (intercalate ".") (libraryModules l),
	"info" .= encodeInfo (libraryBuildInfo l)]

encodeExecutable :: Executable -> Value
encodeExecutable e = object [
	"name" .= executableName e,
	"path" .= executablePath e,
	"info" .= encodeInfo (executableBuildInfo e)]

encodeTest :: Test -> Value
encodeTest t = object [
	"name" .= testName t,
	"enabled" .= testEnabled t,
	"info" .= encodeInfo (testBuildInfo t)]

encodeInfo :: Info -> Value
encodeInfo i = object [
	"source-dirs" .= infoSourceDirs i]
