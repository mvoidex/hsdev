{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Project.JSON (
	) where

import Control.Applicative
import Control.Arrow (second)
import Data.Aeson
import Data.List (intercalate, unfoldr)

import HsDev.Util
import HsDev.Project

instance ToJSON Project where
	toJSON p = object [
		"name" .= projectName p,
		"path" .= projectPath p,
		"cabal" .= projectCabal p,
		"description" .= projectDescription p]

instance FromJSON Project where
	parseJSON = withObject "project" $ \v -> Project <$> v .:: "name" <*> v .:: "path" <*> v .:: "cabal" <*> v .:: "description"

instance ToJSON ProjectDescription where
	toJSON d = object [
		"library" .= projectLibrary d,
		"executables" .= projectExecutables d,
		"tests" .= projectTests d]

instance FromJSON ProjectDescription where
	parseJSON = withObject "project description" $ \v -> ProjectDescription <$> v .:: "library" <*> v .:: "executables" <*> v .:: "tests"

instance ToJSON Library where
	toJSON l = object [
		"modules" .= fmap (intercalate ".") (libraryModules l),
		"info" .= libraryBuildInfo l]

instance FromJSON Library where
	parseJSON = withObject "library" $ \v -> Library <$> (fmap splitModule <$> v .:: "modules") <*> v .:: "info" where
		splitModule :: String -> [String]
		splitModule = takeWhile (not . null) . unfoldr (Just . second (drop 1) . break (== '.'))

instance ToJSON Executable where
	toJSON e = object [
		"name" .= executableName e,
		"path" .= executablePath e,
		"info" .= executableBuildInfo e]

instance FromJSON Executable where
	parseJSON = withObject "executable" $ \v -> Executable <$> v .:: "name" <*> v .:: "path" <*> v .:: "info"

instance ToJSON Test where
	toJSON t = object [
		"name" .= testName t,
		"enabled" .= testEnabled t,
		"info" .= testBuildInfo t]

instance FromJSON Test where
	parseJSON = withObject "test" $ \v -> Test <$> v .:: "name" <*> v .:: "enabled" <*> v .:: "info"

instance ToJSON Info where
	toJSON i = object ["source-dirs" .= infoSourceDirs i]

instance FromJSON Info where
	parseJSON = withObject "info" $ \v -> Info <$> v .:: "source-dirs"
