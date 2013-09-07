{-# LANGUAGE OverloadedStrings #-}

module HsDev.Project (
	Project(..),
	ProjectDescription(..), Library(..), Executable(..), Test(..), Info(..),
	readProject, loadProject,
	project,
	sourceDirs
	) where

import Control.Applicative
import Control.Arrow
import Control.DeepSeq
import Control.Exception (handle, IOException)
import Control.Monad.Error
import Data.Aeson
import Data.List (intercalate, unfoldr)
import Data.Maybe (maybeToList, isJust)
import qualified Distribution.PackageDescription as PD
import Distribution.PackageDescription.Parse
import Distribution.ModuleName (components)
import System.FilePath

import HsDev.Util

-- | Cabal project
data Project = Project {
	projectName :: String,
	projectPath :: FilePath,
	projectCabal :: FilePath,
	projectDescription :: Maybe ProjectDescription }
		deriving (Read)

instance NFData Project where
	rnf (Project n p c _) = rnf n `seq` rnf p `seq` rnf c

instance Eq Project where
	l == r = projectCabal l == projectCabal r

instance Ord Project where
	compare l r = compare (projectName l, projectCabal l) (projectName r, projectCabal r)

instance Show Project where
	show p = unlines $ [
		"project " ++ projectName p,
		"\tcabal: " ++ projectCabal p,
		"\tdescription:"] ++ concatMap (map (tab 2) . lines . show) (maybeToList $ projectDescription p)

instance ToJSON Project where
	toJSON p = object [
		"name" .= projectName p,
		"path" .= projectPath p,
		"cabal" .= projectCabal p,
		"description" .= projectDescription p]

instance FromJSON Project where
	parseJSON = withObject "project" $ \v -> Project <$>
		v .:: "name" <*>
		v .:: "path" <*>
		v .:: "cabal" <*>
		v .:: "description"

data ProjectDescription = ProjectDescription {
	projectLibrary :: Maybe Library,
	projectExecutables :: [Executable],
	projectTests :: [Test] }
		deriving (Eq, Ord, Read)

instance Show ProjectDescription where
	show pd = unlines $
		concatMap (lines . show) (maybeToList (projectLibrary pd)) ++
		concatMap (lines . show) (projectExecutables pd) ++
		concatMap (lines . show) (projectTests pd)

instance ToJSON ProjectDescription where
	toJSON d = object [
		"library" .= projectLibrary d,
		"executables" .= projectExecutables d,
		"tests" .= projectTests d]

instance FromJSON ProjectDescription where
	parseJSON = withObject "project description" $ \v -> ProjectDescription <$>
		v .:: "library" <*>
		v .:: "executables" <*>
		v .:: "tests"

-- | Library in project
data Library = Library {
	libraryModules :: [[String]],
	libraryBuildInfo :: Info }
		deriving (Eq, Ord, Read)

instance Show Library where
	show l = unlines $
		["library", "\tmodules:"] ++
		(map (tab 2 . intercalate ".") $ libraryModules l) ++
		(map (tab 1) . lines . show $ libraryBuildInfo l)

instance ToJSON Library where
	toJSON l = object [
		"modules" .= fmap (intercalate ".") (libraryModules l),
		"info" .= libraryBuildInfo l]

instance FromJSON Library where
	parseJSON = withObject "library" $ \v -> Library <$> (fmap splitModule <$> v .:: "modules") <*> v .:: "info" where
		splitModule :: String -> [String]
		splitModule = takeWhile (not . null) . unfoldr (Just . second (drop 1) . break (== '.'))

-- | Executable
data Executable = Executable {
	executableName :: String,
	executablePath :: FilePath,
	executableBuildInfo :: Info }
		deriving (Eq, Ord, Read)

instance Show Executable where
	show e = unlines $
		["executable " ++ executableName e, "\tpath: " ++ executablePath e] ++
		(map (tab 1) . lines . show $ executableBuildInfo e)

instance ToJSON Executable where
	toJSON e = object [
		"name" .= executableName e,
		"path" .= executablePath e,
		"info" .= executableBuildInfo e]

instance FromJSON Executable where
	parseJSON = withObject "executable" $ \v -> Executable <$>
		v .:: "name" <*>
		v .:: "path" <*>
		v .:: "info"

-- | Test
data Test = Test {
	testName :: String,
	testEnabled :: Bool,
	testBuildInfo :: Info }
		deriving (Eq, Ord, Read)

instance Show Test where
	show t = unlines $
		["test " ++ testName t, "\tenabled: " ++ show (testEnabled t)] ++
		(map (tab 1) . lines . show $ testBuildInfo t)

instance ToJSON Test where
	toJSON t = object [
		"name" .= testName t,
		"enabled" .= testEnabled t,
		"info" .= testBuildInfo t]

instance FromJSON Test where
	parseJSON = withObject "test" $ \v -> Test <$>
		v .:: "name" <*>
		v .:: "enabled" <*>
		v .:: "info"

-- | Build info
data Info = Info {
	infoSourceDirs :: [FilePath] }
		deriving (Eq, Ord, Read)

instance Show Info where
	show i = unlines $
		["source-dirs:"] ++
		(map (tab 1) $ infoSourceDirs i)

instance ToJSON Info where
	toJSON i = object ["source-dirs" .= infoSourceDirs i]

instance FromJSON Info where
	parseJSON = withObject "info" $ \v -> Info <$> v .:: "source-dirs"

-- | Analyze cabal file
analyzeCabal :: String -> Either String ProjectDescription
analyzeCabal source = case parsePackageDescription source of
	ParseOk _ r -> Right ProjectDescription {
		projectLibrary = fmap (toLibrary . PD.condTreeData) $ PD.condLibrary r,
		projectExecutables = fmap (toExecutable . second PD.condTreeData) $ PD.condExecutables r,
		projectTests = fmap (toTest . second PD.condTreeData) $ PD.condTestSuites r }
	ParseFailed e -> Left $ "Parse failed: " ++ show e
	where
		toLibrary (PD.Library exposeds _ info) = Library (map components exposeds) (toInfo info)
		toExecutable (name, PD.Executable _ path info) = Executable name path (toInfo info)
		toTest (name, PD.TestSuite _ _ info enabled) = Test name enabled (toInfo info)
		toInfo info = Info { infoSourceDirs = PD.hsSourceDirs info }

-- | Read project info from .cabal
readProject :: FilePath -> ErrorT String IO Project
readProject file = do
	source <- ErrorT $ handle (\e -> return (Left ("IO error: " ++ show (e :: IOException)))) (fmap Right $ readFile file)
	length source `seq` either throwError (return . mkProject) $ analyzeCabal source
	where
		mkProject desc = (project file) {
			projectDescription = Just desc }

-- | Load project description
loadProject :: Project -> ErrorT String IO Project
loadProject p
	| isJust (projectDescription p) = return p
	| otherwise = readProject (projectCabal p)

-- | Make project by .cabal file
project :: FilePath -> Project
project file = Project {
	projectName = takeBaseName (takeDirectory file),
	projectPath = takeDirectory file,
	projectCabal = file,
	projectDescription = Nothing }

-- | Returns source dirs for library, executables and tests
sourceDirs :: ProjectDescription -> [FilePath]
sourceDirs p = concatMap infoSourceDirs infos where
	infos = maybe [] (return . libraryBuildInfo) (projectLibrary p) ++ map executableBuildInfo (projectExecutables p) ++ map testBuildInfo (projectTests p)
