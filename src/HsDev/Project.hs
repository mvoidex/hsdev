module HsDev.Project (
	Project(..),
	ProjectDescription(..), Library(..), Executable(..), Test(..),
	readProject,
	projectByCabal,
	sourceDirs
	) where

import Control.Arrow
import Control.Monad.Error
import qualified Distribution.PackageDescription as PD
import Distribution.PackageDescription.Parse
import Distribution.ModuleName (components)
import System.FilePath

-- | Cabal project
data Project = Project {
	projectName :: String,
	projectPath :: FilePath,
	projectCabal :: FilePath,
	projectDescription :: Maybe ProjectDescription }
		deriving (Eq, Ord, Read, Show)

data ProjectDescription = ProjectDescription {
	projectLibrary :: Maybe Library,
	projectExecutables :: [Executable],
	projectTests :: [Test] }
		deriving (Eq, Ord, Read, Show)

-- | Library in project
data Library = Library {
	libraryModules :: [[String]],
	libraryBuildInfo :: Info }
		deriving (Eq, Ord, Read, Show)

-- | Executable
data Executable = Executable {
	executableName :: String,
	executablePath :: FilePath,
	executableBuildInfo :: Info }
		deriving (Eq, Ord, Read, Show)

-- | Test
data Test = Test {
	testName :: String,
	testEnabled :: Bool,
	testBuildInfo :: Info }
		deriving (Eq, Ord, Read, Show)

-- | Build info
data Info = Info {
	infoSourceDirs :: [FilePath] }
		deriving (Eq, Ord, Read, Show)

-- | Analyze cabal file
analyzeCabal :: String -> Either String ProjectDescription
analyzeCabal source = case parsePackageDescription source of
	ParseOk _ r -> Right $ ProjectDescription {
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
	source <- liftIO $ readFile file
	either throwError (return . mkProject) $ analyzeCabal source
	where
		mkProject desc = (projectByCabal file) {
			projectDescription = Just desc }

-- | Make project by .cabal file
projectByCabal :: FilePath -> Project
projectByCabal file = Project {
	projectName = takeBaseName (takeDirectory file),
	projectPath = takeDirectory file,
	projectCabal = file,
	projectDescription = Nothing }

-- | Returns source dirs for library, executables and tests
sourceDirs :: ProjectDescription -> [FilePath]
sourceDirs p = concatMap infoSourceDirs infos where
	infos = maybe [] (return . libraryBuildInfo) (projectLibrary p) ++ map executableBuildInfo (projectExecutables p) ++ map testBuildInfo (projectTests p)
