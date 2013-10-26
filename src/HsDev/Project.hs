{-# LANGUAGE OverloadedStrings #-}

module HsDev.Project (
	Project(..),
	ProjectDescription(..), Library(..), Executable(..), Test(..), Info(..),
	readProject, loadProject,
	project,
	Extensions(..), withExtensions,
	infos, sourceDirs,

	-- * Helpers
	showExtension, flagExtension, extensionFlag,
	extensionsOpts
	) where

import Control.Applicative
import Control.Arrow
import Control.DeepSeq
import Control.Exception (handle, IOException)
import Control.Monad.Error
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.List (intercalate, unfoldr, stripPrefix)
import Data.Maybe (maybeToList, isJust)
import Data.Foldable (Foldable(..))
import Data.Traversable
import Data.Text (unpack)
import qualified Distribution.PackageDescription as PD
import Distribution.PackageDescription.Parse
import Distribution.ModuleName (components)
import Distribution.Text (display, simpleParse)
import qualified Distribution.Text
import Language.Haskell.Extension
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
		deriving (Eq, Read)

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
		deriving (Eq, Read)

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
		deriving (Eq, Read)

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
		deriving (Eq, Read)

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
	infoLanguage :: Maybe Language,
	infoExtensions :: [Extension],
	infoSourceDirs :: [FilePath] }
		deriving (Eq, Read)

instance Show Info where
	show i = unlines $ lang ++ exts ++ sources where
		lang = maybe [] (\l -> ["default-language: " ++ display l]) $ infoLanguage i
		exts
			| null (infoExtensions i) = []
			| otherwise = ["extensions:"] ++ map (tab 1) (map display (infoExtensions i))
		sources = ["source-dirs:"] ++
			(map (tab 1) $ infoSourceDirs i)

instance ToJSON Info where
	toJSON i = object [
		"language" .= fmap display (infoLanguage i),
		"extensions" .= map display (infoExtensions i),
		"source-dirs" .= infoSourceDirs i]

instance FromJSON Info where
	parseJSON = withObject "info" $ \v -> Info <$>
		((v .:: "language") >>= traverse (parseDT "Language")) <*>
		((v .:: "extensions") >>= traverse (parseDT "Extension")) <*>
		v .:: "source-dirs"

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
		toInfo info = Info {
			infoLanguage = PD.defaultLanguage info,
			infoExtensions = PD.defaultExtensions info,
			infoSourceDirs = PD.hsSourceDirs info }

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

-- | Entity with project extensions
data Extensions a = Extensions {
	extensions :: [Extension],
	entity :: a }

instance Functor Extensions where
	fmap f (Extensions e x) = Extensions e (f x)

instance Applicative Extensions where
	pure = Extensions []
	(Extensions l f) <*> (Extensions r x) = Extensions (l ++ r) (f x)

instance Foldable Extensions where
	foldMap f (Extensions _ x) = f x

instance Traversable Extensions where
	traverse f (Extensions e x) = Extensions e <$> f x

-- | Extensions for target
withExtensions :: a -> Info -> Extensions a
withExtensions x i = Extensions {
	extensions = infoExtensions i,
	entity = x }

-- | Returns build targets infos
infos :: ProjectDescription -> [Info]
infos p =
	maybe [] (return . libraryBuildInfo) (projectLibrary p) ++
	map executableBuildInfo (projectExecutables p) ++
	map testBuildInfo (projectTests p)

-- | Returns source dirs for library, executables and tests
sourceDirs :: ProjectDescription -> [Extensions FilePath]
sourceDirs = concatMap dirs . infos where
	dirs i = map (`withExtensions` i) $ infoSourceDirs i

parseDT :: Distribution.Text.Text a => String -> String -> Parser a
parseDT typeName v = maybe err return (simpleParse v) where
	err = fail $ "Can't parse " ++ typeName ++ ": " ++ v

-- | Extension as flag name
showExtension :: Extension -> String
showExtension = display

-- | Convert -Xext to ext
flagExtension :: String -> Maybe String
flagExtension = stripPrefix "-X"

-- | Convert ext to -Xext
extensionFlag :: String -> String
extensionFlag = ("-X" ++)

-- | Extensions as opts to GHC
extensionsOpts :: [Extension] -> [String]
extensionsOpts = map (extensionFlag . showExtension)
