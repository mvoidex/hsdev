{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module HsDev.Project.Types (
	Project(..), projectName, projectPath, projectCabal, projectDescription, project,
	ProjectDescription(..), projectVersion, projectLibrary, projectExecutables, projectTests,
	Target(..),
	Library(..), libraryModules, libraryBuildInfo,
	Executable(..), executableName, executablePath, executableBuildInfo,
	Test(..), testName, testEnabled, testBuildInfo,
	Info(..), infoDepends, infoLanguage, infoExtensions, infoGHCOptions, infoSourceDirs,
	Extensions(..), extensions, ghcOptions, entity,
	) where

import Control.Arrow
import Control.DeepSeq (NFData(..))
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Distribution.Text (display, simpleParse)
import qualified Distribution.Text (Text)
import Language.Haskell.Extension
import Text.Format
import System.FilePath

import System.Directory.Paths
import HsDev.Util

-- | Cabal project
data Project = Project {
	_projectName :: String,
	_projectPath :: FilePath,
	_projectCabal :: FilePath,
	_projectDescription :: Maybe ProjectDescription }
		deriving (Read)

instance NFData Project where
	rnf (Project n p c _) = rnf n `seq` rnf p `seq` rnf c

instance Eq Project where
	l == r = _projectCabal l == _projectCabal r

instance Ord Project where
	compare l r = compare (_projectName l, _projectCabal l) (_projectName r, _projectCabal r)

instance Show Project where
	show p = unlines $ [
		"project " ++ _projectName p,
		"\tcabal: " ++ _projectCabal p,
		"\tdescription:"] ++ concatMap (map (tab 2) . lines . show) (maybeToList $ _projectDescription p)

instance ToJSON Project where
	toJSON p = object [
		"name" .= _projectName p,
		"path" .= _projectPath p,
		"cabal" .= _projectCabal p,
		"description" .= _projectDescription p]

instance FromJSON Project where
	parseJSON = withObject "project" $ \v -> Project <$>
		v .:: "name" <*>
		v .:: "path" <*>
		v .:: "cabal" <*>
		v .:: "description"

instance Paths Project where
	paths f (Project nm p c desc) = Project nm <$> f p <*> f c <*> pure desc

-- | Make project by .cabal file
project :: FilePath -> Project
project file = Project {
	_projectName = takeBaseName (takeDirectory cabal),
	_projectPath = takeDirectory cabal,
	_projectCabal = cabal,
	_projectDescription = Nothing }
	where
		file' = dropTrailingPathSeparator $ normalise file
		cabal
			| takeExtension file' == ".cabal" = file'
			| otherwise = file' </> (takeBaseName file' <.> "cabal")

data ProjectDescription = ProjectDescription {
	_projectVersion :: String,
	_projectLibrary :: Maybe Library,
	_projectExecutables :: [Executable],
	_projectTests :: [Test] }
		deriving (Eq, Read)

instance Show ProjectDescription where
	show pd = unlines $
		concatMap (lines . show) (maybeToList (_projectLibrary pd)) ++
		concatMap (lines . show) (_projectExecutables pd) ++
		concatMap (lines . show) (_projectTests pd)

instance ToJSON ProjectDescription where
	toJSON d = object [
		"version" .= _projectVersion d,
		"library" .= _projectLibrary d,
		"executables" .= _projectExecutables d,
		"tests" .= _projectTests d]

instance FromJSON ProjectDescription where
	parseJSON = withObject "project description" $ \v -> ProjectDescription <$>
		v .:: "version" <*>
		v .:: "library" <*>
		v .:: "executables" <*>
		v .:: "tests"

class Target a where
	buildInfo :: a -> Info

-- | Library in project
data Library = Library {
	_libraryModules :: [[String]],
	_libraryBuildInfo :: Info }
		deriving (Eq, Read)

instance Target Library where
	buildInfo = _libraryBuildInfo

instance Show Library where
	show l = unlines $
		["library", "\tmodules:"] ++
		(map (tab 2 . intercalate ".") $ _libraryModules l) ++
		(map (tab 1) . lines . show $ _libraryBuildInfo l)

instance ToJSON Library where
	toJSON l = object [
		"modules" .= fmap (intercalate ".") (_libraryModules l),
		"info" .= _libraryBuildInfo l]

instance FromJSON Library where
	parseJSON = withObject "library" $ \v -> Library <$> (fmap splitModule <$> v .:: "modules") <*> v .:: "info" where
		splitModule :: String -> [String]
		splitModule = takeWhile (not . null) . unfoldr (Just . second (drop 1) . break (== '.'))

-- | Executable
data Executable = Executable {
	_executableName :: String,
	_executablePath :: FilePath,
	_executableBuildInfo :: Info }
		deriving (Eq, Read)

instance Target Executable where
	buildInfo = _executableBuildInfo

instance Show Executable where
	show e = unlines $
		["executable " ++ _executableName e, "\tpath: " ++ _executablePath e] ++
		(map (tab 1) . lines . show $ _executableBuildInfo e)

instance ToJSON Executable where
	toJSON e = object [
		"name" .= _executableName e,
		"path" .= _executablePath e,
		"info" .= _executableBuildInfo e]

instance FromJSON Executable where
	parseJSON = withObject "executable" $ \v -> Executable <$>
		v .:: "name" <*>
		v .:: "path" <*>
		v .:: "info"

-- | Test
data Test = Test {
	_testName :: String,
	_testEnabled :: Bool,
	_testBuildInfo :: Info }
		deriving (Eq, Read)

instance Target Test where
	buildInfo = _testBuildInfo

instance Show Test where
	show t = unlines $
		["test " ++ _testName t, "\tenabled: " ++ show (_testEnabled t)] ++
		(map (tab 1) . lines . show $ _testBuildInfo t)

instance ToJSON Test where
	toJSON t = object [
		"name" .= _testName t,
		"enabled" .= _testEnabled t,
		"info" .= _testBuildInfo t]

instance FromJSON Test where
	parseJSON = withObject "test" $ \v -> Test <$>
		v .:: "name" <*>
		v .:: "enabled" <*>
		v .:: "info"

-- | Build info
data Info = Info {
	_infoDepends :: [String],
	_infoLanguage :: Maybe Language,
	_infoExtensions :: [Extension],
	_infoGHCOptions :: [String],
	_infoSourceDirs :: [FilePath] }
		deriving (Eq, Read)

instance Monoid Info where
	mempty = Info [] Nothing [] [] []
	mappend l r = Info
		(ordNub $ _infoDepends l ++ _infoDepends r)
		(getFirst $ First (_infoLanguage l) `mappend` First (_infoLanguage r))
		(_infoExtensions l ++ _infoExtensions r)
		(_infoGHCOptions l ++ _infoGHCOptions r)
		(ordNub $ _infoSourceDirs l ++ _infoSourceDirs r)

instance Ord Info where
	compare l r = compare (_infoSourceDirs l, _infoDepends l, _infoGHCOptions l) (_infoSourceDirs r, _infoDepends r, _infoGHCOptions r)

instance Show Info where
	show i = unlines $ lang ++ exts ++ opts ++ sources where
		lang = maybe [] (\l -> ["default-language: " ++ display l]) $ _infoLanguage i
		exts
			| null (_infoExtensions i) = []
			| otherwise = "extensions:" : map (tab 1 . display) (_infoExtensions i)
		opts
			| null (_infoGHCOptions i) = []
			| otherwise = "ghc-options:" : map (tab 1) (_infoGHCOptions i)
		sources = "source-dirs:" : (map (tab 1) $ _infoSourceDirs i)

instance ToJSON Info where
	toJSON i = object [
		"build-depends" .= _infoDepends i,
		"language" .= fmap display (_infoLanguage i),
		"extensions" .= map display (_infoExtensions i),
		"ghc-options" .= _infoGHCOptions i,
		"source-dirs" .= _infoSourceDirs i]

instance FromJSON Info where
	parseJSON = withObject "info" $ \v -> Info <$>
		v .: "build-depends" <*>
		((v .:: "language") >>= traverse (parseDT "Language")) <*>
		((v .:: "extensions") >>= traverse (parseDT "Extension")) <*>
		v .:: "ghc-options" <*>
		v .:: "source-dirs"
		where
			parseDT :: Distribution.Text.Text a => String -> String -> Parser a
			parseDT typeName v = maybe err return (simpleParse v) where
				err = fail $ "Can't parse {}: {}" ~~ typeName ~~ v

-- | Entity with project extensions
data Extensions a = Extensions {
	_extensions :: [Extension],
	_ghcOptions :: [String],
	_entity :: a }
		deriving (Eq, Read, Show)

instance Ord a => Ord (Extensions a) where
	compare = comparing _entity

instance Functor Extensions where
	fmap f (Extensions e o x) = Extensions e o (f x)

instance Applicative Extensions where
	pure = Extensions [] []
	(Extensions l lo f) <*> (Extensions r ro x) = Extensions (ordNub $ l ++ r) (ordNub $ lo ++ ro) (f x)

instance Foldable Extensions where
	foldMap f (Extensions _ _ x) = f x

instance Traversable Extensions where
	traverse f (Extensions e o x) = Extensions e o <$> f x

makeLenses ''Project
makeLenses ''ProjectDescription
makeLenses ''Library
makeLenses ''Executable
makeLenses ''Test
makeLenses ''Info
makeLenses ''Extensions
