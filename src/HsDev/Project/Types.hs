{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Project.Types (
	Project(..), projectName, projectPath, projectCabal, projectDescription, project,
	ProjectDescription(..), projectVersion, projectLibrary, projectExecutables, projectTests, infos,
	Target(..),
	Library(..), libraryModules, libraryBuildInfo,
	Executable(..), executableName, executablePath, executableBuildInfo,
	Test(..), testName, testEnabled, testBuildInfo, testMain,
	Info(..), infoDepends, infoLanguage, infoExtensions, infoGHCOptions, infoSourceDirs, infoOtherModules,
	Extensions(..), extensions, ghcOptions, entity,
	) where

import Control.DeepSeq (NFData(..))
import Control.Lens hiding ((.=), (<.>))
import Data.Aeson
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import Distribution.Text (display)
import Language.Haskell.Extension
import System.FilePath

import System.Directory.Paths
import HsDev.Util

-- | Cabal project
data Project = Project {
	_projectName :: Text,
	_projectPath :: Path,
	_projectCabal :: Path,
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
		"project " ++ _projectName p ^. path,
		"\tcabal: " ++ _projectCabal p ^. path,
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
	paths f (Project nm p c desc) = Project nm <$> paths f p <*> paths f c <*> traverse (paths f) desc

-- | Make project by .cabal file
project :: FilePath -> Project
project file = Project {
	_projectName = fromFilePath . takeBaseName . takeDirectory $ cabal,
	_projectPath = fromFilePath . takeDirectory $ cabal,
	_projectCabal = fromFilePath cabal,
	_projectDescription = Nothing }
	where
		file' = dropTrailingPathSeparator $ normalise file
		cabal
			| takeExtension file' == ".cabal" = file'
			| otherwise = file' </> (takeBaseName file' <.> "cabal")

data ProjectDescription = ProjectDescription {
	_projectVersion :: Text,
	_projectLibrary :: Maybe Library,
	_projectExecutables :: [Executable],
	_projectTests :: [Test] }
		deriving (Eq, Read)

-- | Build target infos
infos :: Traversal' ProjectDescription Info
infos f desc = (\lib exes tests -> desc { _projectLibrary = lib, _projectExecutables = exes, _projectTests = tests }) <$>
	(_Just . buildInfo) f (_projectLibrary desc) <*>
	(each . buildInfo) f (_projectExecutables desc) <*>
	(each . buildInfo) f (_projectTests desc)

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

instance Paths ProjectDescription where
	paths f (ProjectDescription v lib exes tests) = ProjectDescription v <$> traverse (paths f) lib <*> traverse (paths f) exes <*> traverse (paths f) tests

class Target a where
	targetName :: Traversal' a Text
	buildInfo :: Lens' a Info
	targetMain :: a -> Maybe Path
	targetModules :: a -> [[Text]]

-- | Library in project
data Library = Library {
	_libraryModules :: [[Text]],
	_libraryBuildInfo :: Info }
		deriving (Eq, Read)

instance Show Library where
	show l = unlines $
		["library", "\tmodules:"] ++
		(map (tab 2 . T.unpack . T.intercalate ".") $ _libraryModules l) ++
		(map (tab 1) . lines . show $ _libraryBuildInfo l)

instance ToJSON Library where
	toJSON l = object [
		"modules" .= fmap (T.intercalate ".") (_libraryModules l),
		"info" .= _libraryBuildInfo l]

instance FromJSON Library where
	parseJSON = withObject "library" $ \v -> Library <$> (fmap (T.split (== '.')) <$> v .:: "modules") <*> v .:: "info" where

instance Paths Library where
	paths f (Library ms info) = Library ms <$> paths f info

-- | Executable
data Executable = Executable {
	_executableName :: Text,
	_executablePath :: Path,
	_executableBuildInfo :: Info }
		deriving (Eq, Read)

instance Show Executable where
	show e = unlines $
		["executable " ++ T.unpack (_executableName e), "\tpath: " ++ (_executablePath e ^. path)] ++
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

instance Paths Executable where
	paths f (Executable n p info) = Executable n <$> paths f p <*> paths f info

-- | Test
data Test = Test {
	_testName :: Text,
	_testEnabled :: Bool,
	_testMain :: Maybe Path,
	_testBuildInfo :: Info }
		deriving (Eq, Read)

instance Show Test where
	show t = unlines $
		["test " ++ T.unpack (_testName t), "\tenabled: " ++ show (_testEnabled t)] ++
		maybe [] (\f -> ["\tmain-is: " ++ f ^. path]) (_testMain t) ++
		(map (tab 1) . lines . show $ _testBuildInfo t)

instance ToJSON Test where
	toJSON t = object [
		"name" .= _testName t,
		"enabled" .= _testEnabled t,
		"main" .= _testMain t,
		"info" .= _testBuildInfo t]

instance FromJSON Test where
	parseJSON = withObject "test" $ \v -> Test <$>
		v .:: "name" <*>
		v .:: "enabled" <*>
		v .::? "main" <*>
		v .:: "info"

instance Paths Test where
	paths f (Test n e m info) = Test n e <$> traverse (paths f) m <*> paths f info

-- | Build info
data Info = Info {
	_infoDepends :: [Text],
	_infoLanguage :: Maybe Language,
	_infoExtensions :: [Extension],
	_infoGHCOptions :: [Text],
	_infoSourceDirs :: [Path],
	_infoOtherModules :: [[Text]] }
		deriving (Eq, Read)

instance Monoid Info where
	mempty = Info [] Nothing [] [] [] []
	mappend l r = Info
		(ordNub $ _infoDepends l ++ _infoDepends r)
		(getFirst $ First (_infoLanguage l) `mappend` First (_infoLanguage r))
		(_infoExtensions l ++ _infoExtensions r)
		(_infoGHCOptions l ++ _infoGHCOptions r)
		(ordNub $ _infoSourceDirs l ++ _infoSourceDirs r)
		(ordNub $ _infoOtherModules l ++ _infoOtherModules r)

instance Ord Info where
	compare l r = compare (_infoSourceDirs l, _infoDepends l, _infoGHCOptions l) (_infoSourceDirs r, _infoDepends r, _infoGHCOptions r)

instance Show Info where
	show i = unlines $ lang ++ exts ++ opts ++ sources ++ otherMods where
		lang = maybe [] (\l -> ["default-language: " ++ display l]) $ _infoLanguage i
		exts
			| null (_infoExtensions i) = []
			| otherwise = "extensions:" : map (tab 1 . display) (_infoExtensions i)
		opts
			| null (_infoGHCOptions i) = []
			| otherwise = "ghc-options:" : map (tab 1 . T.unpack) (_infoGHCOptions i)
		sources = "source-dirs:" : map (tab 1 . T.unpack) (_infoSourceDirs i)
		otherMods = "other-modules:" : (map (tab 1 . T.unpack) . fmap (T.intercalate ".") $ _infoOtherModules i)

instance ToJSON Info where
	toJSON i = object [
		"build-depends" .= _infoDepends i,
		"language" .= _infoLanguage i,
		"extensions" .= _infoExtensions i,
		"ghc-options" .= _infoGHCOptions i,
		"source-dirs" .= _infoSourceDirs i,
		"other-modules" .= _infoOtherModules i]

instance FromJSON Info where
	parseJSON = withObject "info" $ \v -> Info <$>
		v .: "build-depends" <*>
		v .:: "language" <*>
		v .:: "extensions" <*>
		v .:: "ghc-options" <*>
		v .:: "source-dirs" <*>
		v .:: "other-modules"

instance ToJSON Language where
	toJSON = toJSON . display

instance FromJSON Language where
	parseJSON = withText "language" $ \txt -> parseDT "Language" (T.unpack txt)

instance ToJSON Extension where
	toJSON = toJSON . display

instance FromJSON Extension where
	parseJSON = withText "extension" $ \txt -> parseDT "Extension" (T.unpack txt)

instance Paths Info where
	paths f (Info deps lang exts opts dirs omods) = Info deps lang exts opts <$> traverse (paths f) dirs <*> pure omods

-- | Entity with project extensions
data Extensions a = Extensions {
	_extensions :: [Extension],
	_ghcOptions :: [Text],
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

instance Target Library where
	targetName _ = pure
	buildInfo = libraryBuildInfo
	targetMain _ = Nothing
	targetModules lib' = lib' ^.. libraryModules . each

instance Target Executable where
	targetName = executableName
	buildInfo = executableBuildInfo
	targetMain exe' = Just $ exe' ^. executablePath
	targetModules _ = []

instance Target Test where
	targetName = testName
	buildInfo = testBuildInfo
	targetMain test' = fmap toPath (test' ^? testMain . _Just . path) where
		toPath f
			| haskellSource f = fromFilePath f
			| otherwise = fromFilePath (f <.> "hs")
	targetModules _ = []
