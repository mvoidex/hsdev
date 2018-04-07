{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeApplications, LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Project.Types (
	BuildTool(..), Sandbox(..), sandboxType, sandbox,
	Project(..), projectName, projectPath, projectCabal, projectDescription, projectBuildTool, projectPackageDbStack, project,
	ProjectDescription(..), projectVersion, projectLibrary, projectExecutables, projectTests, infos, targetInfos,
	Target(..), TargetInfo(..), targetInfoName, targetBuildInfo, targetInfoMain, targetInfoModules, targetInfo,
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
import qualified Distribution.Text as D (display)
import Language.Haskell.Extension
import System.FilePath
import Text.Format

import System.Directory.Paths
import HsDev.Display
import HsDev.PackageDb.Types
import HsDev.Util

-- | Project build tool
data BuildTool = CabalTool | StackTool deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance NFData BuildTool where
	rnf CabalTool = ()
	rnf StackTool = ()

instance Display BuildTool where
	display CabalTool = "cabal"
	display StackTool = "stack"
	displayType _ = "build-tool"

instance Formattable BuildTool where
	formattable = formattable . display

instance ToJSON BuildTool where
	toJSON CabalTool = toJSON @String "cabal"
	toJSON StackTool = toJSON @String "stack"

instance FromJSON BuildTool where
	parseJSON = withText "build-tool" $ \case
		"cabal" -> return CabalTool
		"stack" -> return StackTool
		other -> fail $ "Can't parse BuildTool, unknown tool: {}" ~~ other

data Sandbox = Sandbox { _sandboxType :: BuildTool, _sandbox :: Path } deriving (Eq, Ord)

makeLenses ''Sandbox

instance NFData Sandbox where
	rnf (Sandbox t p) = rnf t `seq` rnf p

instance Show Sandbox where
	show (Sandbox _ p) = T.unpack p

instance Display Sandbox where
	display (Sandbox _ fpath) = display fpath
	displayType (Sandbox CabalTool _) = "cabal-sandbox"
	displayType (Sandbox StackTool _) = "stack-work"

instance Formattable Sandbox where
	formattable = formattable . display

instance ToJSON Sandbox where
	toJSON (Sandbox t p) = object ["type" .= t, "path" .= p]

instance FromJSON Sandbox where
	parseJSON = withObject "sandbox" $ \v -> Sandbox <$>
		v .:: "type" <*>
		v .:: "path"

instance Paths Sandbox where
	paths f (Sandbox st p) = Sandbox st <$> paths f p

-- | Cabal project
data Project = Project {
	_projectName :: Text,
	_projectPath :: Path,
	_projectCabal :: Path,
	_projectDescription :: Maybe ProjectDescription,
	_projectBuildTool :: BuildTool,
	_projectPackageDbStack :: Maybe PackageDbStack }

instance NFData Project where
	rnf (Project n p c _ t dbs) = rnf n `seq` rnf p `seq` rnf c `seq` rnf t `seq` rnf dbs

instance Eq Project where
	l == r = _projectCabal l == _projectCabal r

instance Ord Project where
	compare l r = compare (_projectName l, _projectCabal l) (_projectName r, _projectCabal r)

instance Show Project where
	show p = unlines $ [
		"project " ++ _projectName p ^. path,
		"\tcabal: " ++ _projectCabal p ^. path,
		"\tdescription:"] ++ concatMap (map (tab 2) . lines . show) (maybeToList $ _projectDescription p)

instance Display Project where
	display = T.unpack . _projectName
	displayType _ = "project"

instance Formattable Project where
	formattable = formattable . display

instance ToJSON Project where
	toJSON p = object [
		"name" .= _projectName p,
		"path" .= _projectPath p,
		"cabal" .= _projectCabal p,
		"description" .= _projectDescription p,
		"build-tool" .= _projectBuildTool p,
		"package-db-stack" .= _projectPackageDbStack p]

instance FromJSON Project where
	parseJSON = withObject "project" $ \v -> Project <$>
		v .:: "name" <*>
		v .:: "path" <*>
		v .:: "cabal" <*>
		v .:: "description" <*>
		v .:: "build-tool" <*>
		v .::? "package-db-stack"

instance Paths Project where
	paths f (Project nm p c desc t dbs) = Project nm <$> paths f p <*> paths f c <*> traverse (paths f) desc <*> pure t <*> pure dbs

-- | Make project by .cabal file
project :: FilePath -> Project
project file = Project {
	-- Should not be the directory of the cabal, s/b the base name of the cabal file.
	_projectName = fromFilePath . dropExtension . takeBaseName $ cabal,
	_projectPath = fromFilePath . takeDirectory $ cabal,
	_projectCabal = fromFilePath cabal,
	_projectDescription = Nothing,
	_projectBuildTool = CabalTool,
	_projectPackageDbStack = Nothing }
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

-- | Build target infos, more detailed
targetInfos :: ProjectDescription -> [TargetInfo]
targetInfos desc = concat [
	map targetInfo $ maybeToList (_projectLibrary desc),
	map targetInfo $ _projectExecutables desc,
	map targetInfo $ _projectTests desc]

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

data TargetInfo = TargetInfo {
	_targetInfoName :: Maybe Text,
	_targetBuildInfo :: Info,
	_targetInfoMain :: Maybe Path,
	_targetInfoModules :: [[Text]] }
		deriving (Eq, Ord, Show)

targetInfo :: Target a => a -> TargetInfo
targetInfo t = TargetInfo (t ^? targetName) (t ^. buildInfo) (targetMain t) (targetModules t)

instance Paths TargetInfo where
	paths f (TargetInfo n i mp ms) = TargetInfo n <$> paths f i <*> traverse (paths f) mp <*> pure ms

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
	parseJSON = withObject "library" $ \v -> Library <$> (fmap (T.split (== '.')) <$> v .:: "modules") <*> v .:: "info"

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
		lang = maybe [] (\l -> ["default-language: " ++ D.display l]) $ _infoLanguage i
		exts
			| null (_infoExtensions i) = []
			| otherwise = "extensions:" : map (tab 1 . D.display) (_infoExtensions i)
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
	toJSON = toJSON . D.display

instance FromJSON Language where
	parseJSON = withText "language" $ \txt -> parseDT "Language" (T.unpack txt)

instance ToJSON Extension where
	toJSON = toJSON . D.display

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
makeLenses ''TargetInfo
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

instance Target TargetInfo where
	targetName = targetInfoName . _Just
	buildInfo = targetBuildInfo
	targetMain = _targetInfoMain
	targetModules = _targetInfoModules
