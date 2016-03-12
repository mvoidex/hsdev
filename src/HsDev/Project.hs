{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module HsDev.Project (
	Project(..),
	ProjectDescription(..), Target(..), Library(..), Executable(..), Test(..), Info(..), infoSourceDirsDef,
	readProject, loadProject,
	project,
	Extensions(..), withExtensions,
	infos, inTarget, fileTargets, findSourceDir, sourceDirs,

	projectName, projectPath, projectCabal, projectDescription, projectLibrary, projectExecutables, projectTests,
	libraryModules, libraryBuildInfo,
	executableName, executablePath, executableBuildInfo,
	testName, testEnabled, testBuildInfo,
	infoDepends, infoLanguage, infoExtensions, infoGHCOptions, infoSourceDirs,
	extensions, ghcOptions, entity,

	-- * Helpers
	showExtension, flagExtension, extensionFlag,
	extensionsOpts
	) where

import Control.Arrow
import Control.DeepSeq (NFData(..))
import Control.Lens (makeLenses, Simple, Lens, view, lens)
import Control.Exception
import Control.Monad.Except
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.List
import Data.Maybe
import Data.Ord
import Distribution.Compiler (CompilerFlavor(GHC))
import qualified Distribution.Package as P
import qualified Distribution.PackageDescription as PD
import Distribution.PackageDescription.Parse
import Distribution.ModuleName (components)
import Distribution.Text (display, simpleParse)
import qualified Distribution.Text (Text)
import Language.Haskell.Extension
import System.FilePath

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

data ProjectDescription = ProjectDescription {
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
		"library" .= _projectLibrary d,
		"executables" .= _projectExecutables d,
		"tests" .= _projectTests d]

instance FromJSON ProjectDescription where
	parseJSON = withObject "project description" $ \v -> ProjectDescription <$>
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

-- | infoSourceDirs lens with default
infoSourceDirsDef :: Simple Lens Info [FilePath]
infoSourceDirsDef = lens get' set' where
	get' i = case _infoSourceDirs i of
		[] -> ["."]
		dirs -> dirs
	set' i ["."] = i { _infoSourceDirs = [] }
	set' i dirs = i { _infoSourceDirs = dirs }

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

-- | Analyze cabal file
analyzeCabal :: String -> Either String ProjectDescription
analyzeCabal source = case liftM flattenDescr $ parsePackageDescription source of
	ParseOk _ r -> Right ProjectDescription {
		_projectLibrary = fmap toLibrary $ PD.library r,
		_projectExecutables = fmap toExecutable $ PD.executables r,
		_projectTests = fmap toTest $ PD.testSuites r }
	ParseFailed e -> Left $ "Parse failed: " ++ show e
	where
		toLibrary (PD.Library exposeds _ _ _ _ info) = Library (map components exposeds) (toInfo info)
		toExecutable (PD.Executable name path info) = Executable name path (toInfo info)
		toTest (PD.TestSuite name _ info enabled) = Test name enabled (toInfo info)
		toInfo info = Info {
			_infoDepends = map pkgName (PD.targetBuildDepends info),
			_infoLanguage = PD.defaultLanguage info,
			_infoExtensions = PD.defaultExtensions info,
			_infoGHCOptions = fromMaybe [] $ lookup GHC (PD.options info),
			_infoSourceDirs = PD.hsSourceDirs info }

		pkgName :: P.Dependency -> String
		pkgName (P.Dependency (P.PackageName s) _) = s

		flattenDescr :: PD.GenericPackageDescription -> PD.PackageDescription
		flattenDescr (PD.GenericPackageDescription pkg _ mlib mexes mtests _) = pkg {
			PD.library = flip fmap mlib $ flattenTree
				(insertInfo PD.libBuildInfo (\i l -> l { PD.libBuildInfo = i })),
			PD.executables = flip fmap mexes $
				second (flattenTree (insertInfo PD.buildInfo (\i l -> l { PD.buildInfo = i }))) >>>
				(\(n, e) -> e { PD.exeName = n }),
			PD.testSuites = flip fmap mtests $
				second (flattenTree (insertInfo PD.testBuildInfo (\i l -> l { PD.testBuildInfo = i }))) >>>
				(\(n, t) -> t { PD.testName = n }) }
			where
				insertInfo :: (a -> PD.BuildInfo) -> (PD.BuildInfo -> a -> a) -> [P.Dependency] -> a -> a
				insertInfo f s deps x = s ((f x) { PD.targetBuildDepends = deps }) x

		flattenTree :: Monoid a => (c -> a -> a) -> PD.CondTree v c a -> a
		flattenTree f (PD.CondNode x cs cmps) = f cs x `mappend` mconcat (concatMap flattenBranch cmps) where
			flattenBranch (_, t, mb) = flattenTree f t : map (flattenTree f) (maybeToList mb)

-- | Read project info from .cabal
readProject :: FilePath -> ExceptT String IO Project
readProject file = do
	source <- ExceptT $ handle (\e -> return (Left ("IO error: " ++ show (e :: IOException)))) (fmap Right $ readFile file)
	length source `seq` either throwError (return . mkProject) $ analyzeCabal source
	where
		mkProject desc = (project file) {
			_projectDescription = Just desc }

-- | Load project description
loadProject :: Project -> ExceptT String IO Project
loadProject p
	| isJust (_projectDescription p) = return p
	| otherwise = readProject (_projectCabal p)

-- | Make project by .cabal file
project :: FilePath -> Project
project file
	| takeExtension file == ".cabal" = Project {
		_projectName = takeBaseName (takeDirectory file),
		_projectPath = takeDirectory file,
		_projectCabal = file,
		_projectDescription = Nothing }
	| otherwise = Project {
		_projectName = takeBaseName file,
		_projectPath = file,
		_projectCabal = file </> (takeBaseName file <.> "cabal"),
		_projectDescription = Nothing }

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

-- | Extensions for target
withExtensions :: a -> Info -> Extensions a
withExtensions x i = Extensions {
	_extensions = _infoExtensions i,
	_ghcOptions = _infoGHCOptions i,
	_entity = x }

-- | Returns build targets infos
infos :: ProjectDescription -> [Info]
infos p =
	maybe [] (return . _libraryBuildInfo) (_projectLibrary p) ++
	map _executableBuildInfo (_projectExecutables p) ++
	map _testBuildInfo (_projectTests p)

-- | Check if source related to target, source must be relative to project directory
inTarget :: FilePath -> Info -> Bool
inTarget src info = any ((`isPrefixOf` normalise src) . normalise) $ view infoSourceDirsDef info

-- | Get possible targets for source file
-- There can be many candidates in case of module related to several executables or tests
fileTargets :: Project -> FilePath -> [Info]
fileTargets p f = case filter ((`isSuffixOf` f') . normalise . _executablePath) exes of
	[] -> filter (f' `inTarget`) $ maybe [] infos $ _projectDescription p
	exes' -> map _executableBuildInfo exes'
	where
		f' = makeRelative (_projectPath p) f
		exes = maybe [] _projectExecutables $ _projectDescription p

-- | Finds source dir file belongs to
findSourceDir :: Project -> FilePath -> Maybe (Extensions FilePath)
findSourceDir p f = do
	info <- listToMaybe $ fileTargets p f
	fmap (`withExtensions` info) $ listToMaybe $ filter (`isParent` f) $ map (_projectPath p </>) $ view infoSourceDirsDef info

-- | Returns source dirs for library, executables and tests
sourceDirs :: ProjectDescription -> [Extensions FilePath]
sourceDirs = ordNub . concatMap dirs . infos where
	dirs i = map (`withExtensions` i) $ view infoSourceDirsDef i

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
extensionsOpts :: Extensions a -> [String]
extensionsOpts e = map (extensionFlag . showExtension) (_extensions e) ++ _ghcOptions e

makeLenses ''Project
makeLenses ''ProjectDescription
makeLenses ''Library
makeLenses ''Executable
makeLenses ''Test
makeLenses ''Info
makeLenses ''Extensions
