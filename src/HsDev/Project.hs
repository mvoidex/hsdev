{-# LANGUAGE CPP #-}

module HsDev.Project (
	module HsDev.Project.Types,

	infoSourceDirsDef,
	analyzeCabal,
	readProject, loadProject,
	withExtensions,
	infos, inTarget, fileTarget, fileTargets, findSourceDir, sourceDirs,
	targetOpts,

	-- * Helpers
	showExtension, flagExtension, extensionFlag,
	extensionsOpts
	) where

import Control.Arrow
import Control.Lens (Simple, Lens, view, lens)
import Control.Monad.Except
import Data.List
import Data.Maybe
import Distribution.Compiler (CompilerFlavor(GHC))
import qualified Distribution.Package as P
import qualified Distribution.PackageDescription as PD
import Distribution.PackageDescription.Parse
import Distribution.ModuleName (components)
import Distribution.Text (display)
import Language.Haskell.Extension
import System.FilePath

import HsDev.Project.Compat
import HsDev.Project.Types
import HsDev.Error
import HsDev.Util

-- | infoSourceDirs lens with default
infoSourceDirsDef :: Simple Lens Info [FilePath]
infoSourceDirsDef = lens get' set' where
	get' i = case _infoSourceDirs i of
		[] -> ["."]
		dirs -> dirs
	set' i ["."] = i { _infoSourceDirs = [] }
	set' i dirs = i { _infoSourceDirs = dirs }

-- | Analyze cabal file
analyzeCabal :: String -> Either String ProjectDescription
analyzeCabal source = case liftM flattenDescr $ parsePackageDesc source of
	ParseOk _ r -> Right ProjectDescription {
		_projectVersion = showVer $ P.pkgVersion $ PD.package r,
		_projectLibrary = fmap toLibrary $ PD.library r,
		_projectExecutables = fmap toExecutable $ PD.executables r,
		_projectTests = fmap toTest $ PD.testSuites r }
	ParseFailed e -> Left $ "Parse failed: " ++ show e
	where
		toLibrary lib = Library (map components $ PD.exposedModules lib) (toInfo $ PD.libBuildInfo lib)
		toExecutable exe = Executable (componentName $ PD.exeName exe) (PD.modulePath exe) (toInfo $ PD.buildInfo exe)
		toTest test = Test (componentName $ PD.testName test) (testSuiteEnabled test) (toInfo $ PD.testBuildInfo test)
		toInfo info = Info {
			_infoDepends = map pkgName (PD.targetBuildDepends info),
			_infoLanguage = PD.defaultLanguage info,
			_infoExtensions = PD.defaultExtensions info ++ PD.otherExtensions info ++ PD.oldExtensions info,
			_infoGHCOptions = fromMaybe [] $ lookup GHC (PD.options info),
			_infoSourceDirs = PD.hsSourceDirs info }

		pkgName :: P.Dependency -> String
		pkgName (P.Dependency dep _) = P.unPackageName dep

		flattenDescr :: PD.GenericPackageDescription -> PD.PackageDescription
		flattenDescr gpkg = pkg {
			PD.library = flip fmap mlib $ flattenCondTree
				(insertInfo PD.libBuildInfo (\i l -> l { PD.libBuildInfo = i })),
			PD.executables = flip fmap mexes $
				second (flattenCondTree (insertInfo PD.buildInfo (\i l -> l { PD.buildInfo = i }))) >>>
				(\(n, e) -> e { PD.exeName = n }),
			PD.testSuites = flip fmap mtests $
				second (flattenCondTree (insertInfo PD.testBuildInfo (\i l -> l { PD.testBuildInfo = i }))) >>>
				(\(n, t) -> t { PD.testName = n }) }
			where
				pkg = PD.packageDescription gpkg
				mlib = PD.condLibrary gpkg
				mexes = PD.condExecutables gpkg
				mtests = PD.condTestSuites gpkg

				insertInfo :: (a -> PD.BuildInfo) -> (PD.BuildInfo -> a -> a) -> [P.Dependency] -> a -> a
				insertInfo f s deps' x = s ((f x) { PD.targetBuildDepends = deps' }) x

-- | Read project info from .cabal
readProject :: FilePath -> IO Project
readProject file = do
	source <- readFile file
	length source `seq` either (hsdevError . InspectCabalError file) (return . mkProject) $ analyzeCabal source
	where
		mkProject desc = (project file) {
			_projectDescription = Just desc }

-- | Load project description
loadProject :: Project -> IO Project
loadProject p
	| isJust (_projectDescription p) = return p
	| otherwise = readProject (_projectCabal p)

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
inTarget src info = any (`isParent` src) $ view infoSourceDirsDef info

-- | Get first target for source file
fileTarget :: Project -> FilePath -> Maybe Info
fileTarget p f = listToMaybe $ fileTargets p f

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

-- | Get options for specific target
targetOpts :: Info -> [String]
targetOpts info' = concat [
	["-i" ++ s | s <- _infoSourceDirs info'],
	extensionsOpts $ withExtensions () info',
	["-package " ++ p | p <- _infoDepends info']]

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
