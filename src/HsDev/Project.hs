{-# LANGUAGE OverloadedStrings, CPP #-}

module HsDev.Project (
	module HsDev.Project.Types,

	infoSourceDirsDef, targetFiles, projectTargetFiles,
	analyzeCabal,
	readProject, loadProject,
	withExtensions,
	fileInTarget, fileTarget, fileTargets, findSourceDir, sourceDirs,
	targetOpts,

	-- * Helpers
	showExtension, flagExtension, extensionFlag,
	extensionsOpts
	) where

import Control.Arrow
import Control.Lens hiding ((.=), (%=), (<.>), set')
import Control.Monad.Except
import Control.Monad.Loops
import Data.List
import Data.Maybe
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T (intercalate)
import Data.Text.Lens (unpacked)
import Distribution.Compiler (CompilerFlavor(GHC))
import qualified Distribution.Package as P
import qualified Distribution.PackageDescription as PD
import qualified Distribution.ModuleName as PD (toFilePath)
import Distribution.ModuleName (components)
import Distribution.Text (display)
import Language.Haskell.Extension
import System.FilePath
import System.Log.Simple hiding (Level(..))
import qualified System.Log.Simple as Log (Level(..))
import Text.Format

import System.Directory.Paths
import HsDev.Project.Compat
import HsDev.Project.Types
import HsDev.Error
import HsDev.Util

-- | infoSourceDirs lens with default
infoSourceDirsDef :: Lens' Info [Path]
infoSourceDirsDef = lens get' set' where
	get' i = case _infoSourceDirs i of
		[] -> ["."]
		dirs -> dirs
	set' i ["."] = i { _infoSourceDirs = [] }
	set' i dirs = i { _infoSourceDirs = dirs }

-- | Get all source file names of target without prepending them with source-dirs
targetFiles :: Target t => t -> [Path]
targetFiles target' = concat [
	maybeToList (targetMain target'),
	map toFile $ targetModules target',
	map toFile $ target' ^.. buildInfo . infoOtherModules . each]
	where
		toFile ps = fromFilePath (joinPath (ps ^.. each . unpacked) <.> "hs")

-- | Get all source file names relative to project root
projectTargetFiles :: (MonadLog m, Target t) => Project -> t -> m [Path]
projectTargetFiles proj t = do
	liftM concat $ forM files $ \file' -> do
		candidate <- liftIO $ firstM (fileExists . absolutise (proj ^. projectPath)) [subPath srcDir file' | srcDir <- srcDirs]
		case candidate of
			Nothing -> do
				sendLog Log.Warning $ "Unable to locate source file: {} in source-dirs: {}" ~~ file' ~~ (T.intercalate ", " srcDirs)
				return []
			Just file'' -> return [normPath file'']
	where
		files = targetFiles t
		srcDirs = t ^.. buildInfo . infoSourceDirsDef . each

-- | Analyze cabal file
analyzeCabal :: String -> Either String ProjectDescription
analyzeCabal source = case liftM flattenDescr $ parsePackageDesc source of
	Right r -> Right ProjectDescription {
		_projectVersion = pack $ showVer $ P.pkgVersion $ PD.package r,
		_projectLibrary = fmap toLibrary $ PD.library r,
		_projectExecutables = fmap toExecutable $ PD.executables r,
		_projectTests = fmap toTest $ PD.testSuites r }
	Left e -> Left $ "Parse failed: " ++ e
	where
		toLibrary lib = Library (map (map pack . components) $ PD.exposedModules lib) (toInfo $ PD.libBuildInfo lib)
		toExecutable exe = Executable (componentName $ PD.exeName exe) (fromFilePath $ PD.modulePath exe) (toInfo $ PD.buildInfo exe)
		toTest test = Test (componentName $ PD.testName test) (testSuiteEnabled test) (fmap fromFilePath mainFile) (toInfo $ PD.testBuildInfo test) where
			mainFile = case PD.testInterface test of
				PD.TestSuiteExeV10 _ fpath -> Just fpath
				PD.TestSuiteLibV09 _ mname -> Just $ PD.toFilePath mname
				_ -> Nothing
		toInfo info = Info {
			_infoDepends = map pkgName (PD.targetBuildDepends info),
			_infoLanguage = PD.defaultLanguage info,
			_infoExtensions = PD.defaultExtensions info ++ PD.otherExtensions info ++ PD.oldExtensions info,
			_infoGHCOptions = maybe [] (map pack) $ lookup GHC (PD.options info),
			_infoSourceDirs = map pack $ PD.hsSourceDirs info,
			_infoOtherModules = map (map pack . components) (PD.otherModules info) }

		pkgName :: P.Dependency -> Text
		pkgName (P.Dependency dep _) = pack $ P.unPackageName dep

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
readProject file' = do
	source <- readFile file'
	length source `seq` either (hsdevError . InspectCabalError file') (return . mkProject) $ analyzeCabal source
	where
		mkProject desc = (project file') {
			_projectDescription = Just desc }

-- | Load project description
loadProject :: Project -> IO Project
loadProject p
	| isJust (_projectDescription p) = return p
	| otherwise = do
		p' <- readProject (_projectCabal p ^. path)
		return $ set projectBuildTool (view projectBuildTool p) p'

-- | Extensions for target
withExtensions :: a -> Info -> Extensions a
withExtensions x i = Extensions {
	_extensions = _infoExtensions i,
	_ghcOptions = _infoGHCOptions i,
	_entity = x }

-- | Check if source related to target, source must be relative to project directory
fileInTarget :: Path -> Info -> Bool
fileInTarget src info = any (`isParent` src) $ view infoSourceDirsDef info

-- | Get first target for source file
fileTarget :: Project -> Path -> Maybe Info
fileTarget p f = listToMaybe $ fileTargets p f

-- | Get possible targets for source file
-- There can be many candidates in case of module related to several executables or tests
fileTargets :: Project -> Path -> [Info]
fileTargets p f = case filter ((`isParent` f') . view executablePath) exes of
	[] -> filter (f' `fileInTarget`) (p ^.. projectDescription . _Just . infos)
	exes' -> map _executableBuildInfo exes'
	where
		f' = relPathTo (_projectPath p) f
		exes = p ^. projectDescription . _Just . projectExecutables

-- | Finds source dir file belongs to
findSourceDir :: Project -> Path -> Maybe (Extensions Path)
findSourceDir p f = do
	info <- listToMaybe $ fileTargets p f
	fmap (`withExtensions` info) $ listToMaybe $ filter (`isParent` f) $ map (_projectPath p `subPath`) (info ^. infoSourceDirsDef)

-- | Returns source dirs for library, executables and tests
sourceDirs :: ProjectDescription -> [Extensions Path]
sourceDirs = ordNub . concatMap dirs . toListOf infos where
	dirs i = map (`withExtensions` i) (i ^. infoSourceDirsDef)

-- | Get options for specific target
targetOpts :: Info -> [String]
targetOpts info' = concat [
	["-i" ++ unpack s | s <- _infoSourceDirs info'],
	extensionsOpts $ withExtensions () info',
	["-package " ++ unpack p | p <- _infoDepends info']]

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
extensionsOpts e = map (extensionFlag . showExtension) (_extensions e) ++ map unpack (_ghcOptions e)
