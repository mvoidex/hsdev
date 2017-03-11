module HsDev.Project (
	module HsDev.Project.Types,

	infoSourceDirsDef, targetFiles,
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
import Control.Lens (Lens', view, lens, _Just, toListOf, (^..), each)
import Control.Monad.Except
import Data.List
import Data.Maybe
import Data.Version (showVersion)
import Distribution.Compiler (CompilerFlavor(GHC))
import qualified Distribution.Package as P
import qualified Distribution.PackageDescription as PD
import qualified Distribution.ModuleName as PD (toFilePath)
import Distribution.PackageDescription.Parse
import Distribution.ModuleName (components)
import Distribution.Text (display)
import Language.Haskell.Extension
import System.FilePath

import HsDev.Project.Types
import HsDev.Error
import HsDev.Util

-- | infoSourceDirs lens with default
infoSourceDirsDef :: Lens' Info [FilePath]
infoSourceDirsDef = lens get' set' where
	get' i = case _infoSourceDirs i of
		[] -> ["."]
		dirs -> dirs
	set' i ["."] = i { _infoSourceDirs = [] }
	set' i dirs = i { _infoSourceDirs = dirs }

-- | Get all source file names of target without prepending them with source-dirs
targetFiles :: Target t => t -> [FilePath]
targetFiles target' = targetModules target' ++ map toFile (target' ^.. buildInfo . infoOtherModules . each) where
	toFile ps = joinPath ps <.> "hs"

-- | Analyze cabal file
analyzeCabal :: String -> Either String ProjectDescription
analyzeCabal source = case liftM flattenDescr $ parsePackageDescription source of
	ParseOk _ r -> Right ProjectDescription {
		_projectVersion = showVersion $ P.pkgVersion $ PD.package r,
		_projectLibrary = fmap toLibrary $ PD.library r,
		_projectExecutables = fmap toExecutable $ PD.executables r,
		_projectTests = fmap toTest $ PD.testSuites r }
	ParseFailed e -> Left $ "Parse failed: " ++ show e
	where
		toLibrary (PD.Library exposeds _ _ _ _ info) = Library (map components exposeds) (toInfo info)
		toExecutable (PD.Executable name path info) = Executable name path (toInfo info)
		toTest (PD.TestSuite name testInterface info enabled) = Test name enabled mainFile (toInfo info) where
			mainFile = case testInterface of
				PD.TestSuiteExeV10 _ fpath -> Just fpath
				PD.TestSuiteLibV09 _ mname -> Just $ PD.toFilePath mname
				_ -> Nothing
		toInfo info = Info {
			_infoDepends = map pkgName (PD.targetBuildDepends info),
			_infoLanguage = PD.defaultLanguage info,
			_infoExtensions = PD.defaultExtensions info ++ PD.otherExtensions info ++ PD.oldExtensions info,
			_infoGHCOptions = fromMaybe [] $ lookup GHC (PD.options info),
			_infoSourceDirs = PD.hsSourceDirs info,
			_infoOtherModules = map components (PD.otherModules info) }

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
				insertInfo f s deps' x = s ((f x) { PD.targetBuildDepends = deps' }) x

		flattenTree :: Monoid a => (c -> a -> a) -> PD.CondTree v c a -> a
		flattenTree f (PD.CondNode x cs cmps) = f cs x `mappend` mconcat (concatMap flattenBranch cmps) where
			flattenBranch (_, t, mb) = flattenTree f t : map (flattenTree f) (maybeToList mb)

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

-- | Check if source related to target, source must be relative to project directory
fileInTarget :: FilePath -> Info -> Bool
fileInTarget src info = any (`isParent` src) $ view infoSourceDirsDef info

-- | Get first target for source file
fileTarget :: Project -> FilePath -> Maybe Info
fileTarget p f = listToMaybe $ fileTargets p f

-- | Get possible targets for source file
-- There can be many candidates in case of module related to several executables or tests
fileTargets :: Project -> FilePath -> [Info]
fileTargets p f = case filter ((`isSuffixOf` f') . normalise . _executablePath) exes of
	[] -> filter (f' `fileInTarget`) (p ^.. projectDescription . _Just . infos)
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
sourceDirs = ordNub . concatMap dirs . toListOf infos where
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
