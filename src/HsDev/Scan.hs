module HsDev.Scan (
	-- * Enumerate functions
	enumCabal, CompileFlag, ModuleToScan, ProjectToScan, SandboxToScan, ScanContents(..),
	enumProject, enumDirectory,

	-- * Scan
	scanProjectFile,
	scanModule, scanModify, upToDate, rescanModule, changedModule, changedModules,

	-- * Reexportss
	module HsDev.Database,
	module HsDev.Symbols.Types,
	module Control.Monad.Except,
	) where

import Control.Applicative ((<|>))
import Control.Lens (view, preview, set, _Right)
import Control.Monad.Except
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)
import System.Directory

import HsDev.Scan.Browse (browsePackages)
import HsDev.Symbols
import HsDev.Symbols.Types
import HsDev.Database
import HsDev.Tools.GhcMod
import HsDev.Inspect
import HsDev.Util

-- | Enum cabal modules
enumCabal :: [String] -> Cabal -> ExceptT String IO [ModuleLocation]
enumCabal = list

-- | Compile flags
type CompileFlag = String
-- | Module with flags ready to scan
type ModuleToScan = (ModuleLocation, [CompileFlag])
-- | Project ready to scan
type ProjectToScan = (Project, [ModuleToScan])
-- | Cabal sandbox to scan
type SandboxToScan = Cabal

-- | Scan info
data ScanContents = ScanContents {
	modulesToScan :: [ModuleToScan],
	projectsToScan :: [ProjectToScan],
	sandboxesToScan :: [SandboxToScan] }

-- | Enum project sources
enumProject :: Project -> ExceptT String IO ProjectToScan
enumProject p = do
	p' <- loadProject p
	cabal <- liftE $ searchSandbox (view projectPath p')
	pkgs <- liftM (map $ view packageName) $ browsePackages [] cabal
	let
		projOpts :: FilePath -> [String]
		projOpts f = concatMap makeOpts $ fileTargets p' f where
			makeOpts :: Info -> [String]
			makeOpts i = concat [
				["-hide-all-packages"],
				["-package " ++ view projectName p'],
				["-package " ++ dep | dep <- view infoDepends i, dep `elem` pkgs]]
	srcs <- projectSources p'
	return (p', [(FileModule (view entity src) (Just p'), extensionsOpts (view extensions src) ++ projOpts (view entity src)) | src <- srcs])

-- | Enum directory modules
enumDirectory :: FilePath -> ExceptT String IO ScanContents
enumDirectory dir = do
	cts <- liftException $ traverseDirectory dir
	let
		projects = filter cabalFile cts
		sources = filter haskellSource cts
	dirs <- liftE $ filterM doesDirectoryExist cts
	sboxes <- liftM catMaybes $ triesMap (liftE . findPackageDb) dirs
	projs <- triesMap (enumProject . project) projects
	let
		projPaths = map (view projectPath . fst) projs
		standalone = map (\f -> FileModule f Nothing) $ filter (\s -> not (any (`isParent` s) projPaths)) sources
	return $ ScanContents {
		modulesToScan = [(s, []) | s <- standalone],
		projectsToScan = projs,
		sandboxesToScan = map Sandbox sboxes }

-- | Scan project file
scanProjectFile :: [String] -> FilePath -> ExceptT String IO Project
scanProjectFile _ f = do
	proj <- (liftE $ locateProject f) >>= maybe (throwError "Can't locate project") return
	loadProject proj

-- | Scan module
scanModule :: [String] -> ModuleLocation -> ExceptT String IO InspectedModule
scanModule opts (FileModule f p) = liftM setProj $ inspectFile opts f where
	setProj =
		set (inspectedId . moduleProject) p .
		set (inspectionResult . _Right . moduleLocation . moduleProject) p
-- scanModule opts (FileModule f _) = inspectFile opts f >>= traverse infer' where
-- 	infer' m = tryInfer <|> return m where
-- 		tryInfer = mapExceptT (withCurrentDirectory (sourceModuleRoot (moduleName m) f)) $
-- 			runGhcMod defaultOptions $ inferTypes opts Cabal m
scanModule opts (CabalModule c p n) = browse opts c n p
scanModule _ (ModuleSource _) = throwError "Can inspect only modules in file or cabal"

-- | Scan additional info and modify scanned module. Dones't fail on error, just left module unchanged
scanModify :: ([String] -> Cabal -> Module -> ExceptT String IO Module) -> InspectedModule -> ExceptT String IO InspectedModule
scanModify f im = traverse f' im <|> return im where
	-- TODO: Get actual sandbox
	f' = f (fromMaybe [] $ preview (inspection . inspectionOpts) im) Cabal

-- | Is inspected module up to date?
upToDate :: [String] -> InspectedModule -> ExceptT String IO Bool
upToDate opts (Inspected insp m _) = case m of
	FileModule f _ -> liftM (== insp) $ fileInspection f opts
	CabalModule _ _ _ -> return $ insp == browseInspection opts
	_ -> return False

-- | Rescan inspected module
rescanModule :: [String] -> InspectedModule -> ExceptT String IO (Maybe InspectedModule)
rescanModule opts im = do
	up <- upToDate opts im
	if up
		then return Nothing
		else fmap Just $ scanModule opts (view inspectedId im)

-- | Is module new or recently changed
changedModule :: Database -> [String] -> ModuleLocation -> ExceptT String IO Bool
changedModule db opts m = maybe (return True) (liftM not . upToDate opts) m' where
	m' = M.lookup m (databaseModules db)

-- | Returns new (to scan) and changed (to rescan) modules
changedModules :: Database -> [String] -> [ModuleToScan] -> ExceptT String IO [ModuleToScan]
changedModules db opts ms = filterM (\(m, opts') -> changedModule db (opts ++ opts') m) ms
