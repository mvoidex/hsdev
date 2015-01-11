module HsDev.Scan (
	-- * Enumerate functions
	enumCabal, CompileFlag, ModuleToScan, ProjectToScan, SandboxToScan, ScanContents(..),
	enumProject, enumDirectory,

	-- * Scan
	scanProjectFile,
	scanModule, upToDate, rescanModule, changedModule, changedModules
	) where

import Control.Applicative
import Control.Monad.Error
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Traversable (traverse)
import Language.Haskell.GhcMod (defaultOptions)
import System.Directory

import HsDev.Symbols
import HsDev.Database
import HsDev.Tools.GhcMod
import HsDev.Tools.GhcMod.InferType (inferTypes)
import HsDev.Inspect
import HsDev.Project
import HsDev.Util

-- | Enum cabal modules
enumCabal :: [String] -> Cabal -> ErrorT String IO [ModuleLocation]
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
enumProject :: Project -> ErrorT String IO ProjectToScan
enumProject p = do
	p' <- loadProject p
	srcs <- projectSources p'
	return (p', [(FileModule (entity src) (Just p'), extensionsOpts (extensions src)) | src <- srcs])

-- | Enum directory modules
enumDirectory :: FilePath -> ErrorT String IO ScanContents
enumDirectory dir = do
	cts <- liftException $ traverseDirectory dir
	let
		projects = filter cabalFile cts
		sources = filter haskellSource cts
	dirs <- liftIO $ filterM doesDirectoryExist cts
	sboxes <- liftIO $ liftM catMaybes $ mapM findPackageDb dirs
	projs <- mapM (enumProject . project) projects
	let
		projPaths = map (projectPath . fst) projs
		standalone = map (\f -> FileModule f Nothing) $ filter (\s -> not (any (`isParent` s) projPaths)) sources
	return $ ScanContents {
		modulesToScan = [(s, []) | s <- standalone],
		projectsToScan = projs,
		sandboxesToScan = map Sandbox sboxes }

-- | Scan project file
scanProjectFile :: [String] -> FilePath -> ErrorT String IO Project
scanProjectFile _ f = do
	proj <- (liftIO $ locateProject f) >>= maybe (throwError "Can't locate project") return
	loadProject proj

-- | Scan module
scanModule :: [String] -> ModuleLocation -> ErrorT String IO InspectedModule
scanModule opts (FileModule f _) = inspectFile opts f >>= traverse infer' where
	infer' m = tryInfer <|> return m where
		tryInfer = mapErrorT (withCurrentDirectory (sourceModuleRoot (moduleName m) f)) $
			runGhcMod defaultOptions $ inferTypes opts Cabal m
scanModule opts (CabalModule c p n) = browse opts c n p
scanModule _ (ModuleSource _) = throwError "Can inspect only modules in file or cabal"

-- | Is inspected module up to date?
upToDate :: [String] -> InspectedModule -> ErrorT String IO Bool
upToDate opts (Inspected insp m _) = case m of
	FileModule f _ -> liftM (== insp) $ fileInspection f opts
	CabalModule _ _ _ -> return $ insp == browseInspection opts
	_ -> return False

-- | Rescan inspected module
rescanModule :: [String] -> InspectedModule -> ErrorT String IO (Maybe InspectedModule)
rescanModule opts im = do
	up <- upToDate opts im
	if up
		then return Nothing
		else fmap Just $ scanModule opts (inspectedId im)

-- | Is module new or recently changed
changedModule :: Database -> [String] -> ModuleLocation -> ErrorT String IO Bool
changedModule db opts m = maybe (return True) (liftM not . upToDate opts) m' where
	m' = M.lookup m (databaseModules db)

-- | Returns new (to scan) and changed (to rescan) modules
changedModules :: Database -> [String] -> [ModuleLocation] -> ErrorT String IO [ModuleLocation]
changedModules db opts ms = filterM (changedModule db opts) ms
