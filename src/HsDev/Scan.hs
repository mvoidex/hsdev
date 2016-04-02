{-# LANGUAGE FlexibleInstances #-}

module HsDev.Scan (
	-- * Enumerate functions
	CompileFlag, ModuleToScan, ProjectToScan, PackageDbToScan, ScanContents(..),
	EnumContents(..),
	enumProject, enumSandbox, enumDirectory,

	-- * Scan
	scanProjectFile,
	scanModule, scanModify, upToDate, rescanModule, changedModule, changedModules,

	-- * Reexportss
	module HsDev.Database,
	module HsDev.Symbols.Types,
	module Control.Monad.Except,
	) where

import Control.DeepSeq
import Control.Lens (view, preview, set, over, each, _Right, _1, _2, _3, (^.), (^..))
import Control.Monad.Except
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.List (intercalate)
import System.Directory
import Text.Format

import HsDev.Scan.Browse (browsePackages)
import HsDev.Server.Types (FileContents(..))
import HsDev.Sandbox
import HsDev.Symbols
import HsDev.Symbols.Types
import HsDev.Database
import HsDev.Display
import HsDev.Tools.GhcMod
import HsDev.Inspect
import HsDev.Util

-- | Compile flags
type CompileFlag = String
-- | Module with flags ready to scan
type ModuleToScan = (ModuleLocation, [CompileFlag], Maybe String)
-- | Project ready to scan
type ProjectToScan = (Project, [ModuleToScan])
-- | Package-db sandbox to scan (top of stack)
type PackageDbToScan = PackageDbStack

-- | Scan info
data ScanContents = ScanContents {
	modulesToScan :: [ModuleToScan],
	projectsToScan :: [ProjectToScan],
	sandboxesToScan :: [PackageDbStack] }

instance NFData ScanContents where
	rnf (ScanContents ms ps ss) = rnf ms `seq` rnf ps `seq` rnf ss

instance Monoid ScanContents where
	mempty = ScanContents [] [] []
	mappend (ScanContents lm lp ls) (ScanContents rm rp rs) = ScanContents
		(uniqueBy (view _1) $ lm ++ rm)
		(uniqueBy (view _1) $ lp ++ rp)
		(ordNub $ ls ++ rs)

instance FormatBuild ScanContents where
	formatBuild (ScanContents ms ps cs) = formatBuild str where
		str :: String
		str = format "modules: {}, projects: {}, package-dbs: {}"
			~~ (intercalate ", " $ ms ^.. each . _1 . moduleFile)
			~~ (intercalate ", " $ ps ^.. each . _1 . projectPath)
			~~ (intercalate ", " $ map (display . topPackageDb) $ cs ^.. each)

class EnumContents a where
	enumContents :: a -> ExceptT String IO ScanContents

instance EnumContents ModuleLocation where
	enumContents mloc = return $ ScanContents [(mloc, [], Nothing)] [] []

instance EnumContents (Extensions ModuleLocation) where
	enumContents ex = return $ ScanContents [(view entity ex, extensionsOpts ex, Nothing)] [] []

instance EnumContents Project where
	enumContents = enumProject

instance EnumContents PackageDbStack where
	enumContents pdbs = return $ ScanContents [] [] (packageDbStacks pdbs)

instance EnumContents Sandbox where
	enumContents = enumSandbox

instance {-# OVERLAPPING #-} EnumContents a => EnumContents [a] where
	enumContents = liftM mconcat . tries . map enumContents

instance {-# OVERLAPS #-} EnumContents FilePath where
	enumContents f
		| haskellSource f = do
			mproj <- liftEIO $ locateProject f
			case mproj of
				Nothing -> enumContents $ FileModule f Nothing
				Just proj -> do
					ScanContents _ [(_, mods)] _ <- enumContents proj
					return $ ScanContents (filter ((== Just f) . preview (_1 . moduleFile)) mods) [] []
		| otherwise = enumDirectory f

instance EnumContents FileContents where
	enumContents (FileContents f cts)
		| haskellSource f = do
			ScanContents [(m, opts, _)] _ _ <- enumContents f
			return $ ScanContents [(m, opts, Just cts)] [] []
		| otherwise = return mempty

-- | Enum project sources
enumProject :: Project -> ExceptT String IO ScanContents
enumProject p = do
	p' <- loadProject p
	pdbs <- liftE $ searchPackageDbStack (view projectPath p')
	pkgs <- liftM (map $ view (package . packageName)) $ browsePackages [] pdbs
	let
		projOpts :: FilePath -> [String]
		projOpts f = concatMap makeOpts $ fileTargets p' f where
			makeOpts :: Info -> [String]
			makeOpts i = concat [
				["-hide-all-packages"],
				["-package " ++ view projectName p'],
				["-package " ++ dep | dep <- view infoDepends i, dep `elem` pkgs]]
	srcs <- projectSources p'
	let
		mlocs = over each (\src -> over ghcOptions (++ projOpts (view entity src)) . over entity (\f -> FileModule f (Just p')) $ src) srcs
	mods <- liftM modulesToScan $ enumContents mlocs
	return $ ScanContents [] [(p', mods)] [] -- (sandboxCabals sboxes)

-- | Enum sandbox
enumSandbox :: Sandbox -> ExceptT String IO ScanContents
enumSandbox = sandboxPackageDbStack >=> enumContents

-- | Enum directory modules
enumDirectory :: FilePath -> ExceptT String IO ScanContents
enumDirectory dir = do
	cts <- liftException $ traverseDirectory dir
	let
		projects = filter cabalFile cts
		sources = filter haskellSource cts
	dirs <- liftE $ filterM doesDirectoryExist cts
	sboxes <- liftM catMaybes $ triesMap (liftE . findSandbox) dirs
	pdbs <- mapM enumSandbox sboxes
	projs <- liftM mconcat $ triesMap (enumProject . project) projects
	let
		projPaths = map (view projectPath . fst) $ projectsToScan projs
		standalone = map (`FileModule` Nothing) $ filter (\s -> not (any (`isParent` s) projPaths)) sources
	return $ mconcat [
		ScanContents [(s, [], Nothing) | s <- standalone] [] [],
		projs,
		mconcat pdbs]

-- | Scan project file
scanProjectFile :: [String] -> FilePath -> ExceptT String IO Project
scanProjectFile _ f = do
	proj <- (liftE $ locateProject f) >>= maybe (throwError "Can't locate project") return
	loadProject proj

-- | Scan module
scanModule :: [(String, String)] -> [String] -> ModuleLocation -> Maybe String -> ExceptT String IO InspectedModule
scanModule defines opts (FileModule f p) mcts = liftM setProj $ inspectFile defines opts f mcts where
	setProj =
		set (inspectedId . moduleProject) p .
		set (inspectionResult . _Right . moduleLocation . moduleProject) p
-- scanModule opts (FileModule f _) = inspectFile opts f >>= traverse infer' where
-- 	infer' m = tryInfer <|> return m where
-- 		tryInfer = mapExceptT (withCurrentDirectory (sourceModuleRoot (moduleName m) f)) $
-- 			runGhcMod defaultOptions $ inferTypes opts Cabal m
scanModule _ opts (InstalledModule c p n) _ = do
	pdbs <- liftIO $ getDbs c
	browse opts pdbs n p
	where
		getDbs :: PackageDb -> IO PackageDbStack
		getDbs = maybe (return userDb) searchPackageDbStack . preview packageDb
scanModule _ _ (ModuleSource _) _ = throwError "Can inspect only modules in file or cabal"

-- | Scan additional info and modify scanned module
scanModify :: ([String] -> PackageDbStack -> Module -> ExceptT String IO Module) -> InspectedModule -> ExceptT String IO InspectedModule
scanModify f im = traverse f' im where
	f' m = do
		pdbs <- liftIO $ case view moduleLocation m of
			-- TODO: Get actual sandbox stack
			FileModule fpath _ -> searchPackageDbStack fpath
			InstalledModule pdb _ _ -> maybe (return userDb) searchPackageDbStack $ preview packageDb pdb
			_ -> return userDb
		f (fromMaybe [] $ preview (inspection . inspectionOpts) im) pdbs m

-- | Is inspected module up to date?
upToDate :: [String] -> InspectedModule -> ExceptT String IO Bool
upToDate opts (Inspected insp m _) = case m of
	FileModule f _ -> liftM (== insp) $ fileInspection f opts
	InstalledModule _ _ _ -> return $ insp == browseInspection opts
	_ -> return False

-- | Rescan inspected module
rescanModule :: [(String, String)] -> [String] -> InspectedModule -> ExceptT String IO (Maybe InspectedModule)
rescanModule defines opts im = do
	up <- upToDate opts im
	if up
		then return Nothing
		else fmap Just $ scanModule defines opts (view inspectedId im) Nothing

-- | Is module new or recently changed
changedModule :: Database -> [String] -> ModuleLocation -> ExceptT String IO Bool
changedModule db opts m = maybe (return True) (liftM not . upToDate opts) m' where
	m' = lookupInspected m db

-- | Returns new (to scan) and changed (to rescan) modules
changedModules :: Database -> [String] -> [ModuleToScan] -> ExceptT String IO [ModuleToScan]
changedModules db opts = filterM $ \m -> if isJust (m ^. _3)
	then return True
	else changedModule db (opts ++ (m ^. _2)) (m ^. _1)
