{-# LANGUAGE FlexibleInstances, TypeOperators, TypeApplications, OverloadedStrings #-}

module HsDev.Scan (
	-- * Enumerate functions
	CompileFlag, ModuleToScan, ProjectToScan, PackageDbToScan, ScanContents(..),
	EnumContents(..),
	enumRescan, enumDependent, enumProject, enumSandbox, enumDirectory,

	-- * Scan
	scanProjectFile,
	scanModify,
	upToDate, changedModules,

	-- * Reexportss
	module HsDev.Symbols.Types,
	module Control.Monad.Except,
	) where

import Control.DeepSeq
import Control.Lens hiding ((%=))
import Control.Monad.Except
import Data.Deps
import Data.Maybe (catMaybes, isJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List (intercalate)
import Data.Text (Text)
import Data.Text.Lens (unpacked)
import qualified Data.Text as T
import Data.String (IsString, fromString)
import qualified Data.Set as S
import System.Directory
import Text.Format
import qualified System.Log.Simple as Log

import HsDev.Error
import qualified HsDev.Database.SQLite as SQLite
import HsDev.Database.SQLite.Select
import HsDev.Scan.Browse (browsePackages)
import HsDev.Server.Types (FileSource(..), CommandMonad(..), inSessionGhc)
import HsDev.Sandbox
import HsDev.Symbols
import HsDev.Symbols.Types
import HsDev.Display
import HsDev.Inspect
import HsDev.Util
import System.Directory.Paths

-- | Compile flags
type CompileFlag = String
-- | Module with flags ready to scan
type ModuleToScan = (ModuleLocation, [CompileFlag], Maybe Text)
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

instance Formattable ScanContents where
	formattable (ScanContents ms ps cs) = formattable str where
		str :: String
		str = format "modules: {}, projects: {}, package-dbs: {}"
			~~ (T.intercalate comma $ ms ^.. each . _1 . moduleFile)
			~~ (T.intercalate comma $ ps ^.. each . _1 . projectPath)
			~~ (intercalate comma $ map (display . topPackageDb) $ cs ^.. each)
		comma :: IsString s => s
		comma = fromString ", "

class EnumContents a where
	enumContents :: CommandMonad m => a -> m ScanContents

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

instance {-# OVERLAPPABLE #-} EnumContents a => EnumContents [a] where
	enumContents = liftM mconcat . tries . map enumContents

instance {-# OVERLAPPING #-} EnumContents FilePath where
	enumContents f
		| haskellSource f = hsdevLiftIO $ do
			mproj <- liftIO $ locateProject f
			case mproj of
				Nothing -> enumContents $ FileModule (fromFilePath f) Nothing
				Just proj -> do
					ScanContents _ [(_, mods)] _ <- enumContents proj
					return $ ScanContents (filter ((== Just f) . preview (_1 . moduleFile . path)) mods) [] []
		| otherwise = enumDirectory f

instance {-# OVERLAPPING #-} EnumContents Path where
	enumContents = enumContents . view path

instance EnumContents FileSource where
	enumContents (FileSource f mcts)
		| haskellSource (view path f) = do
			ScanContents [(m, opts, _)] _ _ <- enumContents f
			return $ ScanContents [(m, opts, mcts)] [] []
		| otherwise = return mempty

-- | Enum rescannable (i.e. already scanned) file
enumRescan :: CommandMonad m => FilePath -> m ScanContents
enumRescan fpath = Log.scope "enum-rescan" $ do
	ms <- SQLite.query @_ @(ModuleLocation SQLite.:. Inspection)
		(toQuery $ qModuleLocation `mappend` select_ ["ml.inspection_time", "ml.inspection_opts"] [] [] `mappend` where_ ["ml.file == ?"]) (SQLite.Only fpath)
	case ms of
		[] -> do
			Log.sendLog Log.Warning $ "file {} not found" ~~ fpath
			return mempty
		((mloc SQLite.:. insp):_) -> do
			when (length ms > 1) $ Log.sendLog Log.Warning $ "several modules with file == {} found, taking first one" ~~ fpath
			return $ ScanContents [(mloc, insp ^.. inspectionOpts . each . unpacked, Nothing)] [] []

-- | Enum file dependent
enumDependent :: CommandMonad m => FilePath -> m ScanContents
enumDependent fpath = Log.scope "enum-dependent" $ do
	ms <- SQLite.query @_ @ModuleId
		(toQuery $ qModuleId `mappend` where_ ["mu.file == ?"]) (SQLite.Only fpath)
	case ms of
		[] -> do
			Log.sendLog Log.Warning $ "file {} not found" ~~ fpath
			return mempty
		(mid:_) -> do
			when (length ms > 1) $ Log.sendLog Log.Warning $ "several modules with file == {} found, taking first one" ~~ fpath
			let
				mcabal = mid ^? moduleLocation . moduleProject . _Just . projectCabal
			depList <- SQLite.query @_ @(Path, Path) "select d.module_file, d.depends_file from sources_depends as d, projects_modules_scope as ps where ps.cabal is ? and ps.module_id == d.module_id;"
				(SQLite.Only mcabal)
			let
				rdeps = inverse . either (const mempty) id . flatten . mconcat . map (uncurry dep) $ depList
				dependent = rdeps ^. ix (fromFilePath fpath)
			liftM mconcat $ mapM (enumRescan . view path) dependent

-- | Enum project sources
enumProject :: CommandMonad m => Project -> m ScanContents
enumProject p = hsdevLiftIO $ do
	p' <- liftIO $ loadProject p
	pdbs <- inSessionGhc $ searchPackageDbStack (view projectPath p')
	pkgs <- inSessionGhc $ liftM (S.fromList . map (view (package . packageName))) $ browsePackages [] pdbs
	let
		projOpts :: Path -> [Text]
		projOpts f = map fromString $ concatMap makeOpts $ fileTargets p' f where
			makeOpts :: Info -> [String]
			makeOpts i = concat [
				["-hide-all-packages"],
				["-package " ++ view (projectName . path) p'],
				["-package " ++ T.unpack dep' | dep' <- view infoDepends i, dep' `S.member` pkgs]]
	srcs <- liftIO $ projectSources p'
	let
		mlocs = over each (\src -> over ghcOptions (++ projOpts (view entity src)) . over entity (\f -> FileModule f (Just p')) $ src) srcs
	mods <- liftM modulesToScan $ enumContents mlocs
	return $ ScanContents [] [(p', mods)] [] -- (sandboxCabals sboxes)

-- | Enum sandbox
enumSandbox :: CommandMonad m => Sandbox -> m ScanContents
enumSandbox = (inSessionGhc . sandboxPackageDbStack) >=> enumContents

-- | Enum directory modules
enumDirectory :: CommandMonad m => FilePath -> m ScanContents
enumDirectory dir = hsdevLiftIO $ do
	cts <- liftIO $ traverseDirectory dir
	let
		projects = filter cabalFile cts
		sources = filter haskellSource cts
	dirs <- liftIO $ filterM doesDirectoryExist cts
	sboxes <- liftM catMaybes $ triesMap (liftIO . findSandbox . fromFilePath) dirs
	pdbs <- mapM enumSandbox sboxes
	projs <- liftM mconcat $ triesMap (enumProject . project) projects
	let
		projPaths = map (view projectPath . fst) $ projectsToScan projs
		standalone = map (`FileModule` Nothing) $ filter (\s -> not (any (`isParent` s) projPaths)) $ map fromFilePath sources
	return $ mconcat [
		ScanContents [(s, [], Nothing) | s <- standalone] [] [],
		projs,
		mconcat pdbs]

-- | Scan project file
scanProjectFile :: CommandMonad m => [String] -> Path -> m Project
scanProjectFile _ f = hsdevLiftIO $ do
	proj <- (liftIO $ locateProject (view path f)) >>= maybe (hsdevError $ FileNotFound f) return
	liftIO $ loadProject proj

-- | Scan additional info and modify scanned module
scanModify :: CommandMonad m => ([String] -> Module -> m Module) -> InspectedModule -> m InspectedModule
scanModify f im = traverse f' im where
	f' m = f (toListOf (inspection . inspectionOpts . each . unpacked) im) m

-- | Is inspected module up to date?
upToDate :: ModuleLocation -> [String] -> Inspection -> IO Bool
upToDate mloc opts insp = do
	insp' <- moduleInspection mloc opts
	return $ fresh insp insp'

-- | Returns new (to scan) and changed (to rescan) modules
changedModules :: Map ModuleLocation Inspection -> [String] -> [ModuleToScan] -> IO [ModuleToScan]
changedModules inspMap opts = filterM $ \m -> if isJust (m ^. _3)
	then return True
	else maybe
		(return True)
		(liftM not . upToDate (m ^. _1) (opts ++ (m ^. _2)))
		(M.lookup (m ^. _1) inspMap)
