{-# LANGUAGE FlexibleInstances #-}

module HsDev.Scan (
	-- * Enumerate functions
	CompileFlag, ModuleToScan, ProjectToScan, PackageDbToScan, ScanContents(..),
	EnumContents(..),
	enumRescan, enumDependent, enumProject, enumSandbox, enumDirectory,

	-- * Scan
	scanProjectFile,
	scanModify, upToDate, changedModule, changedModules,

	-- * Reexportss
	module HsDev.Database,
	module HsDev.Symbols.Types,
	module Control.Monad.Except,
	) where

import Control.DeepSeq
import Control.Lens hiding ((%=))
import Control.Monad.Except
import Data.Async
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.List (intercalate)
import Data.Text (Text)
import Data.Text.Lens (unpacked)
import qualified Data.Text as T
import Data.String (IsString, fromString)
import qualified Data.Set as S
import System.Directory
import Text.Format

import HsDev.Error
import HsDev.Scan.Browse (browsePackages)
import HsDev.Server.Types (FileSource(..), Session(..), askSession, CommandMonad(..), inSessionGhc)
import HsDev.Sandbox
import HsDev.Symbols
import HsDev.Symbols.Resolve
import HsDev.Symbols.Types
import HsDev.Database
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
enumRescan fpath = do
	dbval <- askSession sessionDatabase >>= liftIO . readAsync
	return $ fromMaybe mempty $ do
		m <- dbval ^? databaseModules . atFile (fromFilePath fpath)
		loc <- m ^? inspected . moduleId . moduleLocation
		return $ ScanContents [(loc, m ^.. inspection . inspectionOpts . each . unpacked, Nothing)] [] []

-- | Enum file dependent
enumDependent :: CommandMonad m => FilePath -> m ScanContents
enumDependent fpath = do
	dbval <- askSession sessionDatabase >>= liftIO . readAsync
	let
		mproj = dbval ^? databaseModules . atFile (fromFilePath fpath) . inspected . moduleId . moduleLocation . moduleProject . _Just
		dbslice = dbval ^. maybe standaloneSlice projectSlice mproj
		rdeps = sourceRDeps dbslice
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
				["-package " ++ T.unpack dep | dep <- view infoDepends i, dep `S.member` pkgs]]
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
upToDate :: [String] -> InspectedModule -> IO Bool
upToDate opts im = case view inspectedKey im of
	FileModule f _ -> liftM (== view inspection im) $ fileInspection f opts
	InstalledModule _ _ _ -> return $ view inspection im == InspectionAt 0 (map fromString opts)
	_ -> return False

-- | Is module new or recently changed
changedModule :: Database -> [String] -> ModuleLocation -> IO Bool
changedModule db opts m = maybe (return True) (liftM not . liftIO . upToDate opts) m' where
	m' = db ^? databaseModules . ix m

-- | Returns new (to scan) and changed (to rescan) modules
changedModules :: Database -> [String] -> [ModuleToScan] -> IO [ModuleToScan]
changedModules db opts = filterM $ \m -> if isJust (m ^. _3)
	then return True
	else changedModule db (opts ++ (m ^. _2)) (m ^. _1)
