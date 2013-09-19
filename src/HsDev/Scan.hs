module HsDev.Scan (
	-- * Enumerate functions
	enumCabal, enumProject, enumDirectory,

	-- * Scan
	scanProjectFile,
	scanModule, upToDate, rescanModule, changedModules
	) where

import Control.Applicative
import Control.Exception
import Control.Monad.Error
import Data.Either
import Data.List
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Monoid

import HsDev.Symbols
import HsDev.Database
import HsDev.Tools.GhcMod
import HsDev.Tools.HDocs
import HsDev.Inspect
import HsDev.Project
import HsDev.Util

-- | Enum cabal modules
enumCabal :: [String] -> Cabal -> ErrorT String IO [ModuleLocation]
enumCabal opts cabal = liftM (map $ CabalModule cabal Nothing) $ list opts

-- | Enum project sources
enumProject :: Project -> ErrorT String IO [ModuleLocation]
enumProject p = do
	p' <- loadProject p
	sourceFiles <- projectSources p'
	return $ map (\src -> FileModule src (Just $ projectCabal p')) sourceFiles

-- | Enum directory modules
enumDirectory :: FilePath -> ErrorT String IO [ModuleLocation]
enumDirectory dir = do
	files <- liftException $ traverseDirectory dir
	let
		projects = filter cabalFile files
		sources = filter haskellSource files
	projMods <- liftM concat $ mapM (enumProject . project) projects
	let
		projSources = mapMaybe moduleSource projMods
		standalone = map (\f -> FileModule f Nothing) $ sources \\ projSources
	return $ projMods ++ standalone

-- | Scan project file
scanProjectFile :: [String] -> FilePath -> ErrorT String IO Project
scanProjectFile opts f = do
	proj <- (liftIO $ locateProject f) >>= maybe (throwError "Can't locate project") return
	loadProject proj

-- | Scan module
scanModule :: [String] -> ModuleLocation -> ErrorT String IO InspectedModule
scanModule opts (FileModule f p) = inspectFile opts f
scanModule opts (CabalModule c p n) = browse opts n >>= loadDocs' where
	loadDocs' m = case inspectionResult m of
		Left _ -> return m
		Right m' -> do
			m'' <- liftIO (loadDocs opts m')
			return $ m {
				inspectionResult = Right m'' }
scanModule opts (MemoryModule _) = throwError "Can't inspect memory module"

-- | Is inspected module up to date?
upToDate :: [String] -> InspectedModule -> ErrorT String IO Bool
upToDate opts (InspectedModule insp m _) = case m of
	FileModule f p -> do
		i' <- fileInspection f
		return $ i' == insp
	CabalModule c p n -> return $ InspectionFlags opts == insp
	_ -> return False

-- | Rescan inspected module
rescanModule :: [String] -> InspectedModule -> ErrorT String IO (Maybe InspectedModule)
rescanModule opts im = do
	up <- upToDate opts im
	if up
		then return Nothing
		else fmap Just $ scanModule opts (inspectionModule im)

-- | Returns new (to scan) and changed (to rescan) modules
changedModules :: Database -> [String] -> [ModuleLocation] -> ErrorT String IO [ModuleLocation]
changedModules db opts ms = do
	needUpdate <- filterM (liftM not . upToDate opts) old
	return $ new ++ map inspectionModule needUpdate
	where
		(new, old) = partitionEithers $ map split ms
		split m = maybe (Left m) Right $ M.lookup m $ databaseModules db
