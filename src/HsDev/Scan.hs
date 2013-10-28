module HsDev.Scan (
	-- * Enumerate functions
	enumCabal, enumProject, enumDirectory,

	-- * Scan
	scanProjectFile,
	scanModule, upToDate, rescanModule, changedModule, changedModules
	) where

import Control.Applicative
import Control.Exception
import Control.Monad.Error
import Data.Either
import Data.List
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Monoid
import Data.Traversable (traverse)

import HsDev.Symbols
import HsDev.Database
import HsDev.Tools.GhcMod
import HsDev.Tools.GhcMod.InferType (inferTypes)
import HsDev.Tools.HDocs
import HsDev.Inspect
import HsDev.Project
import HsDev.Util

-- | Enum cabal modules
enumCabal :: [String] -> Cabal -> ErrorT String IO [ModuleLocation]
enumCabal opts cabal = liftM (map $ CabalModule cabal Nothing) $ list opts

-- | Enum project sources
enumProject :: Project -> ErrorT String IO [([String], ModuleLocation)]
enumProject p = do
	p' <- loadProject p
	srcs <- projectSources p'
	return [(extensionsOpts (extensions src), FileModule (entity src) (Just $ projectCabal p')) | src <- srcs]

-- | Enum directory modules
enumDirectory :: FilePath -> ErrorT String IO [([String], ModuleLocation)]
enumDirectory dir = do
	files <- liftException $ traverseDirectory dir
	let
		projects = filter cabalFile files
		sources = filter haskellSource files
	projMods <- liftM concat $ mapM (enumProject . project) projects
	let
		projSources = mapMaybe (moduleSource . snd) projMods
		standalone = map (\f -> FileModule f Nothing) $ sources \\ projSources
	return $ projMods ++ [([], s) | s <- standalone]

-- | Scan project file
scanProjectFile :: [String] -> FilePath -> ErrorT String IO Project
scanProjectFile opts f = do
	proj <- (liftIO $ locateProject f) >>= maybe (throwError "Can't locate project") return
	loadProject proj

-- | Scan module
scanModule :: [String] -> ModuleLocation -> ErrorT String IO InspectedModule
scanModule opts (FileModule f p) = inspectFile opts f >>= traverse (inferTypes opts)
scanModule opts (CabalModule c p n) = browse opts n >>= traverse (liftIO . loadDocs opts)
scanModule opts (MemoryModule _) = throwError "Can't inspect memory module"

-- | Is inspected module up to date?
upToDate :: [String] -> InspectedModule -> ErrorT String IO Bool
upToDate opts (Inspected insp m _) = case m of
	FileModule f p -> liftM (== insp) $ fileInspection f opts
	CabalModule c p n -> liftM (== insp) $ browseInspection opts n
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
