{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module HsDev.Sandbox (
	SandboxType(..), Sandbox(..), sandboxType, sandbox,
	isSandbox, guessSandboxType, sandboxFromPath,
	findSandbox, searchSandbox, projectSandbox, sandboxPackageDbStack, searchPackageDbStack, restorePackageDbStack,

	-- * package-db
	userPackageDb,

	-- * cabal-sandbox util
	cabalSandboxLib, cabalSandboxPackageDb,

	getModuleOpts, getProjectTargetOpts,

	getProjectSandbox,
	getProjectPackageDbStack
	) where

import Control.Applicative
import Control.Arrow
import Control.DeepSeq (NFData(..))
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Except
import Control.Lens (view, makeLenses)
import Data.Aeson
import Data.List (find)
import Data.Maybe (isJust, fromMaybe)
import qualified Data.Text as T (unpack)
import Distribution.Compiler
import Distribution.System
import qualified Distribution.Text as T (display)
import System.FilePath
import System.Log.Simple (MonadLog(..))
import Text.Format

import System.Directory.Paths
import HsDev.Display
import HsDev.PackageDb
import HsDev.Project.Types
import HsDev.Scan.Browse (browsePackages)
import HsDev.Stack hiding (path)
import HsDev.Symbols (moduleOpts, projectTargetOpts)
import HsDev.Symbols.Types (moduleId, Module(..), ModuleLocation(..), moduleLocation)
import HsDev.Tools.Ghc.Worker (GhcM, tmpSession)
import HsDev.Tools.Ghc.Compat as Compat
import HsDev.Tools.Ghc.System (buildPath)
import HsDev.Util (searchPath, directoryContents, cabalFile)

import qualified GHC
import qualified Packages as GHC

data SandboxType = CabalSandbox | StackWork deriving (Eq, Ord, Read, Show, Enum, Bounded)

data Sandbox = Sandbox { _sandboxType :: SandboxType, _sandbox :: Path } deriving (Eq, Ord)

makeLenses ''Sandbox

instance NFData SandboxType where
	rnf CabalSandbox = ()
	rnf StackWork = ()

instance NFData Sandbox where
	rnf (Sandbox t p) = rnf t `seq` rnf p

instance Show Sandbox where
	show (Sandbox _ p) = T.unpack p

instance Display Sandbox where
	display (Sandbox _ fpath) = display fpath
	displayType (Sandbox CabalSandbox _) = "cabal-sandbox"
	displayType (Sandbox StackWork _) = "stack-work"

instance Formattable Sandbox where
	formattable = formattable . display

instance ToJSON Sandbox where
	toJSON (Sandbox _ p) = toJSON p

instance FromJSON Sandbox where
	parseJSON = withText "sandbox" sandboxPath where
		sandboxPath = maybe (fail "Not a sandbox") return . sandboxFromPath

instance Paths Sandbox where
	paths f (Sandbox st p) = Sandbox st <$> paths f p

isSandbox :: Path -> Bool
isSandbox = isJust . guessSandboxType

guessSandboxType :: Path -> Maybe SandboxType
guessSandboxType fpath
	| takeFileName (view path fpath) == ".cabal-sandbox" = Just CabalSandbox
	| takeFileName (view path fpath) == ".stack-work" = Just StackWork
	| otherwise = Nothing

sandboxFromPath :: Path -> Maybe Sandbox
sandboxFromPath fpath = Sandbox <$> guessSandboxType fpath <*> pure fpath

-- | Find sandbox in path
findSandbox :: Path -> IO (Maybe Sandbox)
findSandbox fpath = do
	fpath' <- canonicalize fpath
	isDir <- dirExists fpath'
	if isDir
		then do
			dirs <- liftM ((fpath' :) . map fromFilePath) $ directoryContents (view path fpath')
			return $ msum $ map sandboxFromDir dirs
		else return Nothing
	where
		sandboxFromDir :: Path -> Maybe Sandbox
		sandboxFromDir fdir
			| takeFileName (view path fdir) == "stack.yaml" = sandboxFromPath (fromFilePath (takeDirectory (view path fdir) </> ".stack-work"))
			| otherwise = sandboxFromPath fdir

-- | Search sandbox by parent directory
searchSandbox :: Path -> IO (Maybe Sandbox)
searchSandbox p = runMaybeT $ searchPath (view path p) (MaybeT . findSandbox . fromFilePath)

-- | Get project sandbox: search up for .cabal, then search for stack.yaml in current directory and cabal sandbox in current + parents
projectSandbox :: Path -> IO (Maybe Sandbox)
projectSandbox fpath = runMaybeT $ do
	p <- searchPath (view path fpath) (MaybeT . getCabalFile)
	MaybeT (findSandbox $ fromFilePath p) <|> searchPath p (MaybeT . findSbox')
	where
		getCabalFile = directoryContents >=> return . find cabalFile
		findSbox' = directoryContents >=> return . msum . map (sandboxFromPath . fromFilePath)

-- | Get package-db stack for sandbox
sandboxPackageDbStack :: Sandbox -> GhcM PackageDbStack
sandboxPackageDbStack (Sandbox CabalSandbox fpath) = do
	dir <- cabalSandboxPackageDb
	return $ PackageDbStack [PackageDb $ fromFilePath $ view path fpath </> dir]
sandboxPackageDbStack (Sandbox StackWork fpath) = liftM (view stackPackageDbStack) $ projectEnv $ takeDirectory (view path fpath)

-- | Search package-db stack with user-db as default
searchPackageDbStack :: Path -> GhcM PackageDbStack
searchPackageDbStack p = do
	mbox <- liftIO $ projectSandbox p
	case mbox of
		Nothing -> return userDb
		Just sbox -> sandboxPackageDbStack sbox

-- | Restore package-db stack by package-db
restorePackageDbStack :: PackageDb -> GhcM PackageDbStack
restorePackageDbStack GlobalDb = return globalDb
restorePackageDbStack UserDb = return userDb
restorePackageDbStack (PackageDb p) = liftM (fromMaybe $ fromPackageDbs [p]) $ runMaybeT $ do
	sbox <- MaybeT $ liftIO $ searchSandbox p
	lift $ sandboxPackageDbStack sbox

globalPackageDb :: GhcM FilePath
globalPackageDb = return 

-- | User package-db: <arch>-<platform>-<version>
userPackageDb :: GhcM FilePath
userPackageDb = do
	root <- liftIO $ getAppUserDataDirectory "ghc"
	dir <- buildPath "{arch}-{platform}-{version}"
	return $ root </> dir

-- | Get actual sandbox build path: <arch>-<platform>-<compiler>-<version>
cabalSandboxLib :: GhcM FilePath
cabalSandboxLib = buildPath "{arch}-{platform}-{compiler}-{version}"

-- | Get sandbox package-db: <arch>-<platform>-<compiler>-<version>-packages.conf.d
cabalSandboxPackageDb :: GhcM FilePath
cabalSandboxPackageDb = liftM (++ "-packages.conf.d") cabalSandboxLib

-- | Options for GHC for module and project
getModuleOpts :: [String] -> Module -> GhcM (PackageDbStack, [String])
getModuleOpts opts m = do
	pdbs <- case view (moduleId . moduleLocation) m of
		FileModule fpath _ -> searchPackageDbStack fpath
		InstalledModule _ _ _ -> return userDb
		_ -> return userDb
	pkgs <- browsePackages opts pdbs
	return $ (pdbs, concat [
		moduleOpts pkgs m,
		opts])

-- | Options for GHC for project target
getProjectTargetOpts :: [String] -> Project -> Info -> GhcM (PackageDbStack, [String])
getProjectTargetOpts opts proj t = do
	pdbs <- searchPackageDbStack $ view projectPath proj
	pkgs <- browsePackages opts pdbs
	return $ (pdbs, concat [
		projectTargetOpts pkgs proj t,
		opts])

-- | Get sandbox of project (if any)
getProjectSandbox :: MonadLog m => Project -> m (Maybe Sandbox)
getProjectSandbox = liftIO . projectSandbox . view projectPath

-- | Get project package-db stack
getProjectPackageDbStack :: Project -> GhcM PackageDbStack
getProjectPackageDbStack = getProjectSandbox >=> maybe (return userDb) sandboxPackageDbStack
