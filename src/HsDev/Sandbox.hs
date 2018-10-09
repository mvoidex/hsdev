{-# LANGUAGE OverloadedStrings #-}

module HsDev.Sandbox (
	Sandbox(..), sandboxType, sandbox,
	isSandbox, guessSandboxType, sandboxFromPath,
	findSandbox, searchSandbox, searchSandboxes,
	projectSandbox, sandboxPackageDbStack, searchPackageDbStack, restorePackageDbStack,

	-- * package-db
	userPackageDb,

	-- * cabal-sandbox util
	cabalSandboxPackageDb,

	getModuleOpts, getProjectTargetOpts,

	getProjectSandbox,
	getProjectPackageDbStack
	) where

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Except
import Control.Lens (view)
import Data.List (find, intercalate)
import Data.Maybe (isJust, fromMaybe, catMaybes)
import Data.Maybe.JustIf
import System.Directory (getAppUserDataDirectory, doesDirectoryExist)
import System.FilePath
import System.Log.Simple (MonadLog(..))
import Text.Format

import System.Directory.Paths
import HsDev.Error
import HsDev.PackageDb
import HsDev.Project.Types
import HsDev.Scan.Browse (browsePackages)
import HsDev.Stack hiding (path)
import HsDev.Symbols (moduleOpts, projectTargetOpts)
import HsDev.Symbols.Types (moduleId, Module(..), ModuleLocation(..), moduleLocation)
import HsDev.Tools.Ghc.Worker (GhcM)
import HsDev.Tools.Ghc.System (buildPath)
import HsDev.Util (searchPath, directoryContents, cabalFile)

isSandbox :: Path -> Bool
isSandbox = isJust . guessSandboxType

guessSandboxType :: Path -> Maybe BuildTool
guessSandboxType fpath
	| takeFileName (view path fpath) == ".cabal-sandbox" = Just CabalTool
	| takeFileName (view path fpath) == ".stack-work" = Just StackTool
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

-- | Search sandboxes up from current directory
searchSandboxes :: Path -> IO [Sandbox]
searchSandboxes p = do
	mcabal <- searchFor CabalTool ".cabal-sandbox" ".cabal-sandbox"
	mstack <- searchFor StackTool "stack.yaml" ".stack-work"
	return $ catMaybes [mcabal, mstack]
	where
		searchFor :: BuildTool -> FilePath -> FilePath -> IO (Maybe Sandbox)
		searchFor tool lookFor sandboxDir = runMaybeT $ do
			root <- searchPath (view path p) (MaybeT . getRoot)
			return $ Sandbox tool $ fromFilePath (takeDirectory root </> sandboxDir)
			where
				getRoot = directoryContents >=> return . find ((== lookFor) . takeFileName)

-- | Get project sandbox: search up for .cabal, then search for stack.yaml in current directory and cabal sandbox in current + parents
projectSandbox :: BuildTool -> Path -> IO (Maybe Sandbox)
projectSandbox tool fpath = runMaybeT $ do
	p <- searchPath (view path fpath) (MaybeT . getCabalFile)
	sboxes <- liftIO $ searchSandboxes (fromFilePath $ takeDirectory p)
	MaybeT $ return $ find ((== tool) . view sandboxType) sboxes
	where
		getCabalFile = directoryContents >=> return . find cabalFile

-- | Get package-db stack for sandbox
sandboxPackageDbStack :: Sandbox -> GhcM PackageDbStack
sandboxPackageDbStack (Sandbox CabalTool fpath) = do
	dir <- cabalSandboxPackageDb $ view path fpath
	return $ PackageDbStack [PackageDb $ fromFilePath dir]
sandboxPackageDbStack (Sandbox StackTool fpath) = liftM (view stackPackageDbStack) $ projectEnv $ takeDirectory (view path fpath)

-- | Search package-db stack with user-db as default
searchPackageDbStack :: BuildTool -> Path -> GhcM PackageDbStack
searchPackageDbStack tool p = do
	mbox <- liftIO $ projectSandbox tool p
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

-- | User package-db: <arch>-<os>-<version>
userPackageDb :: GhcM FilePath
userPackageDb = do
	root <- liftIO $ getAppUserDataDirectory "ghc"
	dir <- buildPath "{arch}-{os}-{version}"
	return $ root </> dir

-- | Get sandbox package-db: <arch>-<os>-<compiler>-<version>-packages.conf.d
cabalSandboxPackageDb :: FilePath -> GhcM FilePath
cabalSandboxPackageDb root = do
	dirs <- mapM (fmap (root </>) . buildPath) [
		"{arch}-{os}-{compiler}-{version}-packages.conf.d",
		"{arch}-{os/cabal}-{compiler}-{version}-packages.conf.d"]
	mdir <- liftM msum $ forM dirs $ \dir -> do
		justIf dir <$> liftIO (doesDirectoryExist dir)
	case mdir of
		Nothing -> hsdevError $ OtherError $ unlines [
			"No suitable package-db found in sandbox, is it configured?",
			"Searched in: {}" ~~ intercalate ", " dirs]
		Just dir -> return dir

-- | Options for GHC for module and project
getModuleOpts :: [String] -> Module -> GhcM (PackageDbStack, [String])
getModuleOpts opts m = do
	pdbs <- case view (moduleId . moduleLocation) m of
		FileModule fpath mproj -> searchPackageDbStack (maybe CabalTool (view projectBuildTool) mproj) fpath
		InstalledModule{} -> return userDb
		_ -> return userDb
	pkgs <- browsePackages opts pdbs
	return $ (pdbs, concat [
		moduleOpts pkgs m,
		opts])

-- | Options for GHC for project target
getProjectTargetOpts :: [String] -> Project -> Info -> GhcM (PackageDbStack, [String])
getProjectTargetOpts opts proj t = do
	pdbs <- searchPackageDbStack (view projectBuildTool proj) (view projectPath proj)
	pkgs <- browsePackages opts pdbs
	return $ (pdbs, concat [
		projectTargetOpts pkgs proj t,
		opts])

-- | Get sandbox of project (if any)
getProjectSandbox :: MonadLog m => Project -> m (Maybe Sandbox)
getProjectSandbox p = liftIO . projectSandbox (view projectBuildTool p) . view projectPath $ p

-- | Get project package-db stack
getProjectPackageDbStack :: Project -> GhcM PackageDbStack
getProjectPackageDbStack = getProjectSandbox >=> maybe (return userDb) sandboxPackageDbStack
