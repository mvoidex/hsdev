{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module HsDev.Sandbox (
	SandboxType(..), Sandbox(..), sandboxType, sandbox,
	isSandbox, guessSandboxType, sandboxFromPath,
	findSandbox, searchSandbox, sandboxPackageDbStack, searchPackageDbStack, restorePackageDbStack,

	-- * cabal-sandbox util
	cabalSandboxLib, cabalSandboxPackageDb,

	getModuleOpts
	) where

import Control.Arrow
import Control.DeepSeq (NFData(..))
import Control.Monad.Trans.Maybe
import Control.Monad.Except
import Control.Lens (view, makeLenses)
import Data.Aeson
import Data.Maybe (isJust, fromMaybe)
import Data.List ((\\))
import Data.String (fromString)
import qualified Data.Text as T (unpack)
import Distribution.Compiler
import Distribution.System
import qualified Distribution.Text as T (display)
import System.FilePath
import System.Directory
import System.Log.Simple (MonadLog(..))
import qualified System.Log.Simple as Log

import System.Directory.Paths
import HsDev.PackageDb
import HsDev.Scan.Browse (withPackages, browsePackages)
import HsDev.Stack
import HsDev.Symbols (moduleOpts)
import HsDev.Symbols.Types (Module(..), ModuleLocation(..), moduleLocation)
import HsDev.Tools.Ghc.Compat as Compat
import HsDev.Util (searchPath, directoryContents)

import qualified Packages as GHC

data SandboxType = CabalSandbox | StackWork deriving (Eq, Ord, Read, Show, Enum, Bounded)

data Sandbox = Sandbox { _sandboxType :: SandboxType, _sandbox :: FilePath } deriving (Eq, Ord)

makeLenses ''Sandbox

instance NFData SandboxType where
	rnf CabalSandbox = ()
	rnf StackWork = ()

instance NFData Sandbox where
	rnf (Sandbox t p) = rnf t `seq` rnf p

instance Show Sandbox where
	show (Sandbox _ p) = p

instance ToJSON Sandbox where
	toJSON (Sandbox _ p) = toJSON p

instance FromJSON Sandbox where
	parseJSON = withText "sandbox" sandboxPath where
		sandboxPath = maybe (fail "Not a sandbox") return . sandboxFromPath . T.unpack

instance Paths Sandbox where
	paths f (Sandbox st p) = Sandbox st <$> f p

isSandbox :: FilePath -> Bool
isSandbox = isJust . guessSandboxType

guessSandboxType :: FilePath -> Maybe SandboxType
guessSandboxType fpath
	| takeFileName fpath == ".cabal-sandbox" = Just CabalSandbox
	| takeFileName fpath == ".stack-work" = Just StackWork
	| otherwise = Nothing

sandboxFromPath :: FilePath -> Maybe Sandbox
sandboxFromPath fpath = Sandbox <$> guessSandboxType fpath <*> pure fpath

-- | Find sandbox in path
findSandbox :: FilePath -> IO (Maybe Sandbox)
findSandbox fpath = do
	fpath' <- canonicalize fpath
	isDir <- doesDirectoryExist fpath'
	if isDir
		then do
			dirs <- liftM (fpath' :) $ directoryContents fpath'
			return $ msum $ map sandboxFromDir dirs
		else return Nothing
	where
		sandboxFromDir :: FilePath -> Maybe Sandbox
		sandboxFromDir fdir
			| takeFileName fdir == "stack.yaml" = sandboxFromPath (takeDirectory fdir </> ".stack-work")
			| otherwise = sandboxFromPath fdir

-- | Search sandbox by parent directory
searchSandbox :: FilePath -> IO (Maybe Sandbox)
searchSandbox p = runMaybeT $ searchPath p (MaybeT . findSandbox)

-- | Get package-db stack for sandbox
sandboxPackageDbStack :: MonadLog m => Sandbox -> m PackageDbStack
sandboxPackageDbStack (Sandbox CabalSandbox fpath) = do
	dir <- cabalSandboxPackageDb
	return $ PackageDbStack [PackageDb $ fpath </> dir]
sandboxPackageDbStack (Sandbox StackWork fpath) = liftM (view stackPackageDbStack) $ projectEnv $ takeDirectory fpath

-- | Search package-db stack with user-db as default
searchPackageDbStack :: MonadLog m => FilePath -> m PackageDbStack
searchPackageDbStack p = do
	mbox <- liftIO $ searchSandbox p
	case mbox of
		Nothing -> return userDb
		Just sbox -> sandboxPackageDbStack sbox

-- | Restore package-db stack by package-db
restorePackageDbStack :: MonadLog m => PackageDb -> m PackageDbStack
restorePackageDbStack GlobalDb = return globalDb
restorePackageDbStack UserDb = return userDb
restorePackageDbStack (PackageDb p) = liftM (fromMaybe $ fromPackageDb p) $ runMaybeT $ do
	sbox <- MaybeT $ liftIO $ searchSandbox p
	lift $ sandboxPackageDbStack sbox

-- | Get actual sandbox build path: <arch>-<platform>-<compiler>-<version>
cabalSandboxLib :: MonadLog m => m FilePath
cabalSandboxLib = do
	res <- withPackages ["-no-user-package-db"] $
		return .
		map (GHC.packageNameString &&& GHC.packageVersion) .
		fromMaybe [] .
		Compat.pkgDatabase
	let
		compiler = T.display buildCompilerFlavor
		CompilerId _ version = buildCompilerId
		ver = maybe (T.display version) T.display $ lookup compiler res
	return $ T.display buildPlatform ++ "-" ++ compiler ++ "-" ++ ver

-- | Get sandbox package-db: <arch>-<platform>-<compiler>-<version>-packages.conf.d
cabalSandboxPackageDb :: MonadLog m => m FilePath
cabalSandboxPackageDb = liftM (++ "-packages.conf.d") cabalSandboxLib

-- | Options for GHC for module and project
getModuleOpts :: MonadLog m => [String] -> Module -> m [String]
getModuleOpts opts m = do
	pdbs <- case view moduleLocation m of
		FileModule fpath p -> searchPackageDbStack fpath
		InstalledModule pdb _ _ -> restorePackageDbStack pdb
		ModuleSource _ -> return userDb
	pkgs <- browsePackages opts pdbs
	return $ concat [
		packageDbStackOpts pdbs,
		moduleOpts pkgs m,
		opts]
