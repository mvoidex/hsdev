{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module HsDev.Stack (
	stack, yaml,
	path, pathOf,
	build, buildDeps, configure,
	StackEnv(..), stackRoot, stackProject, stackConfig, stackGhc, stackSnapshot, stackLocal,
	getStackEnv, projectEnv,
	stackPackageDbStack,

	stackCompiler, stackArch,

	MaybeT(..)
	) where

import Control.Arrow
import Control.Lens (makeLenses, Lens', at, ix, lens, (^?), (^.))
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Data.Char
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Distribution.Compiler
import Distribution.System
import qualified Distribution.Text as T (display)
import System.Directory
import System.Environment
import System.FilePath
import System.Process
import System.Log.Simple (MonadLog(..))

import qualified Packages as GHC

import HsDev.Error
import HsDev.PackageDb
import qualified HsDev.Tools.Ghc.Compat as Compat
import HsDev.Scan.Browse (withPackages)
import HsDev.Util as Util

-- | Get compiler version
stackCompiler :: MonadLog m => m String
stackCompiler = do
	res <- withPackages ["-no-user-package-db"] $
		return .
		map (GHC.packageNameString &&& GHC.packageVersion) .
		fromMaybe [] .
		Compat.pkgDatabase
	let
		compiler = T.display buildCompilerFlavor
		CompilerId _ version' = buildCompilerId
		ver = maybe (T.display version') T.display $ lookup compiler res
	return $ compiler ++ "-" ++ ver

-- | Get arch for stack
stackArch :: String
stackArch = T.display buildArch

-- | Invoke stack command, we are trying to get actual stack near current hsdev executable
stack :: MonadLog m => [String] -> m String
stack cmd' = hsdevLiftIO $ do
	curExe <- liftIO getExecutablePath
	Util.withCurrentDirectory (takeDirectory curExe) $ do
		stackExe <- liftIO (findExecutable "stack") >>= maybe (hsdevError $ ToolNotFound "stack") return
		comp <- stackCompiler
		liftIO $ readProcess stackExe (cmd' ++ ["--compiler", comp, "--arch", stackArch]) ""

-- | Make yaml opts
yaml :: Maybe FilePath -> [String]
yaml Nothing = []
yaml (Just y) = ["--stack-yaml", y]

type Paths = Map String FilePath

-- | Stack path
path :: MonadLog m => Maybe FilePath -> m Paths
path mcfg = liftM (M.fromList . map breakPath . lines) $ stack ("path" : yaml mcfg) where
	breakPath :: String -> (String, FilePath)
	breakPath = second (dropWhile isSpace . drop 1) . break (== ':')

-- | Get path for
pathOf :: String -> Lens' Paths (Maybe FilePath)
pathOf = at

-- | Build stack project
build :: MonadLog m => [String] -> Maybe FilePath -> m ()
build opts mcfg = void $ stack $ "build" : (opts ++ yaml mcfg)

-- | Build only dependencies
buildDeps :: MonadLog m => Maybe FilePath -> m ()
buildDeps = build ["--only-dependencies"]

-- | Configure project
configure :: MonadLog m => Maybe FilePath -> m ()
configure = build ["--only-configure"]

data StackEnv = StackEnv {
	_stackRoot :: FilePath,
	_stackProject :: FilePath,
	_stackConfig :: FilePath,
	_stackGhc :: FilePath,
	_stackSnapshot :: FilePath,
	_stackLocal :: FilePath }

makeLenses ''StackEnv

getStackEnv :: Paths -> Maybe StackEnv
getStackEnv p = StackEnv <$>
	(p ^. pathOf "stack-root") <*>
	(p ^. pathOf "project-root") <*>
	(p ^. pathOf "config-location") <*>
	(p ^. pathOf "ghc-paths") <*>
	(p ^. pathOf "snapshot-pkg-db") <*>
	(p ^. pathOf "local-pkg-db")

-- | Projects paths
projectEnv :: MonadLog m => FilePath -> m StackEnv
projectEnv p = hsdevLiftIO $ do
	hasConfig <- liftIO $ doesFileExist yaml'
	unless hasConfig $ hsdevError $ FileNotFound yaml'
	paths' <- path (Just yaml')
	maybe (hsdevError $ ToolError "stack" "can't get paths") return $ getStackEnv paths'
	where
		yaml' = p </> "stack.yaml"

-- | Get package-db stack for stack environment
stackPackageDbStack :: Lens' StackEnv PackageDbStack
stackPackageDbStack = lens g s where
	g :: StackEnv -> PackageDbStack
	g env' = PackageDbStack $ map PackageDb [_stackLocal env', _stackSnapshot env']
	s :: StackEnv -> PackageDbStack -> StackEnv
	s env' pdbs = env' {
		_stackSnapshot = fromMaybe (_stackSnapshot env') $ pdbs ^? packageDbStack . ix 1 . packageDb,
		_stackLocal = fromMaybe (_stackLocal env') $ pdbs ^? packageDbStack . ix 0 . packageDb }
