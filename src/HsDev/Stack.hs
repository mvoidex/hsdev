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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Distribution.Compiler
import Distribution.System
import qualified Distribution.Text as T (display)
import System.Directory
import System.Environment
import System.FilePath

import qualified GHC
import qualified Packages as GHC

import HsDev.Error
import HsDev.PackageDb
import HsDev.Tools.Ghc.Worker (GhcM, tmpSession)
import qualified HsDev.Tools.Ghc.Compat as Compat
import HsDev.Util as Util
import HsDev.Tools.Base (runTool_)

-- | Get compiler version
stackCompiler :: GhcM String
stackCompiler = do
	tmpSession ["-no-user-package-db"]
	df <- GHC.getSessionDynFlags
	let
		res =
			map (GHC.packageNameString &&& GHC.packageVersion) .
			fromMaybe [] .
			Compat.pkgDatabase $ df
		compiler = T.display buildCompilerFlavor
		CompilerId _ version' = buildCompilerId
		ver = maybe (T.display version') T.display $ lookup compiler res
	return $ compiler ++ "-" ++ ver

-- | Get arch for stack
stackArch :: String
stackArch = T.display buildArch

-- | Invoke stack command, we are trying to get actual stack near current hsdev executable
stack :: [String] -> GhcM String
stack cmd' = hsdevLiftIO $ do
	curExe <- liftIO getExecutablePath
	stackExe <- Util.withCurrentDirectory (takeDirectory curExe) $
		liftIO (findExecutable "stack") >>= maybe (hsdevError $ ToolNotFound "stack") return
	comp <- stackCompiler
	liftIO $ runTool_ stackExe (["--compiler", comp, "--arch", stackArch] ++ cmd')

-- | Make yaml opts
yaml :: Maybe FilePath -> [String]
yaml Nothing = []
yaml (Just y) = ["--stack-yaml", y]

type Paths = Map String FilePath

-- | Stack path
path :: Maybe FilePath -> GhcM Paths
path mcfg = liftM (M.fromList . map breakPath . lines) $ stack ("path" : yaml mcfg) where
	breakPath :: String -> (String, FilePath)
	breakPath = second (dropWhile isSpace . drop 1) . break (== ':')

-- | Get path for
pathOf :: String -> Lens' Paths (Maybe FilePath)
pathOf = at

-- | Build stack project
build :: [String] -> Maybe FilePath -> GhcM ()
build opts mcfg = void $ stack $ "build" : (opts ++ yaml mcfg)

-- | Build only dependencies
buildDeps :: Maybe FilePath -> GhcM ()
buildDeps = build ["--only-dependencies"]

-- | Configure project
configure :: Maybe FilePath -> GhcM ()
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
projectEnv :: FilePath -> GhcM StackEnv
projectEnv p = hsdevLiftIO $ Util.withCurrentDirectory p $ do
	paths' <- path Nothing
	maybe (hsdevError $ ToolError "stack" ("can't get paths for " ++ p)) return $ getStackEnv paths'

-- | Get package-db stack for stack environment
stackPackageDbStack :: Lens' StackEnv PackageDbStack
stackPackageDbStack = lens g s where
	g :: StackEnv -> PackageDbStack
	g env' = PackageDbStack $ map PackageDb [_stackLocal env', _stackSnapshot env']
	s :: StackEnv -> PackageDbStack -> StackEnv
	s env' pdbs = env' {
		_stackSnapshot = fromMaybe (_stackSnapshot env') $ pdbs ^? packageDbStack . ix 1 . packageDb,
		_stackLocal = fromMaybe (_stackLocal env') $ pdbs ^? packageDbStack . ix 0 . packageDb }
