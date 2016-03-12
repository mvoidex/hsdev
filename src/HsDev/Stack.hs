{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module HsDev.Stack (
	stack, path, pathOf,
	StackEnv(..), stackRoot, stackProject, stackConfig, stackGhc, stackSnapshot, stackLocal,
	getStackEnv, projectEnv,
	stackPackageDbStack,

	MaybeT(..)
	) where

import Control.Arrow
import Control.Lens (makeLenses, Lens', at, ix, lens, (^?), (^.), view)
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Data.Char
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import System.Directory
import System.FilePath
import System.Process

import HsDev.PackageDb
import HsDev.Project

-- | Invoke stack command
stack :: [String] -> 	MaybeT IO String
stack cmd = do
	stackExe <- MaybeT $ findExecutable "stack"
	liftIO $ readProcess stackExe cmd ""

type Paths = Map String FilePath

-- | Stack path
path :: Maybe FilePath -> MaybeT IO Paths
path mcfg = liftM (M.fromList . map breakPath . lines) $ stack ("path" : maybe [] (\cfg -> ["--stack-yaml", cfg]) mcfg) where
	breakPath :: String -> (String, FilePath)
	breakPath = second (dropWhile isSpace . drop 1) . break (== ':')

-- | Get path for
pathOf :: String -> Lens' Paths (Maybe FilePath)
pathOf = at

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
	(p ^. pathOf "global-stack-root") <*>
	(p ^. pathOf "project-root") <*>
	(p ^. pathOf "config-location") <*>
	(p ^. pathOf "ghc-paths") <*>
	(p ^. pathOf "snapshot-pkg-db") <*>
	(p ^. pathOf "local-pkg-db")

-- | Projects paths
projectEnv :: FilePath -> MaybeT IO StackEnv
projectEnv p = do
	hasConfig <- liftIO $ doesFileExist yaml
	guard hasConfig
	paths' <- path (Just yaml)
	MaybeT $ return $ getStackEnv paths'
	where
		yaml = p </> "stack.yaml"

-- | Get package-db stack for stack environment
stackPackageDbStack :: Lens' StackEnv PackageDbStack
stackPackageDbStack = lens g s where
	g :: StackEnv -> PackageDbStack
	g env' = PackageDbStack $ map PackageDb [_stackLocal env', _stackSnapshot env']
	s :: StackEnv -> PackageDbStack -> StackEnv
	s env' pdbs = env' {
		_stackSnapshot = fromMaybe (_stackSnapshot env') $ pdbs ^? packageDbStack . ix 1 . packageDb,
		_stackLocal = fromMaybe (_stackLocal env') $ pdbs ^? packageDbStack . ix 0 . packageDb }
