{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Tools.Ghc.Session (
	targetSession, interpretModule,

	module HsDev.Tools.Ghc.Worker
	) where

import Control.Lens
import Data.Maybe (isJust)
import Data.Text (Text, unpack)
import System.Log.Simple

import Control.Concurrent.Worker
import System.Directory.Paths
import HsDev.Symbols.Types
import HsDev.Sandbox (getModuleOpts)
import HsDev.Tools.Ghc.Worker

import qualified GHC

-- | Session for module
targetSession :: [String] -> Module -> GhcM ()
targetSession opts m = do
	(pdbs, opts') <- getModuleOpts opts m
	ghcSession pdbs ("-Wall" : opts')

-- | Interpret file
interpretModule :: Module -> Maybe Text -> GhcM ()
interpretModule m mcts
	| isJust mpath = do
		let
			rootDir = maybe (takeDir fpath) (view projectPath) (m ^? moduleId . moduleLocation . moduleProject . _Just)
		withCurrentDirectory (view path rootDir) $ do
			t <- makeTarget (relPathTo rootDir fpath) mcts
			loadTargets [t]
			GHC.setContext [GHC.IIModule . GHC.mkModuleName . unpack . view (moduleId . moduleName) $ m]
	| otherwise = return ()
	where
		mpath = m ^? moduleId . moduleLocation . moduleFile
		Just fpath = mpath
