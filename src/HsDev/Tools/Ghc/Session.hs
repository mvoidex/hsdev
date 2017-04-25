{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Tools.Ghc.Session (
	targetSession, interpretModule,

	module HsDev.Tools.Ghc.Worker
	) where

import Control.Lens
import Data.Text (Text, unpack)
import System.FilePath

import Control.Concurrent.Worker
import HsDev.Symbols.Types
import HsDev.Sandbox (getModuleOpts)
import HsDev.Tools.Ghc.Worker
import System.Directory.Paths

import qualified GHC

-- | Session for module
targetSession :: [String] -> Module -> GhcM ()
targetSession opts m = do
	opts' <- getModuleOpts opts m
	ghcSession ("-Wall" : opts')

-- | Interpret file
interpretModule :: Module -> Maybe Text -> GhcM ()
interpretModule m mcts = do
	targetSession [] m
	let
		f = preview (moduleId . moduleLocation . moduleFile) m
	case f of
		Nothing -> return ()
		Just f' -> withCurrentDirectory (takeDirectory $ view path f') $ do
			t <- makeTarget (over path takeFileName f') mcts
			loadTargets [t]
			GHC.setContext [GHC.IIModule $ GHC.mkModuleName $ unpack $ view (moduleId . moduleName) m]
