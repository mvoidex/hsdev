{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Tools.Ghc.Session (
	ghcSession, ghciSession, haddockSession, targetSession, interpretModule,

	module HsDev.Tools.Ghc.Worker
	) where

import Control.Lens
import Data.Text (unpack)
import System.FilePath

import Control.Concurrent.Worker
import HsDev.Symbols.Types
import HsDev.Sandbox (getModuleOpts)
import HsDev.Tools.Ghc.Worker

import qualified GHC

-- | Get ghc session
ghcSession :: [String] -> GhcM ()
ghcSession = workerSession . SessionGhc

-- | Get ghci session
ghciSession :: GhcM ()
ghciSession = workerSession SessionGhci

-- | Get haddock session with flags
haddockSession :: [String] -> GhcM ()
haddockSession opts = ghcSession ("-haddock" : opts)

-- | Session for module
targetSession :: [String] -> Module -> GhcM ()
targetSession opts m = do
	opts' <- getModuleOpts opts m
	ghcSession ("-Wall" : opts')

-- | Interpret file
interpretModule :: Module -> Maybe String -> GhcM ()
interpretModule m mcts = do
	targetSession [] m
	let
		f = preview (moduleId . moduleLocation . moduleFile) m
	case f of
		Nothing -> return ()
		Just f' -> withCurrentDirectory (takeDirectory f') $ do
			t <- makeTarget (takeFileName f') mcts
			loadTargets [t]
			GHC.setContext [GHC.IIModule $ GHC.mkModuleName $ unpack $ view (moduleId . moduleName) m]
