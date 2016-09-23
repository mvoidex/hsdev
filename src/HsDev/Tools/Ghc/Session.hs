{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Tools.Ghc.Session (
	ghcSession, ghciSession, haddockSession, targetSession,

	module HsDev.Tools.Ghc.Worker
	) where

import Control.Concurrent.Worker
import HsDev.Symbols.Types (Module(..))
import HsDev.Sandbox (getModuleOpts)
import HsDev.Tools.Ghc.Worker

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
