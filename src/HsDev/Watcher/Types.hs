module HsDev.Watcher.Types (
	Watched(..),
	Watcher,

	Cabal, Project
	) where

import qualified System.Directory.Watcher as W
import HsDev.Project (Project)
import HsDev.Cabal (Cabal, SandboxStack)

data Watched = WatchedProject Project [String] | WatchedSandbox SandboxStack [String] | WatchedModule

type Watcher = W.Watcher Watched
