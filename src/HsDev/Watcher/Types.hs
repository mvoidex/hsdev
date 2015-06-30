module HsDev.Watcher.Types (
	Watched(..),
	Watcher
	) where

import qualified System.Directory.Watcher as W
import HsDev.Project (Project)
import HsDev.Cabal (Cabal)

data Watched = WatchedProject Project | WatchedSandbox Cabal | WatchedModule

type Watcher = W.Watcher Watched
