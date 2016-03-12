module HsDev.Watcher.Types (
	Watched(..),
	Watcher,

	PackageDbStack, Project
	) where

import qualified System.Directory.Watcher as W
import HsDev.Project (Project)
import HsDev.PackageDb (PackageDbStack)

data Watched = WatchedProject Project [String] | WatchedPackageDb PackageDbStack [String] | WatchedModule

type Watcher = W.Watcher Watched
