module HsDev.Watcher (
	watchProject, watchModule, watchSandbox,
	isSource, isCabal, isConf,

	module System.Directory.Watcher,
	module HsDev.Watcher.Types
	) where

import Control.Lens (view)
import System.FilePath (takeDirectory, takeExtension)

import System.Directory.Watcher hiding (Watcher)
import HsDev.Project
import HsDev.Symbols
import HsDev.Watcher.Types

-- | Watch for project sources changes
watchProject :: Watcher -> Project -> IO ()
watchProject w proj = do
	mapM_ (\dir -> watchTree w dir isSource (WatchedProject proj)) dirs
	watchDir w (view projectPath proj) isCabal (WatchedProject proj)
	where
		dirs = map (view entity) $ maybe [] sourceDirs $ view projectDescription proj

-- | Watch for standalone source
watchModule :: Watcher -> ModuleLocation -> IO ()
watchModule w (FileModule f Nothing) = watchDir w (takeDirectory f) isSource WatchedModule
watchModule w (FileModule _ (Just proj)) = watchProject w proj
watchModule w (CabalModule cabal _ _) = watchSandbox w cabal
watchModule _ _ = return ()

-- | Watch for sandbox
watchSandbox :: Watcher -> Cabal -> IO ()
watchSandbox _ Cabal = return ()
watchSandbox w (Sandbox f) = watchTree w f isConf (WatchedSandbox $ Sandbox f)

isSource :: Event -> Bool
isSource (Event _ f _) = takeExtension f == ".hs"

isCabal :: Event -> Bool
isCabal (Event _ f _) = takeExtension f == ".cabal"

isConf :: Event -> Bool
isConf (Event _ f _) = takeExtension f == ".conf"
