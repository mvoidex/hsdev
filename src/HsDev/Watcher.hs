module HsDev.Watcher (
	watchProject, watchModule, watchSandbox,
	isSource, isCabal, isConf,

	module System.Directory.Watcher,
	module HsDev.Watcher.Types
	) where

import Control.Lens (view)
import System.FilePath (takeDirectory, takeExtension, (</>))

import System.Directory.Watcher hiding (Watcher)
import HsDev.Project
import HsDev.Symbols
import HsDev.Watcher.Types

-- | Watch for project sources changes
watchProject :: Watcher -> Project -> [String] -> IO ()
watchProject w proj opts = do
	mapM_ (\dir -> watchTree w dir isSource (WatchedProject proj opts)) dirs
	watchDir w projDir isCabal (WatchedProject proj opts)
	where
		dirs = map ((projDir </>) . view entity) $ maybe [] sourceDirs $ view projectDescription proj
		projDir = view projectPath proj

-- | Watch for standalone source
watchModule :: Watcher -> ModuleLocation -> IO ()
watchModule w (FileModule f Nothing) = watchDir w (takeDirectory f) isSource WatchedModule
watchModule w (FileModule _ (Just proj)) = watchProject w proj []
watchModule w (CabalModule cabal _ _) = watchSandbox w cabal []
watchModule _ _ = return ()

-- | Watch for sandbox
watchSandbox :: Watcher -> Cabal -> [String] -> IO ()
watchSandbox _ Cabal _ = return ()
watchSandbox w (Sandbox f) opts = watchTree w f isConf (WatchedSandbox (Sandbox f) opts)

isSource :: Event -> Bool
isSource (Event _ f _) = takeExtension f == ".hs"

isCabal :: Event -> Bool
isCabal (Event _ f _) = takeExtension f == ".cabal"

isConf :: Event -> Bool
isConf (Event _ f _) = takeExtension f == ".conf"
