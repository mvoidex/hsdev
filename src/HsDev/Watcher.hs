module HsDev.Watcher (
	watchProject, watchModule, watchSandbox,
	unwatchProject, unwatchModule, unwatchSandbox,
	isSource, isCabal, isConf,

	module System.Directory.Watcher,
	module HsDev.Watcher.Types
	) where

import Control.Lens (view)
import Control.Monad (void)
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
watchModule w (CabalModule cabal _ _) = watchSandbox w (sandboxStack cabal) []
watchModule _ _ = return ()

-- | Watch for sandbox
watchSandbox :: Watcher -> SandboxStack -> [String] -> IO ()
watchSandbox w sboxes opts = mapM_ watch' (sandboxStacks sboxes) where
	watch' [] = return ()
	watch' fs@(f:_) = watchTree w f isConf (WatchedSandbox fs opts)

unwatchProject :: Watcher -> Project -> IO ()
unwatchProject w proj = do
	mapM_ (unwatchTree w) dirs
	void $ unwatchDir w projDir
	where
		dirs = map ((projDir </>) . view entity) $ maybe [] sourceDirs $ view projectDescription proj
		projDir = view projectPath proj

unwatchModule :: Watcher -> ModuleLocation -> IO ()
unwatchModule w (FileModule f Nothing) = void $ unwatchDir w (takeDirectory f)
unwatchModule _ (FileModule _ (Just _)) = return ()
unwatchModule _ (CabalModule _ _ _) = return ()
unwatchModule _ _ = return ()

unwatchSandbox :: Watcher -> SandboxStack -> IO ()
unwatchSandbox _ [] = return ()
unwatchSandbox w (f:_) = void $ unwatchTree w f

isSource :: Event -> Bool
isSource (Event _ f _) = takeExtension f == ".hs"

isCabal :: Event -> Bool
isCabal (Event _ f _) = takeExtension f == ".cabal"

isConf :: Event -> Bool
isConf (Event _ f _) = takeExtension f == ".conf"
