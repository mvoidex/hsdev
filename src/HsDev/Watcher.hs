module HsDev.Watcher (
	watchProject, watchModule, watchPackageDb, watchPackageDbStack,
	unwatchProject, unwatchModule, unwatchPackageDb,
	isSource, isCabal, isConf,

	module System.Directory.Watcher,
	module HsDev.Watcher.Types
	) where

import Control.Lens (view)
import System.FilePath (takeDirectory, takeExtension, (</>))

import System.Directory.Watcher hiding (Watcher)
import System.Directory.Paths
import HsDev.Project
import HsDev.Symbols
import HsDev.Watcher.Types
import HsDev.Util

-- | Watch for project sources changes
watchProject :: Watcher -> Project -> [String] -> IO ()
watchProject w proj opts = do
	mapM_ (\dir -> watchTree w dir isSource (WatchedProject proj opts)) dirs
	watchDir w projDir isCabal (WatchedProject proj opts)
	where
		dirs = map ((projDir </>) . view (entity . path)) $ maybe [] sourceDirs $ view projectDescription proj
		projDir = view (projectPath . path) proj

-- | Watch for standalone source
watchModule :: Watcher -> ModuleLocation -> IO ()
watchModule w (FileModule f Nothing) = watchDir w (takeDirectory $ view path f) isSource WatchedModule
watchModule w (FileModule _ (Just proj)) = watchProject w proj []
watchModule _ _ = return ()

-- | Watch for top of package-db stack
watchPackageDb :: Watcher -> PackageDbStack -> [String] -> IO ()
watchPackageDb w pdbs opts = case topPackageDb pdbs of
	GlobalDb -> return () -- TODO: Watch for global package-db
	UserDb -> return () -- TODO: Watch for user package-db
	(PackageDb pdb) -> watchTree w (view path pdb) isConf (WatchedPackageDb pdbs opts)

-- | Watch for package-db stack
watchPackageDbStack :: Watcher -> PackageDbStack -> [String] -> IO ()
watchPackageDbStack w pdbs opts = mapM_ (\pdbs' -> watchPackageDb w pdbs' opts) $ packageDbStacks pdbs

unwatchProject :: Watcher -> Project -> IO ()
unwatchProject w proj = do
	mapM_ (unwatchTree w) dirs
	void $ unwatchDir w projDir
	where
		dirs = map ((projDir </>) . view (entity . path)) $ maybe [] sourceDirs $ view projectDescription proj
		projDir = view (projectPath . path) proj

unwatchModule :: Watcher -> ModuleLocation -> IO ()
unwatchModule w (FileModule f Nothing) = void $ unwatchDir w (takeDirectory $ view path f)
unwatchModule _ (FileModule _ (Just _)) = return ()
unwatchModule _ (InstalledModule _ _ _) = return ()
unwatchModule _ _ = return ()

-- | Unwatch package-db
unwatchPackageDb :: Watcher -> PackageDb -> IO ()
unwatchPackageDb _ GlobalDb = return () -- TODO: Unwatch global package-db
unwatchPackageDb _ UserDb = return () -- TODO: Unwatch user package-db
unwatchPackageDb w (PackageDb pdb) = void $ unwatchTree w (view path pdb)

isSource :: Event -> Bool
isSource = haskellSource . view eventPath

isCabal :: Event -> Bool
isCabal = cabalFile . view eventPath

isConf :: Event -> Bool
isConf (Event _ f _) = takeExtension f == ".conf"
