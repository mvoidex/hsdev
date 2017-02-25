{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module HsDev.PackageDb (
	module HsDev.PackageDb.Types,

	packageDbPath, readPackageDb
	) where

import Control.Lens
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import Data.Traversable
import Distribution.InstalledPackageInfo
import Distribution.Package
import Distribution.Text (disp)
import System.FilePath

import HsDev.PackageDb.Types
import HsDev.Error
import HsDev.Symbols.Location
import HsDev.Tools.Base
import HsDev.Util (directoryContents, readFileUtf8)

-- | Get path to package-db
packageDbPath :: PackageDb -> IO FilePath
packageDbPath GlobalDb = do
	out <- fmap lines $ runTool_ "ghc-pkg" ["list", "--global"]
	case out of
		(fpath:_) -> return $ normalise fpath
		[] -> hsdevError $ ToolError "ghc-pkg" "empty output, expecting path to global package-db"
packageDbPath UserDb = do
	out <- fmap lines $ runTool_ "ghc-pkg" ["list", "--user"]
	case out of
		(fpath:_) -> return $ normalise fpath
		[] -> hsdevError $ ToolError "ghc-pkg" "empty output, expecting path to user package db"
packageDbPath (PackageDb fpath) = return fpath

-- | Read package-db conf files
readPackageDb :: PackageDb -> IO (Map ModulePackage [ModuleLocation])
readPackageDb pdb = do
	path <- packageDbPath pdb
	mlibdir <- fmap (listToMaybe . lines) $ runTool_ "ghc" ["--print-libdir"]
	confs <- fmap (filter isConf) $ directoryContents path
	fmap M.unions $ forM confs $ \conf -> do
		cts <- readFileUtf8 conf
		case parseInstalledPackageInfo cts of
			ParseFailed _ -> return M.empty  -- FIXME: Should log as warning
			ParseOk _ res -> return $ over (each . each . moduleInstallDirs . each) (subst mlibdir) $ listMods res
	where
		isConf f = takeExtension f == ".conf"
		listMods pinfo = M.singleton pname pmods where
			pname = ModulePackage
				(show . disp . pkgName $ sourcePackageId pinfo)
				(show . disp . pkgVersion $ sourcePackageId pinfo)
			pmods = map (InstalledModule (libraryDirs pinfo) (Just pname)) names
			names = map (show . disp) (exposedModules pinfo) ++ map (show . disp) (hiddenModules pinfo)
		subst Nothing f = f
		subst (Just libdir) f = case splitDirectories f of
			("$topdir":rest) -> joinPath (libdir : rest)
			_ -> f
