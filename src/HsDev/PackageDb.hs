{-# LANGUAGE OverloadedStrings #-}

module HsDev.PackageDb (
	module HsDev.PackageDb.Types,

	packageDbPath, readPackageDb
	) where

import Control.Lens
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import Data.Text (pack, unpack)
import Data.Traversable
import Distribution.InstalledPackageInfo
import Distribution.Package
import Distribution.Text (disp)
import System.FilePath

import GHC.Paths

import HsDev.PackageDb.Types
import HsDev.Error
import HsDev.Symbols.Location
import HsDev.Tools.Base
import HsDev.Util (directoryContents, readFileUtf8)
import System.Directory.Paths

-- | Get path to package-db
packageDbPath :: PackageDb -> IO Path
packageDbPath GlobalDb = do
	out <- fmap lines $ runTool_ ghc_pkg ["list", "--global"]
	case out of
		(fpath:_) -> return $ fromFilePath $ normalise fpath
		[] -> hsdevError $ ToolError ghc_pkg "empty output, expecting path to global package-db"
packageDbPath UserDb = do
	out <- fmap lines $ runTool_ ghc_pkg ["list", "--user"]
	case out of
		(fpath:_) -> return $ fromFilePath $ normalise fpath
		[] -> hsdevError $ ToolError ghc_pkg "empty output, expecting path to user package db"
packageDbPath (PackageDb fpath) = return fpath

-- | Read package-db conf files
readPackageDb :: PackageDb -> IO (Map ModulePackage [ModuleLocation])
readPackageDb pdb = do
	p <- packageDbPath pdb
	mlibdir <- fmap (listToMaybe . lines) $ runTool_ ghc ["--print-libdir"]
	confs <- fmap (filter isConf) $ directoryContents (p ^. path)
	fmap M.unions $ forM confs $ \conf -> do
		cts <- readFileUtf8 conf
		case parseInstalledPackageInfo (unpack cts) of
			ParseFailed _ -> return M.empty  -- FIXME: Should log as warning
			ParseOk _ res -> return $ over (each . each . moduleInstallDirs . each) (subst mlibdir) $ listMods res
	where
		isConf f = takeExtension f == ".conf"
		listMods pinfo = M.singleton pname pmods where
			pname = ModulePackage
				(pack . show . disp . pkgName $ sourcePackageId pinfo)
				(pack . show . disp . pkgVersion $ sourcePackageId pinfo)
			pmods = [InstalledModule (map fromFilePath $ libraryDirs pinfo) pname nm exposed' | (nm, exposed') <- names]
			names = zip (map (pack . show . disp) (exposedModules pinfo)) (repeat True) ++ zip (map (pack . show . disp) (hiddenModules pinfo)) (repeat False)
		subst Nothing f = f
		subst (Just libdir') f = case splitPaths f of
			("$topdir":rest) -> joinPaths (fromFilePath libdir' : rest)
			_ -> f
