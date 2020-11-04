{-# LANGUAGE OverloadedStrings, CPP #-}

module HsDev.PackageDb (
		module HsDev.PackageDb.Types,

		packageDbPath, readPackageDb
		) where

import Control.Lens
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import Data.Text (pack, unpack)
import qualified Data.Text.Encoding as E (encodeUtf8)
import Data.Traversable
import Distribution.InstalledPackageInfo
import Distribution.Package
import Distribution.Text (display)
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
		 -- Bailing on the user package db if there isn't one doesn't seem quite correct. 'stack' and 'cabal'
		 -- can report no path...
		 -- 	[] -> hsdevError $ ToolError ghc_pkg "empty output, expecting path to user package db"
		 -- Report an empty path instead.
				[] -> return $ fromFilePath ""
packageDbPath (PackageDb fpath) = return fpath

-- | Read package-db conf files
readPackageDb :: PackageDb -> IO (Map ModulePackage [ModuleLocation])
readPackageDb pdb = do
		p <- packageDbPath pdb
		mlibdir <- fmap (listToMaybe . lines) $ runTool_ ghc ["--print-libdir"]
		confs <- fmap (filter isConf) $ directoryContents (p ^. path)
		fmap M.unions $ forM confs $ \conf -> do
				cts <- readFileUtf8 conf
				case parseResult (parseInstalledPackageInfo (unpackCts cts)) of
						Left _ -> return M.empty  -- FIXME: Should log as warning
						Right (_, res) -> mapM (mapM canonicalize) $
							over (each . each . moduleInstallDirs . each) (subst mlibdir) $ listMods res
		where
#if MIN_VERSION_Cabal(3,2,0)
				unpackCts = E.encodeUtf8
#else
				unpackCts = unpack
#endif

#if MIN_VERSION_Cabal(3,0,0)
				parseResult = id
#else
				parseResult (ParseFailed e) = Left [show e]
				parseResult (ParseOk ws res) = Right (map show ws, res)
#endif

				isConf f = takeExtension f == ".conf"
				listMods pinfo = M.singleton pname pmods where
						pname = ModulePackage
								(pack . display . pkgName $ sourcePackageId pinfo)
								(pack . display . pkgVersion $ sourcePackageId pinfo)
						pmods = [InstalledModule (map fromFilePath $ libraryDirs pinfo) pname nm exposed' | (nm, exposed') <- names]
						names = zip (map (pack . display) (exposedModules pinfo)) (repeat True) ++ zip (map (pack . display) (hiddenModules pinfo)) (repeat False)
				subst Nothing f = f
				subst (Just libdir') f = case splitPaths f of
						("$topdir":rest) -> joinPaths (fromFilePath libdir' : rest)
						_ -> f
