module HsDev.Tools.HDocs (
	hdocsy, hdocs, hdocsPackage, hdocsCabal,
	setSymbolDocs, setDocs, setModuleDocs,

	hdocsProcess,

	module Control.Monad.Except
	) where

import Control.DeepSeq
import Control.Lens (set, view, mapMOf, each)
import Control.Monad ()
import Control.Monad.Except

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as L (pack)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T

import qualified HDocs.Module as HDocs
import qualified HDocs.Haddock as HDocs

import qualified GHC
import qualified PackageConfig as P

import Data.LookupTable
import HsDev.Error
import HsDev.Scan.Browse (packageConfigs, readPackage)
import HsDev.Tools.Base
import HsDev.Tools.Ghc.Worker (GhcM, haddockSession, liftGhc)
import HsDev.Symbols
import System.Directory.Paths

-- | Get docs for modules
hdocsy :: [ModuleLocation] -> [String] -> GhcM [Map String String]
hdocsy mlocs opts = (map $ force . HDocs.formatDocs) <$> docs' mlocs where
	docs' :: [ModuleLocation] -> GhcM [HDocs.ModuleDocMap]
	docs' ms = do
		haddockSession opts
		liftGhc $ hsdevLiftWith (ToolError "hdocs") $
			liftM (map snd) $ HDocs.readSourcesGhc opts $ map (view (moduleFile . path)) ms

-- | Get docs for module
hdocs :: ModuleLocation -> [String] -> GhcM (Map String String)
hdocs mloc opts = (force . HDocs.formatDocs) <$> docs' mloc where
	docs' :: ModuleLocation -> GhcM HDocs.ModuleDocMap
	docs' mloc' = do
		haddockSession opts
		liftGhc $ case mloc' of
			(FileModule fpath _) -> hsdevLiftWith (ToolError "hdocs") $ liftM snd $ HDocs.readSourceGhc opts (view path fpath)
			(InstalledModule _ _ mname) -> do
				df <- GHC.getSessionDynFlags
				liftIO $ hsdevLiftWith (ToolError "hdocs") $ HDocs.moduleDocsF df (T.unpack mname)
			_ -> hsdevError $ ToolError "hdocs" $ "Can't get docs for: " ++ show mloc'

-- | Get docs for package
hdocsPackage :: P.PackageConfig -> GhcM (Map Text (Map Text Text))
hdocsPackage p = do
	ifaces <-
		liftIO . hsdevLiftWith (ToolError "hdocs") .
		liftM concat . mapM ((`mplus` return []) . HDocs.readInstalledInterfaces) $
		P.haddockInterfaces p
	let
		idocs = HDocs.installedInterfacesDocs ifaces
		iexports = M.fromList $ map (HDocs.exportsDocs idocs) ifaces
		docs = M.map HDocs.formatDocs iexports
		tdocs = M.map (M.map fromString . M.mapKeys fromString) . M.mapKeys fromString $ docs
	return $!! tdocs

-- | Get all docs
hdocsCabal :: PackageDbStack -> [String] -> GhcM [(ModulePackage, (Map Text (Map Text Text)))]
hdocsCabal pdbs opts = do
	haddockSession (packageDbStackOpts pdbs ++ opts)
	pkgs <- packageConfigs
	forM pkgs $ \pkg -> do
		pkgDocs' <- hdocsPackage pkg
		return (readPackage pkg, pkgDocs')

-- | Set docs for module
setSymbolDocs :: MonadIO m => LookupTable (Text, Text) (Maybe Text) -> Map Text Text -> Symbol -> m Symbol
setSymbolDocs tbl d sym = do
	symDocs <- cacheInTableM tbl (symName, symMod) (return $ M.lookup symName d)
	return $ set symbolDocs symDocs sym
	where
		symName = view (symbolId . symbolName) sym
		symMod = view (symbolId . symbolModule . moduleName) sym

-- | Set docs for module symbols
setDocs :: MonadIO m => LookupTable (Text, Text) (Maybe Text) -> Map Text Text -> Module -> m Module
setDocs tbl d = mapMOf (moduleExports . each) setDoc >=> mapMOf (moduleScope . each . each) setDoc where
	setDoc = setSymbolDocs tbl d

-- | Set docs for modules
setModuleDocs :: MonadIO m => LookupTable (Text, Text) (Maybe Text) -> Map Text (Map Text Text) -> Module -> m Module
setModuleDocs tbl docs m = maybe return (setDocs tbl) (M.lookup (view (moduleId . moduleName) m) docs) $ m

hdocsProcess :: String -> [String] -> IO (Maybe (Map String String))
hdocsProcess mname opts = liftM (decode . L.pack . last . lines) $ runTool_ "hdocs" opts' where
	opts' = mname : concat [["-g", opt] | opt <- opts]
