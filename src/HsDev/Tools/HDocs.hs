{-# LANGUAGE CPP, OverloadedStrings #-}

module HsDev.Tools.HDocs (
	hdocsy, hdocs, hdocsPackage, hdocsCabal,
	setSymbolDocs, setDocs, setModuleDocs,

	hdocsProcess,

	readDocs, readModuleDocs, readProjectTargetDocs,

	hdocsSupported,

	module Control.Monad.Except
	) where

import Control.Lens
import Control.Monad ()
import Control.Monad.Except

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
#ifdef NODOCS
import qualified System.Log.Simple as Log
#endif

#ifndef NODOCS
import Control.DeepSeq
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as L (pack)
import Data.String (fromString)
import qualified Data.Text as T

import qualified HDocs.Module as HDocs
import qualified HDocs.Haddock as HDocs

import qualified GHC
#endif

import qualified PackageConfig as P

import Data.LookupTable
#ifndef NODOCS
import HsDev.Error
import HsDev.Scan.Browse (packageConfigs, readPackage)
import HsDev.Tools.Base
#endif
import HsDev.Symbols
import HsDev.Tools.Ghc.Worker
import System.Directory.Paths

-- | Get docs for modules
hdocsy :: [ModuleLocation] -> [String] -> GhcM [Map String String]
#ifndef NODOCS
hdocsy mlocs opts = (map $ force . HDocs.formatDocs) <$> docs' mlocs where
	docs' :: [ModuleLocation] -> GhcM [HDocs.ModuleDocMap]
	docs' ms = do
		haddockSession opts
		liftGhc $ hsdevLiftWith (ToolError "hdocs") $
			liftM (map snd) $ HDocs.readSourcesGhc opts $ map (view (moduleFile . path)) ms
#else
hdocsy _ _ = notSupported >> return mempty
#endif

-- | Get docs for module
hdocs :: ModuleLocation -> [String] -> GhcM (Map String String)
#ifndef NODOCS
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
#else
hdocs _ _ = notSupported >> return mempty
#endif

-- | Get docs for package
hdocsPackage :: P.PackageConfig -> GhcM (Map Text (Map Text Text))
#ifndef NODOCS
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
#else
hdocsPackage _ = notSupported >> return mempty
#endif

-- | Get all docs
hdocsCabal :: PackageDbStack -> [String] -> GhcM [(ModulePackage, (Map Text (Map Text Text)))]
#ifndef NODOCS
hdocsCabal pdbs opts = do
	haddockSession (packageDbStackOpts pdbs ++ opts)
	pkgs <- packageConfigs
	forM pkgs $ \pkg -> do
		pkgDocs' <- hdocsPackage pkg
		return (readPackage pkg, pkgDocs')
#else
hdocsCabal _ _ = notSupported >> return mempty
#endif

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
#ifndef NODOCS
hdocsProcess mname opts = liftM (decode . L.pack . last . lines) $ runTool_ "hdocs" opts' where
	opts' = mname : concat [["-g", opt] | opt <- opts]
#else
hdocsProcess _ _ = return mempty
#endif

-- | Read docs for one module
readDocs :: Text -> [String] -> Path -> GhcM (Maybe (Map String String))
#ifndef NODOCS
readDocs mname opts fpath = do
	docs <- liftGhc $ hsdevLift $ HDocs.readSourcesGhc opts [view path fpath]
	return $ fmap HDocs.formatDocs $ lookup (T.unpack mname) docs
#else
readDocs _ _ _ = notSupported >> return mempty
#endif

-- | Read docs for one module
readModuleDocs :: [String] -> Module -> GhcM (Maybe (Map String String))
#ifndef NODOCS
readModuleDocs opts m = case view (moduleId . moduleLocation) m of
	FileModule fpath _ -> withCurrentDirectory (sourceRoot_ (m ^. moduleId) ^. path) $ do
		readDocs (m ^. moduleId . moduleName) opts fpath
	_ -> hsdevError $ ModuleNotSource (view (moduleId . moduleLocation) m)
#else
readModuleDocs _ _ = notSupported >> return mempty
#endif

readProjectTargetDocs :: [String] -> Project -> [Path] -> GhcM (Map String (Map String String))
#ifndef NODOCS
readProjectTargetDocs opts proj fpaths = withCurrentDirectory (proj ^. projectPath . path) $ do
	docs <- liftGhc $ hsdevLift $ HDocs.readSourcesGhc opts (fpaths ^.. each . path)
	return $ M.map HDocs.formatDocs $ M.fromList docs
#else
readProjectTargetDocs _ _ _ = notSupported >> return mempty
#endif

#ifdef NODOCS
notSupported :: Log.MonadLog m => m ()
notSupported = Log.sendLog Log.Warning "compiled without hdocs support"
#endif

hdocsSupported :: Bool
#ifndef NODOCS
hdocsSupported = True
#else
hdocsSupported = False
#endif
