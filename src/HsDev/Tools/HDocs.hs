module HsDev.Tools.HDocs (
	hdocsy, hdocs, hdocsCabal,
	setDocs,

	hdocsProcess,

	module Control.Monad.Except
	) where

import Control.DeepSeq
import Control.Lens (set, view, over, each)
import Control.Monad ()
import Control.Monad.Except

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as L (pack)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.String (fromString)

import qualified HDocs.Module as HDocs
import qualified HDocs.Haddock as HDocs (readSourceGhc, readSourcesGhc)

import qualified GHC

import HsDev.Error
import HsDev.Tools.Base
import HsDev.Tools.Ghc.Worker (GhcM, haddockSession, liftGhc)
import HsDev.Symbols

-- | Get docs for modules
hdocsy :: [ModuleLocation] -> [String] -> GhcM [Map String String]
hdocsy mlocs opts = (map $ force . HDocs.formatDocs) <$> docs' mlocs where
	docs' :: [ModuleLocation] -> GhcM [HDocs.ModuleDocMap]
	docs' ms = do
		haddockSession opts
		liftGhc $ hsdevLiftWith (ToolError "hdocs") $
			liftM (map snd) $ HDocs.readSourcesGhc opts $ map (view moduleFile) ms

-- | Get docs for module
hdocs :: ModuleLocation -> [String] -> GhcM (Map String String)
hdocs mloc opts = (force . HDocs.formatDocs) <$> docs' mloc where
	docs' :: ModuleLocation -> GhcM HDocs.ModuleDocMap
	docs' mloc' = do
		haddockSession opts
		liftGhc $ case mloc' of
			(FileModule fpath _) -> hsdevLiftWith (ToolError "hdocs") $ liftM snd $ HDocs.readSourceGhc opts fpath
			(InstalledModule _ _ mname) -> do
				df <- GHC.getSessionDynFlags
				liftIO $ hsdevLiftWith (ToolError "hdocs") $ HDocs.moduleDocsF df mname
			_ -> hsdevError $ ToolError "hdocs" $ "Can't get docs for: " ++ show mloc'

-- | Get all docs
hdocsCabal :: PackageDbStack -> [String] -> GhcM (Map String (Map String String))
hdocsCabal pdbs opts = do
	haddockSession (packageDbStackOpts pdbs ++ opts)
	df <- GHC.getSessionDynFlags
	liftIO $ hsdevLiftWith (ToolError "hdocs") $
		liftM (M.map $ force . HDocs.formatDocs) $ HDocs.installedDocsF df

-- | Set docs for module
setDocs :: Map String String -> Module -> Module
setDocs d = over (moduleExports . each) setDoc . over (moduleScope . each . each) setDoc where
	setDoc sym' = set symbolDocs (M.lookup (view (symbolId . symbolName) sym') d') sym'
	d' = M.mapKeys fromString . M.map fromString $ d

hdocsProcess :: String -> [String] -> IO (Maybe (Map String String))
hdocsProcess mname opts = liftM (decode . L.pack . last . lines) $ runTool_ "hdocs" opts' where
	opts' = mname : concat [["-g", opt] | opt <- opts]
