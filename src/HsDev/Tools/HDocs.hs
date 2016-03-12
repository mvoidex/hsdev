module HsDev.Tools.HDocs (
	hdocsy, hdocs, hdocsCabal,
	setDocs,
	loadDocs,

	hdocsProcess,

	module Control.Monad.Except
	) where

import Control.Exception
import Control.DeepSeq
import Control.Lens (set, view, over)
import Control.Monad ()
import Control.Monad.Except

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as L (pack)
import Data.Map (Map)
import qualified Data.Map as M
import Data.String (fromString)
import System.Process (readProcess)

import qualified HDocs.Module as HDocs
import qualified HDocs.Haddock as HDocs (readSource, readSources_)

import HsDev.Symbols

-- | Get docs for modules
hdocsy :: [ModuleLocation] -> [String] -> IO [Map String String]
hdocsy mlocs opts = runExceptT (docs' mlocs) >>= return . either (const $ replicate (length mlocs) M.empty) (map $ force . HDocs.formatDocs) where
	docs' :: [ModuleLocation] -> ExceptT String IO [HDocs.ModuleDocMap]
	docs' ms = liftM (map snd) $ HDocs.readSources_ opts $ map (view moduleFile) ms

-- | Get docs for module
hdocs :: ModuleLocation -> [String] -> IO (Map String String)
hdocs mloc opts = runExceptT (docs' mloc) >>= return . either (const M.empty) (force . HDocs.formatDocs) where
	docs' :: ModuleLocation -> ExceptT String IO HDocs.ModuleDocMap
	docs' (FileModule fpath _) = liftM snd $ HDocs.readSource opts fpath
	docs' (InstalledModule _ _ mname) = HDocs.moduleDocs opts mname
	docs' _ = throwError $ "Can't get docs for: " ++ show mloc

-- | Get all docs
hdocsCabal :: PackageDbStack -> [String] -> ExceptT String IO (Map String (Map String String))
hdocsCabal pdbs opts = liftM (M.map $ force . HDocs.formatDocs) $ HDocs.installedDocs (packageDbStackOpts pdbs ++ opts)

-- | Set docs for module
setDocs :: Map String String -> Module -> Module
setDocs d = over moduleDeclarations (map setDoc) where
	setDoc decl' = set declarationDocs (M.lookup (view declarationName decl') d') decl'
	d' = M.mapKeys fromString . M.map fromString $ d

-- | Load docs for module
loadDocs :: [String] -> Module -> IO Module
loadDocs opts m = do
	d <- hdocs (view moduleLocation m) opts
	return $ setDocs d m

hdocsProcess :: String -> [String] -> IO (Maybe (Map String String))
hdocsProcess mname opts = handle onErr $ liftM (decode . L.pack . last . lines) $ readProcess "hdocs" opts' "" where
	opts' = mname : concat [["-g", opt] | opt <- opts]
	onErr :: SomeException -> IO (Maybe a)
	onErr _ = return Nothing
