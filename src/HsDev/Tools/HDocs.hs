module HsDev.Tools.HDocs (
	hdocs, hdocsCabal,
	setDocs,
	loadDocs,

	hdocsProcess
	) where

import Control.Exception
import Control.Monad ()
import Control.Monad.Error

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as L (pack)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe ()
import System.Process (readProcess)

import qualified HDocs.Module as HDocs
import qualified HDocs.Haddock as HDocs ()

import HsDev.Symbols

-- | Get docs for module
hdocs :: String -> [String] -> IO (Map String String)
hdocs mname opts = do
	ds <- runErrorT $ HDocs.moduleDocs opts mname
	return $ either (const M.empty) HDocs.formatDocs ds

-- | Get all docs
hdocsCabal :: Cabal -> [String] -> ErrorT String IO (Map String (Map String String))
hdocsCabal cabal opts = liftM (M.map HDocs.formatDocs) $ HDocs.installedDocs (cabalOpt cabal ++ opts)

-- | Set docs for module
setDocs :: Map String String -> Module -> Module
setDocs d m = m { moduleDeclarations = M.mapWithKey setDoc $ moduleDeclarations m } where
	setDoc name decl = decl { declarationDocs = M.lookup name d }

-- | Load docs for module
loadDocs :: [String] -> Module -> IO Module
loadDocs opts m = do
	d <- hdocs (moduleName m) opts
	return $ setDocs d m

hdocsProcess :: String -> [String] -> IO (Maybe (Map String String))
hdocsProcess mname opts = handle onErr $ liftM (decode . L.pack) $ readProcess "hdocs" opts' "" where
	opts' = mname : concat [["-g", opt] | opt <- opts]
	onErr :: SomeException -> IO (Maybe a)
	onErr _ = return Nothing
