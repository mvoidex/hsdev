module HsDev.Tools.HDocs (
	hdocs,
	setDocs,
	loadDocs,

	hdocsProcess
	) where

import Control.Exception
import Control.Monad (liftM)

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L (pack)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import System.Process (readProcess)

import HDocs.Module (docs, runDocsM, formatDoc)

import HsDev.Symbols

-- | Get docs for module
hdocs :: String -> [String] -> IO (Map String String)
hdocs mname opts = catch hdocs' onError where
	hdocs' = do
		d <- runDocsM $ docs opts mname
		return $ either (const M.empty) (M.map formatDoc) d
	onError :: SomeException -> IO (Map String String)
	onError _ = return M.empty

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
hdocsProcess mname opts = liftM (decode . L.pack) $ readProcess "hdocs" opts' "" where
	opts' = mname : concat [["-g", opt] | opt <- opts]
