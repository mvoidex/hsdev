module HsDev.Tools.HDocs (
	hdocs,
	setDocs,
	loadDocs
	) where

import Control.Exception

import Data.Map (Map)
import qualified Data.Map as M

import HDocs.Module (docs, runDocsM, formatDoc)

import HsDev.Symbols

-- | Get docs for module
hdocs :: String -> [String] -> IO (Map String String)
hdocs moduleName opts = catch hdocs' onError where
	hdocs' = do
		d <- runDocsM $ docs opts moduleName
		return $ either (const M.empty) (M.map formatDoc) d
	onError :: SomeException -> IO (Map String String)
	onError _ = return M.empty

setDocs :: Map String String -> Symbol Module -> Symbol Module
setDocs d s = setModuleReferences $ fmap setDocs' s where
	setDocs' m = m {
		moduleDeclarations = M.mapWithKey setDoc $ moduleDeclarations m }
	setDoc name decl = decl { symbolDocs = M.lookup name d }

-- | Load docs for module
loadDocs :: [String] -> Symbol Module -> IO (Symbol Module)
loadDocs opts m = do
	d <- hdocs (symbolName m) opts
	return $ setDocs d m
