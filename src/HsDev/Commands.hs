{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module HsDev.Commands (
	-- * Commands
	findSymbol, findModule,
	fileModule,
	lookupSymbol,
	whois,
	scopeModules, scope,
	completions, wideCompletions,
	moduleCompletions,
	symbolExported,

	-- * Reexports
	module HsDev.Database,
	module HsDev.Symbols.Types,
	module Control.Monad.Except
	) where

import Control.Lens
import Control.Monad.Except
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T (isPrefixOf, split, unpack)

import HsDev.Database
import HsDev.Project
import HsDev.Symbols
import HsDev.Symbols.Types
import HsDev.Symbols.Util
import HsDev.Util (liftE, ordNub, uniqueBy)
import System.Directory.Paths

-- | Find declaration by name
findSymbol :: Database -> Text -> ExceptT String IO [Symbol]
findSymbol db ident = case nameModule qname of
	Just mname -> do
		ms <- findModule db mname
		return $ ordNub $ ms ^.. each . exportedSymbols . filtered checkName
	Nothing -> return $ ordNub $ db ^.. symbols . filtered checkName
	where
		checkName :: Symbol -> Bool
		checkName s = nameIdent qname == view (symbolId . symbolName) s
		qname = toName ident

-- | Find module by name
findModule :: Database -> Text -> ExceptT String IO [Module]
findModule db mname = return (db ^.. modules . filtered ((== mname) . view sourcedName))

-- | Find module in file
fileModule :: Database -> Path -> ExceptT String IO Module
fileModule db src = do
	src' <- liftE $ canonicalize src
	maybe (throwError $ "File '" ++ view path src' ++ "' not found") return $ db ^? databaseModules . ix (FileModule src' Nothing) . inspected

-- | Lookup visible within project/cabal symbol
lookupSymbol :: Database -> Path -> Text -> ExceptT String IO [Symbol]
lookupSymbol db file ident = do
	m <- fileModule db file
	let
		mproj = m ^? moduleId . moduleLocation . moduleProject . _Just
		dbslice = case mproj of
			Nothing -> db ^. slices [packageDbStackSlice userDb, standaloneSlice]
			Just proj -> db ^. slices [projectDepsSlice proj, projectSlice proj]
	liftM newestPackage $ findSymbol dbslice ident

-- | Whois symbol in scope
whois :: Database -> Path -> Text -> ExceptT String IO [Symbol]
whois db file ident = do
	mthis <- fileModule db file
	return $ fromMaybe [] (mthis ^? moduleScope . ix qname)
	where
		qname = toName ident

-- | Accessible modules
scopeModules :: Database -> Path -> ExceptT String IO [Module]
scopeModules db file = do
	m <- fileModule db file
	let
		mproj = m ^? moduleId . moduleLocation . moduleProject . _Just
		dbslice = case mproj of
			Nothing -> db ^. slices [packageDbStackSlice userDb, standaloneSlice]
			Just proj -> db ^. slices [projectDepsSlice proj, projectSlice proj]
	return $ newestPackage (dbslice ^.. modules)

-- | Symbols in scope
scope :: Database -> Path -> ExceptT String IO [Symbol]
scope db file = do
	mthis <- fileModule db file
	return $ ordNub $ mthis ^.. moduleScope . each . each

-- | Completions
completions :: Database -> Path -> Text -> ExceptT String IO [Symbol]
completions db file prefix = do
	mthis <- fileModule db file
	return $ mthis ^.. scopeSymbols . filtered prefixed . _1 . briefSymbol
	where
		qname = toName prefix
		prefixed = any (namePrefix qname) . view _2

-- | Wide completions
wideCompletions :: Database -> Path -> Text -> ExceptT String IO [Symbol]
wideCompletions db file prefix = do
	ms <- scopeModules db file
	return $ uniqueBy (view symbolId)
		[s | s <- ms ^.. each . exportedSymbols . filtered prefixed . briefSymbol]
	where
		qname = toName prefix
		prefixed = namePrefix qname . toName . view sourcedName

-- | Module completions
moduleCompletions :: Database -> [Module] -> Text -> ExceptT String IO [String]
moduleCompletions _ ms prefix = return $ map T.unpack $ ordNub $ completions' $ map (view (moduleId . moduleName)) ms where
	completions' = mapMaybe getNext where
		getNext m
			| prefix `T.isPrefixOf` m = listToMaybe $ map snd $ dropWhile (uncurry (==)) $ zip (T.split (== '.') prefix) (T.split (== '.') m)
			| otherwise = Nothing

-- | Modules that exports this symbol
-- FIXME: there's no index, we go through all symbols
symbolExported :: Database -> Symbol -> ExceptT String IO [Module]
symbolExported db sym = return $ db ^.. modules . filtered (\m -> not $ null $ m ^.. exportedSymbols . filtered (== sym))
