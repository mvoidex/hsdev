{-# LANGUAGE RankNTypes #-}

module HsDev.Symbols.Resolve (
	sourceDeps, sourceRDeps,
	sourceDeps', sourceRDeps',
	RefineTable, refineTable, refineSymbol, refineSymbols,
	symbolUniqId
	) where

import Control.Lens
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, maybeToList)
import Data.String (fromString)
import Data.Text (Text)
import Data.Generics.Uniplate.Operations
import Language.Haskell.Exts as Exts (ModuleName(..), importModule)

import Data.Deps
import HsDev.Database
import HsDev.Symbols hiding (exportedSymbols)
import HsDev.Symbols.Util
import HsDev.Symbols.Parsed (Ann, Parsed)
import System.Directory.Paths

-- | Flattened dependencies
sourceDeps :: Database -> Deps Path
sourceDeps db = either (const mempty) id $ flatten $ mconcat $ do
	src <- sources
	fpath <- maybeToList $ preview (moduleId . moduleLocation . moduleFile) src
	msrc <- maybeToList $ view moduleSource src
	imp <- [fromString n | ModuleName _ n <- map Exts.importModule (childrenBi msrc) :: [ModuleName Ann]]
	im <- case preview (moduleId . moduleLocation . moduleProject . _Just) src of
		Nothing -> maybeToList $ M.lookup (normPath (joinPaths [sourceModuleRoot (view (moduleId . moduleName) src) fpath, importPath imp])) tbl
		Just proj -> do
			target <- fileTargets proj fpath
			dir <- view infoSourceDirs target
			maybeToList $ M.lookup (normPath $ joinPaths [view projectPath proj, dir, importPath imp]) tbl
	ipath <- maybeToList $ preview (moduleId . moduleLocation . moduleFile) im
	return $ dep fpath ipath
	where
		sources = db ^.. modules . filtered byFile
		tbl = M.fromList $ do
			src <- sources
			Just fpath <- return $ preview (moduleId . moduleLocation . moduleFile) src
			return (fpath, src)

-- | Flattened reverse dependencies
sourceRDeps :: Database -> Deps Path
sourceRDeps = inverse . sourceDeps

-- | Flattened dependencies
-- TODO: `Project` in `ModuleId` should be complete (filled with build infos)
sourceDeps' :: Map Path (ModuleId, Maybe Parsed) -> Deps Path
sourceDeps' sources = either (const mempty) id $ flatten $ mconcat $ do
	(m, mparsed) <- M.elems sources
	fpath <- maybeToList $ preview (moduleLocation . moduleFile) m
	msrc <- maybeToList mparsed
	imp <- [fromString n | ModuleName _ n <- map Exts.importModule (childrenBi msrc) :: [ModuleName Ann]]
	(im, _) <- case preview (moduleLocation . moduleProject . _Just) m of
		Nothing -> maybeToList $ M.lookup (normPath (joinPaths [sourceModuleRoot (view moduleName m) fpath, importPath imp])) sources
		Just proj -> do
			target <- fileTargets proj fpath
			dir <- view infoSourceDirs target
			maybeToList $ M.lookup (normPath $ joinPaths [view projectPath proj, dir, importPath imp]) sources
	ipath <- maybeToList $ preview (moduleLocation . moduleFile) im
	return $ dep fpath ipath

-- | Flattened reverse dependencies
sourceRDeps' :: Map Path (ModuleId, Maybe Parsed) -> Deps Path
sourceRDeps' = inverse . sourceDeps'

type RefineTable = M.Map (Text, Text, SymbolInfo) Symbol

refineTable :: [Symbol] -> RefineTable
refineTable syms = M.fromList [(symbolUniqId s, s) | s <- syms]

refineSymbol :: RefineTable -> Symbol -> Symbol
refineSymbol tbl s = fromMaybe s $ M.lookup (symbolUniqId s) tbl

refineSymbols :: RefineTable -> Module -> Module
refineSymbols tbl = over moduleSymbols (refineSymbol tbl)

symbolUniqId :: Symbol -> (Text, Text, SymbolInfo)
symbolUniqId s = (view (symbolId . symbolName) s, view (symbolId . symbolModule . moduleName) s, nullifyInfo $ view symbolInfo s)
