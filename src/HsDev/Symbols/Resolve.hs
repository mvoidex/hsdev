{-# LANGUAGE RankNTypes #-}

module HsDev.Symbols.Resolve (
	resolve,
	resolveFile, resolveStandalone, resolveProject, resolveAll,
	sourceDeps, sourceRDeps,
	RefineTable, refineTable, refineSymbol, refineSymbols,
	symbolUniqId
	) where

import Control.Lens
import qualified Data.Map as M
import Data.List ((\\))
import Data.Maybe (fromMaybe, maybeToList)
import Data.String (fromString)
import Data.Text (Text)
import Data.Generics.Uniplate.Operations
import Language.Haskell.Names (Scoped(..), annotate)
import qualified Language.Haskell.Names as N (resolve)
import Language.Haskell.Names.Imports (importTable)
import Language.Haskell.Names.ModuleSymbols (moduleTable)
import Language.Haskell.Names.Exports (exportedSymbols)
import Language.Haskell.Exts as Exts (ModuleName(..), SrcSpanInfo, importModule)
import System.FilePath

import Data.Deps
import HsDev.Database
import HsDev.Symbols hiding (exportedSymbols)
import HsDev.Symbols.Util
import HsDev.Util (ordNub)

resolve :: [Module] -> [Module] -> [Module]
resolve _ [] = []
resolve envs ms = map (refineSymbols stbl . resolveModule) ms where
	stbl = refineTable $ ms ^.. each . moduleSymbols ++ envs ^.. each . moduleSymbols
	env = N.resolve (ms ^.. each . moduleSource . _Just) (environment envs)
	resolveModule m = case view moduleSource m of
		Nothing -> m
		Just src -> set moduleExports exports . set moduleScope scope . over (moduleSource . _Just) annotateSource $ m where
			imports = importTable env src
			tbl = moduleTable imports src
			scope = M.map (map fromSymbol) tbl
			exports = map fromSymbol $ exportedSymbols tbl src
			annotateSource = annotate env . fmap (\(Scoped _ l) -> l)

resolveFile :: FilePath -> Database -> Database
resolveFile fpath db = modulesUpdate db (resolve ems ms) where
	deps' = sourceDeps db
	dependent = maybe [] (fpath :) $ inverse deps' ^? ix fpath
	dependencies = ordNub (concat [fromMaybe [] (deps' ^? ix dep') | dep' <- dependent]) \\ dependent
	fileSlice = fileDepsSlice fpath . modules
	ems = db ^.. fileSlice . filtered (\m -> or (not (byFile m) : [inFile d' m | d' <- dependencies]))
	ms = db ^.. fileSlice . filtered (\m -> or [inFile dep' m | dep' <- dependent])

resolveStandalone :: [FilePath] -> Database -> Database
resolveStandalone [] _ = mempty
resolveStandalone fpaths db = modulesUpdate db (resolve ems ms) where
	deps' = sourceDeps db
	dependent = ordNub $ concat $ fpaths : [fromMaybe [] (inverse deps' ^? ix fpath) | fpath <- fpaths]
	dependencies = ordNub (concat [fromMaybe [] (deps' ^? ix dep') | dep' <- dependent]) \\ dependent
	ems = db ^.. slices [slice installed, standaloneSlice] . modules . filtered (\m -> or (not (byFile m) : [inFile d' m | d' <- dependencies]))
	ms = db ^.. slice (\m -> or [inFile dep' m | dep' <- dependent]) . modules

resolveProject :: Project -> Database -> Database
resolveProject proj db = modulesUpdate db (resolve (db ^.. projectDepsSlice proj . modules) (db ^.. projectSlice proj . modules))

resolveAll :: Database -> Database
resolveAll db = mconcat (resolveStandalone stand db : map (`resolveProject` db) projs) where
	projs = db ^.. databaseProjects . each
	stand = db ^.. standaloneSlice . modules . moduleId . moduleLocation . moduleFile

-- | Flattened dependencies
sourceDeps :: Database -> Deps FilePath
sourceDeps db = either (const mempty) id $ flatten $ mconcat $ do
	src <- sources
	fpath <- maybeToList $ preview (moduleId . moduleLocation . moduleFile) src
	msrc <- maybeToList $ view moduleSource src
	imp <- [n | ModuleName _ n <- map Exts.importModule (childrenBi msrc) :: [ModuleName (Scoped SrcSpanInfo)]]
	im <- case preview (moduleId . moduleLocation . moduleProject . _Just) src of
		Nothing -> maybeToList $ M.lookup (normalise (sourceModuleRoot (view (moduleId . moduleName) src) fpath </> importPath (fromString imp))) tbl
		Just proj -> do
			target <- fileTargets proj fpath
			dir <- view infoSourceDirs target
			maybeToList $ M.lookup (normalise $ view projectPath proj </> dir </> importPath (fromString imp)) tbl
	ipath <- maybeToList $ preview (moduleId . moduleLocation . moduleFile) im
	return $ dep fpath ipath
	where
		sources = db ^.. modules . filtered byFile
		tbl = M.fromList $ do
			src <- sources
			Just fpath <- return $ preview (moduleId . moduleLocation . moduleFile) src
			return (fpath, src)

-- | Flattened reverse dependencies
sourceRDeps :: Database -> Deps FilePath
sourceRDeps = inverse . sourceDeps

moduleUpdate :: Database -> Module -> Database
moduleUpdate db m = maybe mempty fromModule $ set (_Just . inspectionResult . _Right) m (db ^? databaseModules . ix (view (moduleId . moduleLocation) m))

modulesUpdate :: Database -> [Module] -> Database
modulesUpdate db = mconcat . map (moduleUpdate db)

type RefineTable = M.Map (Text, Text, SymbolInfo) Symbol

refineTable :: [Symbol] -> RefineTable
refineTable syms = M.fromList [(symbolUniqId s, s) | s <- syms]

refineSymbol :: RefineTable -> Symbol -> Symbol
refineSymbol tbl s = fromMaybe s $ M.lookup (symbolUniqId s) tbl

refineSymbols :: RefineTable -> Module -> Module
refineSymbols tbl = over moduleSymbols (refineSymbol tbl)

symbolUniqId :: Symbol -> (Text, Text, SymbolInfo)
symbolUniqId s = (view (symbolId . symbolName) s, view (symbolId . symbolModule . moduleName) s, nullifyInfo $ view symbolInfo s)
