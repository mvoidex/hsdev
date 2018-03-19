{-# LANGUAGE RankNTypes #-}

module HsDev.Symbols.Resolve (
	RefineTable, refineTable, refineSymbol, refineSymbols,
	symbolUniqId
	) where

import Control.Lens
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import HsDev.Symbols

type RefineTable = M.Map (Text, Text, SymbolInfo) Symbol

refineTable :: [Symbol] -> RefineTable
refineTable syms = M.fromList [(symbolUniqId s, s) | s <- syms]

refineSymbol :: RefineTable -> Symbol -> Symbol
refineSymbol tbl s = fromMaybe s $ M.lookup (symbolUniqId s) tbl

refineSymbols :: RefineTable -> Module -> Module
refineSymbols tbl = over moduleSymbols (refineSymbol tbl)

symbolUniqId :: Symbol -> (Text, Text, SymbolInfo)
symbolUniqId s = (view (symbolId . symbolName) s, view (symbolId . symbolModule . moduleName) s, nullifyInfo $ view symbolInfo s)
