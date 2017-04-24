{-# LANGUAGE FlexibleInstances #-}

module HsDev.Symbols.HaskellNames (
	ToEnvironment(..),
	fromSymbol, toSymbol
	) where

import Control.Lens (view)
import Data.String
import qualified Data.Map.Strict as M
import qualified Data.Text as T (unpack)
import qualified Language.Haskell.Exts as H
import qualified Language.Haskell.Names as N

import HsDev.Symbols.Types

class ToEnvironment a where
	environment :: a -> N.Environment

instance ToEnvironment Module where
	environment m = M.singleton (H.ModuleName () (T.unpack $ view sourcedName m)) (map toSymbol $ view moduleExports m)

instance ToEnvironment [Module] where
	environment = M.unions . map environment

fromSymbol :: N.Symbol -> Symbol
fromSymbol s = Symbol sid Nothing Nothing info where
	sid = SymbolId (fromName_ $ N.symbolName s) mid
	mid = case N.symbolModule s of
		H.ModuleName _ m -> ModuleId (fromString m) NoLocation
	info = case s of
		N.Value _ _ -> Function mempty
		N.Method _ _ p -> Method mempty (fromName_ p)
		N.Selector _ _ p cs -> Selector mempty (fromName_ p) (map fromName_ cs)
		N.Constructor _ _ p -> Constructor mempty (fromName_ p)
		N.Type _ _ -> Type mempty mempty
		N.NewType _ _ -> NewType mempty mempty
		N.Data _ _ -> Data mempty mempty
		N.Class _ _ -> Class mempty mempty
		N.TypeFam _ _ a -> TypeFam mempty mempty (fmap fromName_ a)
		N.DataFam _ _ a -> DataFam mempty mempty (fmap fromName_ a)
		N.PatternConstructor _ _ p -> PatConstructor mempty (fmap fromName_ p)
		N.PatternSelector _ _ p c -> PatSelector mempty (fmap fromName_ p) (fromName_ c)

toSymbol :: Symbol -> N.Symbol
toSymbol s = case view symbolInfo s of
	Function _ -> N.Value m n
	Method _ p -> N.Method m n (toName_ p)
	Selector _ p cs -> N.Selector m n (toName_ p) (map toName_ cs)
	Constructor _ p -> N.Constructor m n (toName_ p)
	Type _ _ -> N.Type m n
	NewType _ _ -> N.NewType m n
	Data _ _ -> N.Data m n
	Class _ _ -> N.Class m n
	TypeFam _ _ a -> N.TypeFam m n (fmap toName_ a)
	DataFam _ _ a -> N.DataFam m n (fmap toName_ a)
	PatConstructor _ p -> N.PatternConstructor m n (fmap toName_ p)
	PatSelector _ p c -> N.PatternSelector m n (fmap toName_ p) (toName_ c)
	where
		m = toModuleName_ $ view sourcedModuleName s
		n = toName_ $ view sourcedName s
