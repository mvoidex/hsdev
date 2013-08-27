module HsDev.Symbols.Class (
	Symbol(..),
	
	module HsDev.Symbols.Location
	) where

import HsDev.Symbols.Location

class Symbol a where
	symbolName :: a -> String
	symbolQualifiedName :: a -> String
	symbolDocs :: a -> Maybe String
	symbolLocation :: a -> Location
