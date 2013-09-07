module HsDev.Symbols.Class (
	Symbol(..),
	symbolModuleLocation,

	module HsDev.Symbols.Location
	) where

import HsDev.Symbols.Location

class Symbol a where
	symbolName :: a -> String
	symbolQualifiedName :: a -> String
	symbolDocs :: a -> Maybe String
	symbolLocation :: a -> Location

symbolModuleLocation :: Symbol a => a -> ModuleLocation
symbolModuleLocation = locationModule . symbolLocation
