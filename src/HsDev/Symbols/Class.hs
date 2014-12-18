module HsDev.Symbols.Class (
	Symbol(..),
	symbolModuleLocation,

	module HsDev.Symbols.Location
	) where

import Data.Text (Text)

import HsDev.Symbols.Location

class Symbol a where
	symbolName :: a -> Text
	symbolQualifiedName :: a -> Text
	symbolDocs :: a -> Maybe Text
	symbolLocation :: a -> Location

symbolModuleLocation :: Symbol a => a -> ModuleLocation
symbolModuleLocation = locationModule . symbolLocation
