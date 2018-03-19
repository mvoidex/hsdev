module HsDev.Symbols.Class (
	Sourced(..),
	sourcedModuleName,

	module HsDev.Symbols.Location
	) where

import Control.Lens (Lens', Traversal')
import Data.Text (Text)

import HsDev.Symbols.Location

class Sourced a where
	sourcedName :: Lens' a Text
	sourcedDocs :: Traversal' a Text
	sourcedModule :: Lens' a ModuleId
	sourcedLocation :: Traversal' a Position
	sourcedDocs _ = pure
	sourcedLocation _ = pure

instance Sourced ModuleId where
	sourcedName = moduleName
	sourcedModule = id

instance Sourced SymbolId where
	sourcedName = symbolName
	sourcedModule = symbolModule

sourcedModuleName :: Sourced a => Lens' a Text
sourcedModuleName = sourcedModule . sourcedName
