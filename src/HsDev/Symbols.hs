{-# LANGUAGE  TypeSynonymInstances #-}

module HsDev.Symbols (
	Location(..),
	Import(..),
	Symbol(..),
	Cabal(..),
	Module(..),
	TypeInfo(..),
	Declaration(..),

	position,
	symbolQualifiedName,
	
	) where

import Control.Arrow
import Data.Function (fix)
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Time.Clock.POSIX

-- | Location of symbol
data Location = Location {
	locationFile :: FilePath,
	locationLine :: Int,
	locationColumn :: Int,
	locationProject :: Maybe String,
	locationModifiedTime :: Maybe POSIXTime }
		deriving (Eq, Ord, Read, Show)

instance Read POSIXTime where
	readsPrec i = map (first fromIntegral) . readsPrec i

-- | Module import
data Import = Import {
	importModuleName :: String,
	importIsQualified :: Bool,
	importAs :: Maybe String }
		deriving (Eq, Ord, Read, Show)

-- | Symbol
data Symbol a = Symbol {
	symbolName :: String,
	symbolModule :: Maybe (Symbol Module),
	symbolDocs :: Maybe String,
	symbolLocation :: Maybe Location,
	symbolTags :: [String],
	symbol :: a }
		deriving (Ord, Read, Show)

instance Functor Symbol where
	fmap f s = s { symbol = f (symbol s) }

instance Eq a => Eq (Symbol a) where
	l == r = and [
		symbolName l == symbolName r,
		symbolModule l == symbolModule r,
		symbolLocation l == symbolLocation r,
		symbol l == symbol r]

-- | Cabal or cabal-dev
data Cabal = Cabal | CabalDev FilePath deriving (Eq, Ord, Read, Show)

-- | Module
data Module = Module {
	moduleExports :: [String],
	moduleImports :: Map String Import,
	moduleDeclarations :: Map String (Symbol Declaration),
	moduleCabal :: Maybe Cabal }
		deriving (Ord, Read, Show)

instance Eq Module where
	l == r = moduleCabal l == moduleCabal r

-- | Common info for type/newtype/data/class
data TypeInfo = TypeInfo {
	typeInfoContext :: String,
	typeInfoArgs :: [String],
	typeInfoDefinition :: String }
		deriving (Eq, Ord, Read, Show)

-- | Declaration
data Declaration =
	Function { functionType :: String } |
	Type { typeInfo :: TypeInfo } |
	NewType { newTypeInfo :: TypeInfo } |
	Data { dataInfo :: TypeInfo } |
	Class { classInfo :: TypeInfo }
		deriving (Ord, Read, Show)

instance Eq Declaration where
	(Function l) == (Function r) = l == r
	(Type _) == (Type _) = True
	(NewType _) == (NewType _) = True
	(Data _) == (Data _) = True
	(Class _) == (Class _) = True
	_ == _ = False

-- | Returns `filename:line:column`
position :: Location -> String
position loc =  intercalate ":" [locationFile loc, show $ locationLine loc, show $ locationColumn loc]

-- | Returns qualified name of symbol
symbolQualifiedName :: Symbol a -> String
symbolQualifiedName s = pre ++ symbolName s where
	pre = maybe "" ((++ ".") . symbolName) $ symbolModule s

mkLocation :: Location -> IO Location
mkLocation loc = undefined

moduleLocation :: FilePath -> Location
moduleLocation fname = Location fname 1 1 Nothing Nothing

-- | Set module references
setModuleReferences :: Symbol Module -> Symbol Module
setModuleReferences m = fix setRefs where
	setRefs m' = fmap setRefs' m where
		setRefs' m'' = m'' {
			moduleDeclarations = M.map setModule (moduleDeclarations $ symbol m) }
			where
				setModule s = s { symbolModule = Just m' }

-- | Add declaration to module
addDeclaration :: Symbol Declaration -> Symbol Module -> Symbol Module
addDeclaration decl m = setModuleReferences $ fmap setDecls m where
	setDecls m' = m' { moduleDeclarations = decls }
	decls = M.insert (symbolName decl) decl $ moduleDeclarations $ symbol m

-- Unalias import name
unalias :: Symbol Module -> String -> [String]
unalias m alias = [importModuleName i | i <- M.elems (moduleImports (symbol m)), importAs i == Just alias]
