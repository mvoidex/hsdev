{-# LANGUAGE  TypeSynonymInstances, FlexibleInstances #-}

module HsDev.Symbols (
	Location(..),
	Import(..),
	Symbol(..),
	Cabal(..),
	Module(..),
	TypeInfo(..),
	Declaration(..),

	mkSymbol,
	position,
	symbolQualifiedName,
	mkLocation,
	moduleLocation,
	setModuleReferences,
	addDeclaration,
	unalias
	) where

import Control.Arrow
import Data.Function (fix)
import Data.List
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M
import Data.Time.Clock.POSIX

import HsDev.Project

-- | Location of symbol
data Location = Location {
	locationFile :: FilePath,
	locationLine :: Int,
	locationColumn :: Int,
	locationProject :: Maybe Project }
		deriving (Eq, Ord)

instance Read POSIXTime where
	readsPrec i = map (first fromIntegral) . readsPrec i

instance Show Location where
	show loc = intercalate ":" [locationFile loc, show $ locationLine loc, show $ locationColumn loc]

-- | Module import
data Import = Import {
	importModuleName :: String,
	importIsQualified :: Bool,
	importAs :: Maybe String }
		deriving (Eq, Ord)

instance Show Import where
	show i = "import " ++ (if importIsQualified i then "qualified " else "") ++ importModuleName i ++ maybe "" (" as " ++) (importAs i)

-- | Symbol
data Symbol a = Symbol {
	symbolName :: String,
	symbolModule :: Maybe (Symbol Module),
	symbolDocs :: Maybe String,
	symbolLocation :: Maybe Location,
	symbolTags :: [String],
	symbol :: a }

instance Ord a => Ord (Symbol a) where
	compare l r = compare (asTuple l) (asTuple r) where
		asTuple s = (
			symbolName s,
			maybe "" symbolName (symbolModule s),
			symbolLocation s,
			symbol s)

instance Functor Symbol where
	fmap f s = s { symbol = f (symbol s) }

instance Eq a => Eq (Symbol a) where
	l == r = and [
		symbolName l == symbolName r,
		symbolModule l == symbolModule r,
		symbolLocation l == symbolLocation r,
		symbol l == symbol r]

instance Show (Symbol Module) where
	show m = unlines [
		"module " ++ symbolName m,
		"\texports: " ++ intercalate ", " (moduleExports $ symbol m),
		"\timports:",
		unlines $ map (tab 2 . show) $ M.elems (moduleImports $ symbol m),
		"\tdeclarations:",
		unlines $ map (tabs 2 . show) $ M.elems (moduleDeclarations $ symbol m),
		"\tcabal: " ++ show (moduleCabal $ symbol m),
		"\tdocs: " ++ fromMaybe "" (symbolDocs m),
		"\tlocation: " ++ show (symbolLocation m)]

instance Show (Symbol Declaration) where
	show d = unlines [
		title,
		"\tmodule: " ++ maybe "" symbolName (symbolModule d),
		"\tdocs: " ++ fromMaybe "" (symbolDocs d),
		"\tlocation:" ++ show (symbolLocation d)]
		where
			title = case symbol d of
				Function t -> symbolName d ++ maybe "" (" :: " ++) t
				Type ti -> title' "type" ti
				NewType ti -> title' "newtype" ti
				Data ti -> title' "data" ti
				Class ti -> title' "class" ti
			title' n ti = n ++ " " ++ maybe "" (++ " => ") (typeInfoContext ti) ++ symbolName d ++ " " ++ unwords (typeInfoArgs ti) ++ maybe "" (" = " ++) (typeInfoDefinition ti)

-- | Cabal or cabal-dev
data Cabal = Cabal | CabalDev FilePath deriving (Eq, Ord)

instance Show Cabal where
	show Cabal = "<cabal>"
	show (CabalDev path) = path

-- | Module
data Module = Module {
	moduleExports :: [String],
	moduleImports :: Map String Import,
	moduleDeclarations :: Map String (Symbol Declaration),
	moduleCabal :: Maybe Cabal }
		deriving (Ord)

instance Eq Module where
	l == r = moduleCabal l == moduleCabal r

tab :: Int -> String -> String
tab n s = replicate n '\t' ++ s

tabs :: Int -> String -> String
tabs n = unlines . map (tab n) . lines

-- | Common info for type/newtype/data/class
data TypeInfo = TypeInfo {
	typeInfoContext :: Maybe String,
	typeInfoArgs :: [String],
	typeInfoDefinition :: Maybe String }
		deriving (Eq, Ord, Read, Show)

showTypeInfo :: TypeInfo -> String -> String -> String
showTypeInfo ti pre name = pre ++ maybe "" (++ " =>") (typeInfoContext ti) ++ unwords (typeInfoArgs ti) ++ maybe "" (" = " ++) (typeInfoDefinition ti)

-- | Declaration
data Declaration =
	Function { functionType :: Maybe String } |
	Type { typeInfo :: TypeInfo } |
	NewType { newTypeInfo :: TypeInfo } |
	Data { dataInfo :: TypeInfo } |
	Class { classInfo :: TypeInfo }
		deriving (Ord)

instance Eq Declaration where
	(Function l) == (Function r) = l == r
	(Type _) == (Type _) = True
	(NewType _) == (NewType _) = True
	(Data _) == (Data _) = True
	(Class _) == (Class _) = True
	_ == _ = False

-- | Make symbol by name and data
mkSymbol :: String -> a -> Symbol a
mkSymbol name d = Symbol name Nothing Nothing Nothing [] d

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
moduleLocation fname = Location fname 1 1 Nothing

-- | Set module references
setModuleReferences :: Symbol Module -> Symbol Module
setModuleReferences m = fix setRefs where
	setRefs m' = setModule $ fmap setRefs' m where
		setRefs' m'' = m'' {
			moduleDeclarations = M.map setModule (moduleDeclarations $ symbol m) }
		setModule s = s { symbolModule = Just m' }

-- | Add declaration to module
addDeclaration :: Symbol Declaration -> Symbol Module -> Symbol Module
addDeclaration decl m = setModuleReferences $ fmap setDecls m where
	setDecls m' = m' { moduleDeclarations = decls }
	decls = M.insert (symbolName decl) decl $ moduleDeclarations $ symbol m

-- Unalias import name
unalias :: Symbol Module -> String -> [String]
unalias m alias = [importModuleName i | i <- M.elems (moduleImports (symbol m)), importAs i == Just alias]
