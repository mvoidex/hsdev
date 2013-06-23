{-# LANGUAGE  TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
	location,
	moduleLocation,
	setModuleReferences,
	addDeclaration,
	unalias,
	brief,
	detailed,
	moduleContents,
	locateProject
	) where

import Control.Arrow
import Control.DeepSeq
import Data.List
import Data.Function (fix)
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M
import Data.Time.Clock.POSIX
import System.Directory
import System.FilePath

import HsDev.Project
import HsDev.Util

-- | Location of symbol
data Location = Location {
	locationFile :: FilePath,
	locationLine :: Int,
	locationColumn :: Int,
	locationProject :: Maybe Project }
		deriving (Eq, Ord)

instance NFData Location where
	rnf (Location f l c p) = rnf f `seq` rnf l `seq` rnf c `seq` rnf p

instance Read POSIXTime where
	readsPrec i = map (first (fromIntegral :: Integer -> POSIXTime)) . readsPrec i

instance Show Location where
	show loc = intercalate ":" [locationFile loc, show $ locationLine loc, show $ locationColumn loc]

-- | Module import
data Import = Import {
	importModuleName :: String,
	importIsQualified :: Bool,
	importAs :: Maybe String,
	importLocation :: Maybe Location }
		deriving (Eq, Ord)

instance NFData Import where
	rnf (Import m q a l) = rnf m `seq` rnf q `seq` rnf a `seq` rnf l

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

instance NFData a => NFData (Symbol a) where
	rnf (Symbol n _ ds l t v) = rnf n `seq` rnf ds `seq` rnf l `seq` rnf t `seq` rnf v

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
		fmap symbolName (symbolModule l) == fmap symbolName (symbolModule r),
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

instance NFData Cabal where
	rnf Cabal = ()
	rnf (CabalDev p) = rnf p

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

instance NFData Module where
	rnf (Module e i d c) = rnf e `seq` rnf i `seq` rnf d `seq` rnf c

instance Eq Module where
	l == r = moduleCabal l == moduleCabal r

-- | Common info for type/newtype/data/class
data TypeInfo = TypeInfo {
	typeInfoContext :: Maybe String,
	typeInfoArgs :: [String],
	typeInfoDefinition :: Maybe String }
		deriving (Eq, Ord, Read, Show)

instance NFData TypeInfo where
	rnf (TypeInfo c a d) = rnf c `seq` rnf a `seq` rnf d

showTypeInfo :: TypeInfo -> String -> String -> String
showTypeInfo ti pre name = pre ++ maybe "" (++ " =>") (typeInfoContext ti) ++ " " ++ name ++ " " ++ unwords (typeInfoArgs ti) ++ maybe "" (" = " ++) (typeInfoDefinition ti)

-- | Declaration
data Declaration =
	Function { functionType :: Maybe String } |
	Type { typeInfo :: TypeInfo } |
	NewType { newTypeInfo :: TypeInfo } |
	Data { dataInfo :: TypeInfo } |
	Class { classInfo :: TypeInfo }
		deriving (Ord)

instance NFData Declaration where
	rnf (Function f) = rnf f
	rnf (Type i) = rnf i
	rnf (NewType i) = rnf i
	rnf (Data i) = rnf i
	rnf (Class i) = rnf i

instance Eq Declaration where
	(Function l) == (Function r) = l == r
	(Type _) == (Type _) = True
	(NewType _) == (NewType _) = True
	(Data _) == (Data _) = True
	(Class _) == (Class _) = True
	_ == _ = False

-- | Make symbol by name and data
mkSymbol :: String -> a -> Symbol a
mkSymbol name = Symbol name Nothing Nothing Nothing []

-- | Returns `filename:line:column`
position :: Location -> String
position loc =  intercalate ":" [locationFile loc, show $ locationLine loc, show $ locationColumn loc]

-- | Returns qualified name of symbol
symbolQualifiedName :: Symbol a -> String
symbolQualifiedName s = pre ++ symbolName s where
	pre = maybe "" ((++ ".") . symbolName) $ symbolModule s

-- | Make location, canonicalizing path and locating project
mkLocation :: Location -> IO Location
mkLocation loc = do
	f <- canonicalizePath (locationFile loc)
	p <- locateProject f
	return $ loc {
		locationFile = f,
		locationProject = p }

-- | Make location by file, line and column
location :: FilePath -> Int -> Int -> Maybe Project -> Location
location f = Location (normalise f)

-- | Module location is located at file:1:1
moduleLocation :: FilePath -> Location
moduleLocation fname = location fname 1 1 Nothing

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

-- | Brief of declaration
brief :: Symbol Declaration -> String
brief s = case symbol s of
	Function t -> symbolName s ++ maybe "" (" :: " ++) t
	Type t -> showTypeInfo t "type" (symbolName s)
	NewType t -> showTypeInfo t "newtype" (symbolName s)
	Data t -> showTypeInfo t "data" (symbolName s)
	Class t -> showTypeInfo t "class" (symbolName s)

-- | Detailed info about declaration
detailed :: Symbol Declaration -> String
detailed s = unlines $ header ++ docs ++ loc where
	header = [
		brief s,
		"",
		maybe "" symbolName $ symbolModule s]
	docs = maybe [] return $ symbolDocs s
	loc = maybe [] (return . showLoc) $ symbolLocation s where
		showLoc l = "Defined at " ++ position l

-- | Module contents
moduleContents :: Symbol Module -> String
moduleContents s = unlines $ header ++ cts where
	header = [
		"module " ++ symbolName s,
		""]
	cts = map showDecl (M.elems $ moduleDeclarations $ symbol s)
	showDecl d = brief d ++ maybe "" (" -- " ++) (symbolDocs d)

-- | Find project file is related to
locateProject :: FilePath -> IO (Maybe Project)
locateProject file = do
	file' <- canonicalizePath file
	isDir <- doesDirectoryExist file'
	if isDir then locateHere file' else locateParent (takeDirectory file')
	where
		locateHere path = do
			cts <- getDirectoryContents path
			return $ fmap (project . (path </>)) $ find ((== ".cabal") . takeExtension) cts
		locateParent dir = do
			cts <- getDirectoryContents dir
			case find ((== ".cabal") . takeExtension) cts of
				Nothing -> if isDrive dir then return Nothing else locateParent (takeDirectory dir)
				Just cabalFile -> return $ Just $ project (dir </> cabalFile)
