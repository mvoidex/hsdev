{-# LANGUAGE  TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Symbols (
	-- * Information
	Import(..),
	Symbol(..),
	Module(..), ModuleId(..), moduleId, moduleModuleDeclarations,
	Declaration(..),
	TypeInfo(..),
	DeclarationInfo(..),
	ModuleDeclaration(..),
	Inspection(..),
	InspectedModule(..),

	-- * Functions
	showTypeInfo,
	declarationInfo, declarationTypeInfo, declarationTypeCtor, declarationTypeName,
	qualifiedName,
	importQualifier,

	-- * Utility
	Canonicalize(..),
	locateProject,

	-- * Modifiers
	addDeclaration,

	-- * Other
	unalias, moduleContents,

	-- * Reexports
	module HsDev.Symbols.Class,
	module HsDev.Symbols.Documented
	) where

import Control.Arrow
import Control.DeepSeq
import Control.Monad (liftM2)
import Data.List
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M
import Data.Time.Clock.POSIX
import Data.Traversable (traverse)
import System.Directory
import System.FilePath

import HsDev.Symbols.Class
import HsDev.Symbols.Documented
import HsDev.Project
import HsDev.Util

---- | Location of symbol
--data Location = Location {
--	locationFile :: FilePath,
--	locationLine :: Int,
--	locationColumn :: Int,
--	locationProject :: Maybe Project,
--	locationTimeStamp :: Maybe UTCTime }
--		deriving (Eq, Ord)

--instance NFData Location where
--	rnf (Location f l c p t) = rnf f `seq` rnf l `seq` rnf c `seq` rnf p `seq` rnf t

--instance Read POSIXTime where
--	readsPrec i = map (first (fromIntegral :: Integer -> POSIXTime)) . readsPrec i

--instance Show Location where
--	show loc = intercalate ":" [locationFile loc, show $ locationLine loc, show $ locationColumn loc]

-- | Module import
data Import = Import {
	importModuleName :: String,
	importIsQualified :: Bool,
	importAs :: Maybe String,
	importPosition :: Maybe Position }
		deriving (Eq, Ord)

instance NFData Import where
	rnf (Import m q a l) = rnf m `seq` rnf q `seq` rnf a `seq` rnf l

instance Show Import where
	show i = "import " ++ (if importIsQualified i then "qualified " else "") ++ importModuleName i ++ maybe "" (" as " ++) (importAs i)

-- | Imported module can be accessed via qualifier
importQualifier :: Maybe String -> Import -> Bool
importQualifier Nothing i
	| not (importIsQualified i) = True
	| otherwise = False
importQualifier (Just q) i
	| q == importModuleName i = True
	| Just q == importAs i = True
	| otherwise = False

instance Symbol Module where
	symbolName = moduleName
	symbolQualifiedName = moduleName
	symbolDocs = moduleDocs
	symbolLocation m = Location (moduleLocation m) Nothing

instance Symbol Declaration where
	symbolName = declarationName
	symbolQualifiedName = declarationName
	symbolDocs = declarationDocs
	symbolLocation d = Location (MemoryModule Nothing) (declarationPosition d)

instance Symbol ModuleDeclaration where
	symbolName = declarationName . moduleDeclaration
	symbolQualifiedName d = qualifiedName (declarationModule d) (moduleDeclaration d)
	symbolDocs = declarationDocs . moduleDeclaration
	symbolLocation d = (symbolLocation $ declarationModule d) {
		locationPosition = declarationPosition $ moduleDeclaration d }

-- | Module
data Module = Module {
	moduleName :: String,
	moduleDocs :: Maybe String,
	moduleLocation :: ModuleLocation,
	moduleExports :: [String],
	moduleImports :: Map String Import,
	moduleDeclarations :: Map String Declaration }
		deriving (Ord)

instance NFData Module where
	rnf (Module n d s e i ds) = rnf n `seq` rnf d `seq` rnf s `seq` rnf e `seq` rnf i `seq` rnf ds

instance Eq Module where
	l == r = moduleName l == moduleName r && moduleLocation l == moduleLocation r

instance Show Module where
	show m = unlines $ filter (not . null) [
		"module " ++ moduleName m,
		"\tlocation: " ++ show (moduleLocation m),
		"\texports: " ++ intercalate ", " (moduleExports m),
		"\timports:",
		unlines $ map (tab 2 . show) $ M.elems (moduleImports m),
		"\tdeclarations:",
		unlines $ map (tabs 2 . show) $ M.elems (moduleDeclarations m),
		maybe "" ("\tdocs: " ++) (moduleDocs m)]

-- | Get list of declarations as ModuleDeclaration
moduleModuleDeclarations :: Module -> [ModuleDeclaration]
moduleModuleDeclarations m = [ModuleDeclaration m d | d <- M.elems (moduleDeclarations m)]

-- | Module id
data ModuleId = ModuleId {
	moduleIdName :: String,
	moduleIdLocation :: ModuleLocation }
		deriving (Eq, Ord)

instance NFData ModuleId where
	rnf (ModuleId n l) = rnf n `seq` rnf l

instance Show ModuleId where
	show (ModuleId n l) = "module " ++ n ++ " from " ++ show l

-- | Create module id from module
moduleId :: Module -> ModuleId
moduleId m = ModuleId {
	moduleIdName = moduleName m,
	moduleIdLocation = moduleLocation m }

-- | Declaration
data Declaration = Declaration {
	declarationName :: String,
	declarationDocs :: Maybe String,
	declarationPosition :: Maybe Position,
	declaration :: DeclarationInfo }
		deriving (Eq, Ord)

instance NFData Declaration where
	rnf (Declaration n d l x) = rnf n `seq` rnf d `seq` rnf l `seq` rnf x

instance Show Declaration where
	show d = unlines $ filter (not . null) [
		brief d,
		maybe "" ("\tdocs: " ++) $ declarationDocs d,
		maybe "" (("\tlocation: " ++ ) . show) $ declarationPosition d]

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

-- | Declaration info
data DeclarationInfo =
	Function { functionType :: Maybe String } |
	Type { typeInfo :: TypeInfo } |
	NewType { newTypeInfo :: TypeInfo } |
	Data { dataInfo :: TypeInfo } |
	Class { classInfo :: TypeInfo }
		deriving (Ord)

instance NFData DeclarationInfo where
	rnf (Function f) = rnf f
	rnf (Type i) = rnf i
	rnf (NewType i) = rnf i
	rnf (Data i) = rnf i
	rnf (Class i) = rnf i

instance Eq DeclarationInfo where
	(Function l) == (Function r) = l == r
	(Type _) == (Type _) = True
	(NewType _) == (NewType _) = True
	(Data _) == (Data _) = True
	(Class _) == (Class _) = True
	_ == _ = False

-- | Get function type of type info
declarationInfo :: DeclarationInfo -> Either (Maybe String) TypeInfo
declarationInfo (Function t) = Left t
declarationInfo (Type ti) = Right ti
declarationInfo (NewType ti) = Right ti
declarationInfo (Data ti) = Right ti
declarationInfo (Class ti) = Right ti

-- | Get type info of declaration
declarationTypeInfo :: DeclarationInfo -> Maybe TypeInfo
declarationTypeInfo = either (const Nothing) Just . declarationInfo

declarationTypeCtor :: String -> TypeInfo -> DeclarationInfo
declarationTypeCtor "type" = Type
declarationTypeCtor "newtype" = NewType
declarationTypeCtor "data" = Data
declarationTypeCtor "class" = Class
declarationTypeCtor "" = error "Invalid type constructor name"

declarationTypeName :: DeclarationInfo -> Maybe String
declarationTypeName (Type _) = Just "type"
declarationTypeName (NewType _) = Just "newtype"
declarationTypeName (Data _) = Just "data"
declarationTypeName (Class _) = Just "class"
declarationTypeName _ = Nothing

-- | Symbol in module
data ModuleDeclaration = ModuleDeclaration {
	declarationModule :: Module,
	moduleDeclaration :: Declaration }
		deriving (Eq, Ord)

instance NFData ModuleDeclaration where
	rnf (ModuleDeclaration m s) = rnf m `seq` rnf s

instance Show ModuleDeclaration where
	show (ModuleDeclaration m s) = unlines $ filter (not . null) [
		show s,
		"\tmodule: " ++ show (moduleLocation m)]

-- | Returns qualified name of symbol
qualifiedName :: Module -> Declaration -> String
qualifiedName m d = moduleName m ++ "." ++ declarationName d

class Canonicalize a where
	canonicalize :: a -> IO a

instance Canonicalize Cabal where
	canonicalize Cabal = return Cabal
	canonicalize (Sandbox p) = fmap Sandbox $ canonicalizePath p

instance Canonicalize ModuleLocation where
	canonicalize (FileModule f p) = liftM2 FileModule (canonicalizePath f) (traverse canonicalizePath p)
	canonicalize (CabalModule c p n) = fmap (\c' -> CabalModule c' p n) $ canonicalize c
	canonicalize (MemoryModule m) = return $ MemoryModule m

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
				Just cabalf -> return $ Just $ project (dir </> cabalf)

-- | Add declaration to module
addDeclaration :: Declaration -> Module -> Module
addDeclaration decl m = m { moduleDeclarations = decls' } where
	decls' = M.insert (declarationName decl) decl $ moduleDeclarations m

-- | Unalias import name
unalias :: Module -> String -> [String]
unalias m alias = [importModuleName i | i <- M.elems (moduleImports m), importAs i == Just alias]

instance Documented Module where
	brief m = moduleName m ++ " in " ++ show (moduleLocation m)
	detailed m = unlines $ header ++ docs ++ cts where
		header = [brief m, ""]
		docs = maybe [] return $ moduleDocs m
		cts = moduleContents m

instance Documented Declaration where
	brief d = case declarationInfo $ declaration d of
		Left f -> name ++ maybe "" (" :: " ++) f
		Right ti -> showTypeInfo ti (fromMaybe err $ declarationTypeName $ declaration d) name
		where
			name = declarationName d
			err = error "Impossible happened: declarationTypeName"

instance Documented ModuleDeclaration where
	brief = brief . moduleDeclaration

-- | Module contents
moduleContents :: Module -> [String]
moduleContents = map showDecl . M.elems . moduleDeclarations where
	showDecl d = brief d ++ maybe "" (" -- " ++) (declarationDocs d)

-- | Inspection data
data Inspection =
	-- | No inspection
	InspectionNone |
	-- | Time of file inspection
	InspectionTime POSIXTime |
	-- | Flags of cabal module inspection
	InspectionFlags [String]
		deriving (Eq, Ord)

instance NFData Inspection where
	rnf InspectionNone = ()
	rnf (InspectionTime t) = rnf t
	rnf (InspectionFlags fs) = rnf fs

instance Show Inspection where
	show InspectionNone = "none"
	show (InspectionTime tm) = "mtime " ++ show tm
	show (InspectionFlags fs) = "flags [" ++ intercalate ", " fs ++ "]"

instance Read POSIXTime where
	readsPrec i = map (first (fromIntegral :: Integer -> POSIXTime)) . readsPrec i

-- | Inspected module
data InspectedModule = InspectedModule {
	inspection :: Inspection,
	inspectionModule :: ModuleLocation,
	inspectionResult :: Either String Module }
		deriving (Eq, Ord)

instance NFData InspectedModule where
	rnf (InspectedModule i mi m) = rnf i `seq` rnf mi `seq` rnf m

instance Show InspectedModule where
	show (InspectedModule i mi m) = unlines [either showError show m, "\tinspected: " ++ show i] where
		showError :: String -> String
		showError e = unlines $ ("\terror: " ++ e) : case mi of
			FileModule f p -> ["file: " ++ f, "project: " ++ fromMaybe "" p]
			CabalModule c p n -> ["cabal: " ++ show c, "package: " ++ fromMaybe "" p, "name: " ++ n]
			MemoryModule mem -> ["mem: " ++ fromMaybe "" mem]
