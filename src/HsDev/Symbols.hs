{-# LANGUAGE  TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Symbols (
	-- * Information
	Import(..),
	Symbol(..),
	ModuleId(..), Module(..), moduleLocals, moduleLocalDeclarations, moduleModuleDeclarations, moduleId,
	Locals(..),
	Declaration(..), declarationLocals,
	TypeInfo(..),
	DeclarationInfo(..),
	ModuleDeclaration(..),
	Inspection(..), inspectionOpts,
	Inspected(..), InspectedModule,

	-- * Functions
	showTypeInfo,
	declarationInfo, declarationTypeInfo, declarationTypeCtor, declarationTypeName,
	qualifiedName,
	importQualifier,

	-- * Utility
	Canonicalize(..),
	locateProject,
	locateSourceDir,

	-- * Modifiers
	addDeclaration,

	-- * Other
	unalias, moduleContents,

	-- * Reexports
	module HsDev.Symbols.Class,
	module HsDev.Symbols.Documented
	) where

import Control.Applicative
import Control.Arrow
import Control.DeepSeq (NFData(..))
import Control.Monad.Trans.Maybe
import Control.Monad.Error
import Data.Aeson
import Data.List
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Data.Monoid (Monoid(mempty))
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..))
import System.Directory
import System.FilePath

import HsDev.Symbols.Class
import HsDev.Symbols.Documented (Documented(..))
import HsDev.Project
import HsDev.Util (tab, tabs, (.::))

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

instance ToJSON Import where
	toJSON i = object [
		"name" .= importModuleName i,
		"qualified" .= importIsQualified i,
		"as" .= importAs i,
		"pos" .= importPosition i]

instance FromJSON Import where
	parseJSON = withObject "import" $ \v -> Import <$>
		v .:: "name" <*>
		v .:: "qualified" <*>
		v .:: "as" <*>
		v .:: "pos"

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

instance Symbol ModuleId where
	symbolName = moduleIdName
	symbolQualifiedName = moduleIdName
	symbolDocs = const Nothing
	symbolLocation m = Location (moduleIdLocation m) Nothing

instance Symbol Declaration where
	symbolName = declarationName
	symbolQualifiedName = declarationName
	symbolDocs = declarationDocs
	symbolLocation d = Location (ModuleSource Nothing) (declarationPosition d)

instance Symbol ModuleDeclaration where
	symbolName = declarationName . moduleDeclaration
	symbolQualifiedName d = qualifiedName (declarationModuleId d) (moduleDeclaration d)
	symbolDocs = declarationDocs . moduleDeclaration
	symbolLocation d = (symbolLocation $ declarationModuleId d) {
		locationPosition = declarationPosition $ moduleDeclaration d }

-- | Module id
data ModuleId = ModuleId {
	moduleIdName :: String,
	moduleIdLocation :: ModuleLocation }
		deriving (Eq, Ord)

instance NFData ModuleId where
	rnf (ModuleId n l) = rnf n `seq` rnf l

instance Show ModuleId where
	show (ModuleId n l) = "module " ++ n ++ " from " ++ show l

instance ToJSON ModuleId where
	toJSON m = object [
		"name" .= moduleIdName m,
		"location" .= moduleIdLocation m]

instance FromJSON ModuleId where
	parseJSON = withObject "module id" $ \v -> ModuleId <$>
		v .:: "name" <*>
		v .:: "location"

-- | Module
data Module = Module {
	moduleName :: String,
	moduleDocs :: Maybe String,
	moduleLocation :: ModuleLocation,
	moduleExports :: [String],
	moduleImports :: [Import],
	moduleDeclarations :: Map String Declaration }
		deriving (Ord)

instance ToJSON Module where
	toJSON m = object [
		"name" .= moduleName m,
		"docs" .= moduleDocs m,
		"location" .= moduleLocation m,
		"exports" .= moduleExports m,
		"imports" .= moduleImports m,
		"declarations" .= M.elems (moduleDeclarations m)]

instance FromJSON Module where
	parseJSON = withObject "module" $ \v -> Module <$>
		v .:: "name" <*>
		v .:: "docs" <*>
		v .:: "location" <*>
		v .:: "exports" <*>
		v .:: "imports" <*>
		((M.fromList . map (declarationName &&& id)) <$>v .:: "declarations")

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
		unlines $ map (tab 2 . show) $ moduleImports m,
		"\tdeclarations:",
		unlines $ map (tabs 2 . show) $ M.elems (moduleDeclarations m),
		maybe "" ("\tdocs: " ++) (moduleDocs m)]

-- | Bring locals to top
moduleLocals :: Module -> Module
moduleLocals m = m { moduleDeclarations = moduleLocalDeclarations m }

-- | Get declarations with locals
moduleLocalDeclarations :: Module -> Map String Declaration
moduleLocalDeclarations =
	M.fromList .
	map (declarationName &&& id) . 
	concatMap declarationLocals' .
	M.elems .
	moduleDeclarations
	where
		declarationLocals' :: Declaration -> [Declaration]
		declarationLocals' d = d : declarationLocals d

-- | Get list of declarations as ModuleDeclaration
moduleModuleDeclarations :: Module -> [ModuleDeclaration]
moduleModuleDeclarations m = [ModuleDeclaration (moduleId m) d | d <- M.elems (moduleDeclarations m)]

-- Make ModuleId by Module
moduleId :: Module -> ModuleId
moduleId m = ModuleId {
	moduleIdName = moduleName m,
	moduleIdLocation = moduleLocation m }

class Locals a where
	locals :: a -> [Declaration]
	where_ :: a -> [Declaration] -> a

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

instance ToJSON Declaration where
	toJSON d = object [
		"name" .= declarationName d,
		"docs" .= declarationDocs d,
		"pos" .= declarationPosition d,
		"decl" .= declaration d]

instance FromJSON Declaration where
	parseJSON = withObject "declaration" $ \v -> Declaration <$>
		v .:: "name" <*>
		v .:: "docs" <*>
		v .:: "pos" <*>
		v .:: "decl"

instance Locals Declaration where
	locals = locals . declaration
	where_ d ds = d { declaration = where_ (declaration d) ds }

declarationLocals :: Declaration -> [Declaration]
declarationLocals d = map prefix' $ locals $ declaration d where
	prefix' decl = decl { declarationName = declarationName decl }

-- | Common info for type/newtype/data/class
data TypeInfo = TypeInfo {
	typeInfoContext :: Maybe String,
	typeInfoArgs :: [String],
	typeInfoDefinition :: Maybe String }
		deriving (Eq, Ord, Read, Show)

instance NFData TypeInfo where
	rnf (TypeInfo c a d) = rnf c `seq` rnf a `seq` rnf d

instance ToJSON TypeInfo where
	toJSON t = object [
		"ctx" .= typeInfoContext t,
		"args" .= typeInfoArgs t,
		"def" .= typeInfoDefinition t]

instance FromJSON TypeInfo where
	parseJSON = withObject "type info" $ \v -> TypeInfo <$>
		v .:: "ctx" <*>
		v .:: "args" <*>
		v .:: "def"

showTypeInfo :: TypeInfo -> String -> String -> String
showTypeInfo ti pre name = pre ++ maybe "" (++ " =>") (typeInfoContext ti) ++ " " ++ name ++ " " ++ unwords (typeInfoArgs ti) ++ maybe "" (" = " ++) (typeInfoDefinition ti)

-- | Declaration info
data DeclarationInfo =
	Function { functionType :: Maybe String, localDeclarations :: [Declaration] } |
	Type { typeInfo :: TypeInfo } |
	NewType { newTypeInfo :: TypeInfo } |
	Data { dataInfo :: TypeInfo } |
	Class { classInfo :: TypeInfo }
		deriving (Ord)

instance NFData DeclarationInfo where
	rnf (Function f ds) = rnf f `seq` rnf ds
	rnf (Type i) = rnf i
	rnf (NewType i) = rnf i
	rnf (Data i) = rnf i
	rnf (Class i) = rnf i

instance Eq DeclarationInfo where
	(Function l lds) == (Function r rds) = l == r && lds == rds
	(Type _) == (Type _) = True
	(NewType _) == (NewType _) = True
	(Data _) == (Data _) = True
	(Class _) == (Class _) = True
	_ == _ = False

instance ToJSON DeclarationInfo where
	toJSON i = case declarationInfo i of
		Left (t, ds) -> object ["what" .= ("function" :: String), "type" .= t, "locals" .= ds]
		Right ti -> object ["what" .= declarationTypeName i, "info" .= ti]

instance FromJSON DeclarationInfo where
	parseJSON = withObject "declaration info" $ \v -> do
		w <- fmap (id :: String -> String) $ v .:: "what"
		if w == "function"
			then Function <$> v .:: "type" <*> v .:: "locals"
			else declarationTypeCtor w <$> v .:: "info"

instance Locals DeclarationInfo where
	locals (Function _ ds) = ds
	locals _ = []
	where_ (Function n s) ds = Function n (s ++ ds)
	where_ d _ = d

-- | Get function type of type info
declarationInfo :: DeclarationInfo -> Either (Maybe String, [Declaration]) TypeInfo
declarationInfo (Function t ds) = Left (t, ds)
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
	declarationModuleId :: ModuleId,
	moduleDeclaration :: Declaration }
		deriving (Eq, Ord)

instance NFData ModuleDeclaration where
	rnf (ModuleDeclaration m s) = rnf m `seq` rnf s

instance Show ModuleDeclaration where
	show (ModuleDeclaration m s) = unlines $ filter (not . null) [
		show s,
		"\tmodule: " ++ show (moduleIdLocation m)]

instance ToJSON ModuleDeclaration where
	toJSON d = object [
		"module-id" .= declarationModuleId d,
		"declaration" .= moduleDeclaration d]

instance FromJSON ModuleDeclaration where
	parseJSON = withObject "module declaration" $ \v -> ModuleDeclaration <$>
		v .:: "module-id" <*>
		v .:: "declaration"

-- | Returns qualified name of symbol
qualifiedName :: ModuleId -> Declaration -> String
qualifiedName m d = moduleIdName m ++ "." ++ declarationName d

class Canonicalize a where
	canonicalize :: a -> IO a

instance Canonicalize Cabal where
	canonicalize Cabal = return Cabal
	canonicalize (Sandbox p) = fmap Sandbox $ canonicalizePath p

instance Canonicalize Project where
	canonicalize (Project nm p c desc) = liftM3 (Project nm) (canonicalizePath p) (canonicalizePath c) (return desc)

instance Canonicalize ModuleLocation where
	canonicalize (FileModule f p) = liftM2 FileModule (canonicalizePath f) (traverse canonicalize p)
	canonicalize (CabalModule c p n) = fmap (\c' -> CabalModule c' p n) $ canonicalize c
	canonicalize (ModuleSource m) = return $ ModuleSource m

-- | Find project file is related to
locateProject :: FilePath -> IO (Maybe Project)
locateProject file = do
	file' <- canonicalizePath file
	isDir <- doesDirectoryExist file'
	if isDir then locateHere file' else locateParent (takeDirectory file')
	where
		locateHere path = do
			cts <- filter (not . null . takeBaseName) <$> getDirectoryContents path
			return $ fmap (project . (path </>)) $ find ((== ".cabal") . takeExtension) cts
		locateParent dir = do
			cts <- filter (not . null . takeBaseName) <$> getDirectoryContents dir
			case find ((== ".cabal") . takeExtension) cts of
				Nothing -> if isDrive dir then return Nothing else locateParent (takeDirectory dir)
				Just cabalf -> return $ Just $ project (dir </> cabalf)

-- | Locate source dir of file
locateSourceDir :: FilePath -> IO (Maybe FilePath)
locateSourceDir f = runMaybeT $ do
	file <- liftIO $ canonicalizePath f
	p <- MaybeT $ locateProject file
	proj <- MaybeT $ fmap (either (const Nothing) Just) $ runErrorT $ loadProject p
	MaybeT $ return $ findSourceDir proj file

-- | Add declaration to module
addDeclaration :: Declaration -> Module -> Module
addDeclaration decl m = m { moduleDeclarations = decls' } where
	decls' = M.insert (declarationName decl) decl $ moduleDeclarations m

-- | Unalias import name
unalias :: Module -> String -> [String]
unalias m alias = [importModuleName i | i <- moduleImports m, importAs i == Just alias]

instance Documented ModuleId where
	brief m = moduleIdName m ++ " in " ++ show (moduleIdLocation m)

instance Documented Module where
	brief m = moduleName m ++ " in " ++ show (moduleLocation m)
	detailed m = unlines $ header ++ docs ++ cts where
		header = [brief m, ""]
		docs = maybe [] return $ moduleDocs m
		cts = moduleContents m

instance Documented Declaration where
	brief d = case declarationInfo $ declaration d of
		Left (f, _) -> name ++ maybe "" (" :: " ++) f
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
	-- | Time and flags of inspection
	InspectionAt POSIXTime [String]
		deriving (Eq, Ord)

-- | Get inspection opts
inspectionOpts :: Inspection -> [String]
inspectionOpts InspectionNone = []
inspectionOpts (InspectionAt _ opts) = opts

instance NFData Inspection where
	rnf InspectionNone = ()
	rnf (InspectionAt t fs) = rnf t `seq` rnf fs

instance Show Inspection where
	show InspectionNone = "none"
	show (InspectionAt tm fs) = "mtime " ++ show tm ++ ", flags [" ++ intercalate ", " fs ++ "]"

instance Read POSIXTime where
	readsPrec i = map (first (fromIntegral :: Integer -> POSIXTime)) . readsPrec i

instance ToJSON Inspection where
	toJSON InspectionNone = object ["inspected" .= False]
	toJSON (InspectionAt tm fs) = object [
		"mtime" .= (floor tm :: Integer),
		"flags" .= fs]

instance FromJSON Inspection where
	parseJSON = withObject "inspection" $ \v ->
		((const InspectionNone :: Bool -> Inspection) <$> v .:: "inspected") <|>
		(InspectionAt <$> (fromInteger <$> v .:: "mtime") <*> (v .:: "flags"))

-- | Inspected entity
data Inspected i a = Inspected {
	inspection :: Inspection,
	inspectedId :: i,
	inspectionResult :: Either String a }
		deriving (Eq, Ord)

instance Functor (Inspected i) where
	fmap f insp = insp {
		inspectionResult = fmap f (inspectionResult insp) }

instance Foldable (Inspected i) where
	foldMap f = either mempty f . inspectionResult

instance Traversable (Inspected i) where
	traverse f (Inspected insp i r) = Inspected insp i <$> either (pure . Left) (liftA Right . f) r

instance (NFData i, NFData a) => NFData (Inspected i a) where
	rnf (Inspected t i r) = rnf t `seq` rnf i `seq` rnf r

-- | Inspected module
type InspectedModule = Inspected ModuleLocation Module

instance Show InspectedModule where
	show (Inspected i mi m) = unlines [either showError show m, "\tinspected: " ++ show i] where
		showError :: String -> String
		showError e = unlines $ ("\terror: " ++ e) : case mi of
			FileModule f p -> ["file: " ++ f, "project: " ++ maybe "" projectPath p]
			CabalModule c p n -> ["cabal: " ++ show c, "package: " ++ maybe "" show p, "name: " ++ n]
			ModuleSource src -> ["source: " ++ fromMaybe "" src]

instance ToJSON InspectedModule where
	toJSON im = object [
		"inspection" .= inspection im,
		"location" .= inspectedId im,
		either ("error" .=) ("module" .=) (inspectionResult im)]

instance FromJSON InspectedModule where
	parseJSON = withObject "inspected module" $ \v -> Inspected <$>
		v .:: "inspection" <*>
		v .:: "location" <*>
		((Left <$> v .:: "error") <|> (Right <$> v .:: "module"))
