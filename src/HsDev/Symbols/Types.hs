{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Symbols.Types (
	ThingPart(..),
	Export(..), exportQualified, exportName, exportPart, exportModule,
	ImportSpec(..), importSpecName, importSpecPart,
	ImportList(..), hidingList, importSpecs,
	Import(..), importModuleName, importIsQualified, importAs, importList, importPosition,
	ModuleId(..), moduleIdName, moduleIdLocation,
	Module(..), moduleName, moduleDocs, moduleLocation, moduleExports, moduleImports, moduleDeclarations, moduleContents, moduleId,
	Declaration(..), declarationName, declarationDefined, declarationImported, declarationDocs, declarationPosition, declaration, minimalDecl,
	TypeInfo(..), typeInfoContext, typeInfoArgs, typeInfoDefinition, typeInfoFunctions, showTypeInfo,
	DeclarationInfo(..), functionType, localDeclarations, related, typeInfo, declarationInfo, declarationTypeCtor, declarationTypeName,
	ModuleDeclaration(..), declarationModuleId, moduleDeclaration,
	ExportedDeclaration(..), exportedBy, exportedDeclaration,
	Inspection(..), inspectionAt, inspectionOpts,
	Inspected(..), inspection, inspectedId, inspectionTags, inspectionResult, noTags,
	ModuleTag(..),
	InspectedModule, notInspected,

	module HsDev.PackageDb,
	module HsDev.Project,
	module HsDev.Symbols.Class,
	module HsDev.Symbols.Documented
	) where

import Control.Applicative
import Control.Arrow
import Control.Lens (makeLenses, view, set, Simple, Lens, Lens', lens)
import Control.Monad
import Control.DeepSeq (NFData(..))
import Data.Aeson
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Function
import Data.Ord
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time.Clock.POSIX (POSIXTime)

import HsDev.PackageDb
import HsDev.Project
import HsDev.Symbols.Class
import HsDev.Symbols.Documented
import HsDev.Types
import HsDev.Util (tab, tabs, (.::), (.::?), (.::?!), noNulls)

-- | What to export/import for data/class etc
data ThingPart = ThingNothing | ThingAll | ThingWith [Text] deriving (Eq, Ord)

instance NFData ThingPart where
	rnf ThingNothing = ()
	rnf ThingAll = ()
	rnf (ThingWith ns) = rnf ns

instance Show ThingPart where
	show ThingNothing = ""
	show ThingAll = "(..)"
	show (ThingWith ns) = "(" ++ intercalate ", " (map unpack ns) ++ ")"

instance ToJSON ThingPart where
	toJSON ThingNothing = toJSON ("nothing" :: String)
	toJSON ThingAll = toJSON ("all" :: String)
	toJSON (ThingWith ns) = object [
		"with" .= ns]

instance FromJSON ThingPart where
	parseJSON v = parse' <|> parseWith v where
		parse' = do
			s <- parseJSON v
			mplus
				(guard (s == ("nothing" :: String)) >> return ThingNothing)
				(guard (s == ("all" :: String)) >> return ThingAll)
		parseWith = withObject "export part" $ \v' -> ThingWith <$> v' .:: "with"

-- | Module export
data Export =
	ExportName {
		_exportQualified :: Maybe Text,
		_exportName :: Text,
		_exportPart :: ThingPart } |
	ExportModule { _exportModule :: Text }
		deriving (Eq, Ord)

instance NFData Export where
	rnf (ExportName q n w) = rnf q `seq` rnf n `seq` rnf w
	rnf (ExportModule m) = rnf m

instance Show Export where
	show (ExportName Nothing n w) = unpack n ++ show w
	show (ExportName (Just q) n w) = unpack q ++ "." ++ unpack n ++ show w
	show (ExportModule m) = "module " ++ unpack m

instance ToJSON Export where
	toJSON (ExportName q n w) = object ["module" .= q, "name" .= n, "part" .= w]
	toJSON (ExportModule m) = object ["module" .= m]

instance FromJSON Export where
	parseJSON = withObject "export" $ \v ->
		(ExportName <$> (v .:: "module") <*> (v .:: "name") <*> (v .:: "part")) <|>
		(ExportModule <$> (v .:: "module"))

-- | Import spec
data ImportSpec = ImportSpec {
	_importSpecName :: Text,
	_importSpecPart :: ThingPart }
		deriving (Eq, Ord)

instance NFData ImportSpec where
	rnf (ImportSpec n p) = rnf n `seq` rnf p

instance Show ImportSpec where
	show (ImportSpec n p) = unpack n ++ show p

instance ToJSON ImportSpec where
	toJSON (ImportSpec n p) = object ["name" .= n, "part" .= p]

instance FromJSON ImportSpec where
	parseJSON = withObject "import-spec" $ \v -> ImportSpec <$> (v .:: "name") <*> (v .:: "part")

-- | Import list
data ImportList = ImportList {
	_hidingList :: Bool,
	_importSpecs :: [ImportSpec] }
		deriving (Eq, Ord)

instance NFData ImportList where
	rnf (ImportList h ls) = rnf h `seq` rnf ls

instance Show ImportList where
	show (ImportList h ls) = (if h then ("hiding " ++) else id) $ "(" ++ intercalate ", " (map show ls) ++ ")"

instance ToJSON ImportList where
	toJSON (ImportList h ls) = object [
		"hiding" .= h,
		"specs" .= ls]

instance FromJSON ImportList where
	parseJSON = withObject "import-list" $ \v -> ImportList <$>
		v .:: "hiding" <*>
		v .:: "specs"

-- | Module import
data Import = Import {
	_importModuleName :: Text,
	_importIsQualified :: Bool,
	_importAs :: Maybe Text,
	_importList :: Maybe ImportList,
	_importPosition :: Maybe Position }
		deriving (Eq, Ord)

instance NFData Import where
	rnf (Import m q a il l) = rnf m `seq` rnf q `seq` rnf a `seq` rnf il `seq` rnf l

instance Show Import where
	show i = concat [
		"import ",
		if _importIsQualified i then "qualified " else "",
		unpack $ _importModuleName i,
		maybe "" ((" as " ++) . unpack) (_importAs i),
		maybe "" ((" " ++) . show) (_importList i)]

instance ToJSON Import where
	toJSON i = object $ noNulls [
		"name" .= _importModuleName i,
		"qualified" .= _importIsQualified i,
		"as" .= _importAs i,
		"import-list" .= _importList i,
		"pos" .= _importPosition i]

instance FromJSON Import where
	parseJSON = withObject "import" $ \v -> Import <$>
		v .:: "name" <*>
		v .:: "qualified" <*>
		v .::? "as" <*>
		v .::? "import-list" <*>
		v .::? "pos"

-- | Module id
data ModuleId = ModuleId {
	_moduleIdName :: Text,
	_moduleIdLocation :: ModuleLocation }
		deriving (Eq, Ord)

instance NFData ModuleId where
	rnf (ModuleId n l) = rnf n `seq` rnf l

instance Show ModuleId where
	show (ModuleId n l) = "module " ++ unpack n ++ " from " ++ show l

instance ToJSON ModuleId where
	toJSON m = object [
		"name" .= _moduleIdName m,
		"location" .= _moduleIdLocation m]

instance FromJSON ModuleId where
	parseJSON = withObject "module id" $ \v -> ModuleId <$>
		v .:: "name" <*>
		v .:: "location"

-- | Module
data Module = Module {
	_moduleName :: Text,
	_moduleDocs :: Maybe Text,
	_moduleLocation :: ModuleLocation,
	_moduleExports :: Maybe [Export],
	_moduleImports :: [Import],
	_moduleDeclarations :: [Declaration] }
		deriving (Ord)

instance ToJSON Module where
	toJSON m = object $ noNulls [
		"name" .= _moduleName m,
		"docs" .= _moduleDocs m,
		"location" .= _moduleLocation m,
		"exports" .= _moduleExports m,
		"imports" .= _moduleImports m,
		"declarations" .= _moduleDeclarations m]

instance FromJSON Module where
	parseJSON = withObject "module" $ \v -> Module <$>
		v .:: "name" <*>
		v .::? "docs" <*>
		v .:: "location" <*>
		v .::? "exports" <*>
		v .::?! "imports" <*>
		v .::?! "declarations"

instance NFData Module where
	rnf (Module n d s e i ds) = rnf n `seq` rnf d `seq` rnf s `seq` rnf e `seq` rnf i `seq` rnf ds

instance Eq Module where
	l == r = _moduleName l == _moduleName r && _moduleLocation l == _moduleLocation r

instance Show Module where
	show m = unlines $ filter (not . null) [
		"module " ++ unpack (_moduleName m),
		"\tlocation: " ++ show (_moduleLocation m),
		"\texports: " ++ maybe "*" (intercalate ", " . map show) (_moduleExports m),
		"\timports:",
		unlines $ map (tab 2 . show) $ _moduleImports m,
		"\tdeclarations:",
		unlines $ map (tabs 2 . show) $ _moduleDeclarations m,
		maybe "" (("\tdocs: " ++) . unpack) (_moduleDocs m)]

moduleId :: Simple Lens Module ModuleId
moduleId = lens
	(uncurry ModuleId . (_moduleName &&& _moduleLocation))
	(\m mi -> m { _moduleName = _moduleIdName mi, _moduleLocation = _moduleIdLocation mi })

-- | Module contents
moduleContents :: Module -> [String]
moduleContents = map showDecl . _moduleDeclarations where
	showDecl d = brief d ++ maybe "" ((" -- " ++) . unpack) (_declarationDocs d)

-- | Declaration
data Declaration = Declaration {
	_declarationName :: Text,
	_declarationDefined :: Maybe ModuleId, -- ^ Where declaration defined, @Nothing@ if here
	_declarationImported :: Maybe [Import], -- ^ Declaration imported with. @Nothing@ if unknown (cabal modules) or here (source file)
	_declarationDocs :: Maybe Text,
	_declarationPosition :: Maybe Position,
	_declaration :: DeclarationInfo }
		deriving (Eq, Ord)

instance NFData Declaration where
	rnf (Declaration n def is d l x) = rnf n `seq` rnf def `seq` rnf is `seq` rnf d `seq` rnf l `seq` rnf x

instance Show Declaration where
	show d = unlines $ filter (not . null) [
		brief d,
		maybe "" (("\tdocs: " ++) . unpack) $ _declarationDocs d,
		maybe "" (("\tdefined in: " ++) . show) $ _declarationDefined d,
		maybe "" (("\tlocation: " ++ ) . show) $ _declarationPosition d]

instance ToJSON Declaration where
	toJSON d = object $ noNulls [
		"name" .= _declarationName d,
		"defined" .= _declarationDefined d,
		"imported" .= _declarationImported d,
		"docs" .= _declarationDocs d,
		"pos" .= _declarationPosition d,
		"decl" .= _declaration d]

instance FromJSON Declaration where
	parseJSON = withObject "declaration" $ \v -> Declaration <$>
		v .:: "name" <*>
		v .::? "defined" <*>
		v .::? "imported" <*>
		v .::? "docs" <*>
		v .::? "pos" <*>
		v .:: "decl"

-- | Minimal declaration info without defined, docs and position
minimalDecl :: Lens' Declaration Declaration
minimalDecl = lens to' from' where
	to' :: Declaration -> Declaration
	to' decl' = decl' { _declarationDefined = Nothing, _declarationDocs = Nothing, _declarationPosition = Nothing }
	from' :: Declaration -> Declaration -> Declaration
	from' decl' mdecl = decl' { _declarationName = _declarationName mdecl, _declarationImported = _declarationImported mdecl, _declaration = _declaration mdecl }

-- | Common info for type, newtype, data and class
data TypeInfo = TypeInfo {
	_typeInfoContext :: Maybe Text,
	_typeInfoArgs :: [Text],
	_typeInfoDefinition :: Maybe Text,
	_typeInfoFunctions :: [Text] }
		deriving (Eq, Ord, Read, Show)

instance NFData TypeInfo where
	rnf (TypeInfo c a d f) = rnf c `seq` rnf a `seq` rnf d `seq` rnf f

instance ToJSON TypeInfo where
	toJSON t = object $ noNulls [
		"ctx" .= _typeInfoContext t,
		"args" .= _typeInfoArgs t,
		"def" .= _typeInfoDefinition t,
		"funs" .= _typeInfoFunctions t]

instance FromJSON TypeInfo where
	parseJSON = withObject "type info" $ \v -> TypeInfo <$>
		v .::? "ctx" <*>
		v .::?! "args" <*>
		v .::? "def" <*>
		v .::?! "funs"

showTypeInfo :: TypeInfo -> String -> String -> String
showTypeInfo ti pre name = concat [
	pre,
	maybe "" ((++ " =>") . unpack) (_typeInfoContext ti), " ",
	name, " ",
	unwords (map unpack $ _typeInfoArgs ti),
	maybe "" ((" = " ++) . unpack) (_typeInfoDefinition ti)]

-- | Declaration info
data DeclarationInfo =
	Function { _functionType :: Maybe Text, _localDeclarations :: [Declaration], _related :: Maybe Text } |
	Type { _typeInfo :: TypeInfo } |
	NewType { _typeInfo :: TypeInfo } |
	Data { _typeInfo :: TypeInfo } |
	Class { _typeInfo :: TypeInfo }
		deriving (Ord)

-- | Get function type of type info
declarationInfo :: DeclarationInfo -> Either (Maybe Text, [Declaration], Maybe Text) TypeInfo
declarationInfo (Function t ds r) = Left (t, ds, r)
declarationInfo (Type ti) = Right ti
declarationInfo (NewType ti) = Right ti
declarationInfo (Data ti) = Right ti
declarationInfo (Class ti) = Right ti

declarationTypeCtor :: String -> TypeInfo -> DeclarationInfo
declarationTypeCtor "type" = Type
declarationTypeCtor "newtype" = NewType
declarationTypeCtor "data" = Data
declarationTypeCtor "class" = Class
declarationTypeCtor _ = error "Invalid type constructor name"

declarationTypeName :: DeclarationInfo -> Maybe String
declarationTypeName (Type _) = Just "type"
declarationTypeName (NewType _) = Just "newtype"
declarationTypeName (Data _) = Just "data"
declarationTypeName (Class _) = Just "class"
declarationTypeName _ = Nothing

instance NFData DeclarationInfo where
	rnf (Function f ds r) = rnf f `seq` rnf ds `seq` rnf r
	rnf (Type i) = rnf i
	rnf (NewType i) = rnf i
	rnf (Data i) = rnf i
	rnf (Class i) = rnf i

instance Eq DeclarationInfo where
	(Function l lds lr) == (Function r rds rr) = l == r && lds == rds && lr == rr
	(Type _) == (Type _) = True
	(NewType _) == (NewType _) = True
	(Data _) == (Data _) = True
	(Class _) == (Class _) = True
	_ == _ = False

instance ToJSON DeclarationInfo where
	toJSON i = case declarationInfo i of
		Left (t, ds, r) -> object $ noNulls ["what" .= ("function" :: String), "type" .= t, "locals" .= ds, "related" .= r]
		Right ti -> object ["what" .= declarationTypeName i, "info" .= ti]

instance FromJSON DeclarationInfo where
	parseJSON = withObject "declaration info" $ \v -> do
		w <- fmap (id :: String -> String) $ v .:: "what"
		if w == "function"
			then Function <$> v .::? "type" <*> v .::?! "locals" <*> v .::? "related"
			else declarationTypeCtor w <$> v .:: "info"

-- | Symbol in context of some module
data ModuleDeclaration = ModuleDeclaration {
	_declarationModuleId :: ModuleId,
	_moduleDeclaration :: Declaration }
		deriving (Eq, Ord)

instance NFData ModuleDeclaration where
	rnf (ModuleDeclaration m s) = rnf m `seq` rnf s

instance Show ModuleDeclaration where
	show (ModuleDeclaration m s) = unlines $ filter (not . null) [
		show s,
		"\tmodule: " ++ show (_moduleIdLocation m)]

instance ToJSON ModuleDeclaration where
	toJSON d = object [
		"module-id" .= _declarationModuleId d,
		"declaration" .= _moduleDeclaration d]

instance FromJSON ModuleDeclaration where
	parseJSON = withObject "module declaration" $ \v -> ModuleDeclaration <$>
		v .:: "module-id" <*>
		v .:: "declaration"

-- | Symbol exported with
data ExportedDeclaration = ExportedDeclaration {
	_exportedBy :: [ModuleId],
	_exportedDeclaration :: Declaration }
		deriving (Eq, Ord)

instance NFData ExportedDeclaration where
	rnf (ExportedDeclaration m s) = rnf m `seq` rnf s

instance Show ExportedDeclaration where
	show (ExportedDeclaration m s) = unlines $ filter (not . null) [
		show s,
		"\tmodules: " ++ intercalate ", " (map (show . _moduleIdLocation) m)]

instance ToJSON ExportedDeclaration where
	toJSON d = object [
		"exported-by" .= _exportedBy d,
		"declaration" .= _exportedDeclaration d]

instance FromJSON ExportedDeclaration where
	parseJSON = withObject "exported declaration" $ \v -> ExportedDeclaration <$>
		v .:: "exported-by" <*>
		v .:: "declaration"

-- | Inspection data
data Inspection =
	-- | No inspection
	InspectionNone |
	-- | Time and flags of inspection
	InspectionAt {
		_inspectionAt :: POSIXTime,
		_inspectionOpts :: [String] }
			deriving (Eq, Ord)

instance NFData Inspection where
	rnf InspectionNone = ()
	rnf (InspectionAt t fs) = rnf t `seq` rnf fs

instance Show Inspection where
	show InspectionNone = "none"
	show (InspectionAt tm fs) = "mtime " ++ show tm ++ ", flags [" ++ intercalate ", " fs ++ "]"

instance Read POSIXTime where
	readsPrec i = map (first (fromIntegral :: Integer -> POSIXTime)) . readsPrec i

instance Monoid Inspection where
	mempty = InspectionNone
	mappend InspectionNone r = r
	mappend l InspectionNone = l
	mappend (InspectionAt ltm lopts) (InspectionAt rtm ropts)
		| ltm >= rtm = InspectionAt ltm lopts
		| otherwise = InspectionAt rtm ropts

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
data Inspected i t a = Inspected {
	_inspection :: Inspection,
	_inspectedId :: i,
	_inspectionTags :: Set t,
	_inspectionResult :: Either HsDevError a }

inspectedTup :: Inspected i t a -> (Inspection, i, Set t, Maybe a)
inspectedTup (Inspected insp i tags res) = (insp, i, tags, either (const Nothing) Just res)

instance (Eq i, Eq t, Eq a) => Eq (Inspected i t a) where
	(==) = (==) `on` inspectedTup

instance (Ord i, Ord t, Ord a) => Ord (Inspected i t a) where
	compare = comparing inspectedTup

instance Functor (Inspected i t) where
	fmap f insp = insp {
		_inspectionResult = fmap f (_inspectionResult insp) }

instance Foldable (Inspected i t) where
	foldMap f = either mempty f . _inspectionResult

instance Traversable (Inspected i t) where
	traverse f (Inspected insp i ts r) = Inspected insp i ts <$> either (pure . Left) (liftA Right . f) r

instance (NFData i, NFData t, NFData a) => NFData (Inspected i t a) where
	rnf (Inspected t i ts r) = rnf t `seq` rnf i `seq` rnf ts `seq` rnf r

-- | Empty tags
noTags :: Set t
noTags = S.empty

data ModuleTag = InferredTypesTag | RefinedDocsTag deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance NFData ModuleTag where
	rnf InferredTypesTag = ()
	rnf RefinedDocsTag = ()

instance ToJSON ModuleTag where
	toJSON InferredTypesTag = toJSON ("types" :: String)
	toJSON RefinedDocsTag = toJSON ("docs" :: String)

instance FromJSON ModuleTag where
	parseJSON = withText "module-tag" $ \txt -> msum [
		guard (txt == "types") >> return InferredTypesTag,
		guard (txt == "docs") >> return RefinedDocsTag]

-- | Inspected module
type InspectedModule = Inspected ModuleLocation ModuleTag Module

instance Show InspectedModule where
	show (Inspected i mi ts m) = unlines [either showError show m, "\tinspected: " ++ show i, "\ttags: " ++ intercalate ", " (map show $ S.toList ts)] where
		showError :: HsDevError -> String
		showError e = unlines $ ("\terror: " ++ show e) : case mi of
			FileModule f p -> ["file: " ++ f, "project: " ++ maybe "" (view projectPath) p]
			InstalledModule c p n -> ["cabal: " ++ show c, "package: " ++ maybe "" show p, "name: " ++ n]
			ModuleSource src -> ["source: " ++ fromMaybe "" src]

instance ToJSON InspectedModule where
	toJSON im = object [
		"inspection" .= _inspection im,
		"location" .= _inspectedId im,
		"tags" .= S.toList (_inspectionTags im),
		either ("error" .=) ("module" .=) (_inspectionResult im)]

instance FromJSON InspectedModule where
	parseJSON = withObject "inspected module" $ \v -> Inspected <$>
		v .:: "inspection" <*>
		v .:: "location" <*>
		(S.fromList <$> (v .::?! "tags")) <*>
		((Left <$> v .:: "error") <|> (Right <$> v .:: "module"))

notInspected :: ModuleLocation -> InspectedModule
notInspected mloc = Inspected mempty mloc noTags (Left $ NotInspected mloc)

instance Symbol Module where
	symbolName = _moduleName
	symbolQualifiedName = _moduleName
	symbolDocs = _moduleDocs
	symbolLocation m = Location (_moduleLocation m) Nothing

instance Symbol ModuleId where
	symbolName = _moduleIdName
	symbolQualifiedName = _moduleIdName
	symbolDocs = const Nothing
	symbolLocation m = Location (_moduleIdLocation m) Nothing

instance Symbol Declaration where
	symbolName = _declarationName
	symbolQualifiedName = _declarationName
	symbolDocs = _declarationDocs
	symbolLocation d = Location (ModuleSource Nothing) (_declarationPosition d)

instance Symbol ModuleDeclaration where
	symbolName = _declarationName . _moduleDeclaration
	symbolQualifiedName d = qualifiedName (_declarationModuleId d) (_moduleDeclaration d) where
		qualifiedName :: ModuleId -> Declaration -> Text
		qualifiedName m' d' = T.concat [_moduleIdName m', ".", _declarationName d']
	symbolDocs = _declarationDocs . _moduleDeclaration
	symbolLocation d = set locationPosition (_declarationPosition $ _moduleDeclaration d)
		(symbolLocation . _declarationModuleId $ d)

instance Documented ModuleId where
	brief m = unpack (_moduleIdName m) ++ " in " ++ show (_moduleIdLocation m)

instance Documented Module where
	brief m = unpack (_moduleName m) ++ " in " ++ show (_moduleLocation m)
	detailed m = unlines $ header ++ docs ++ cts where
		header = [brief m, ""]
		docs = maybe [] (return . unpack) $ _moduleDocs m
		cts = moduleContents m

instance Documented Declaration where
	brief d = case declarationInfo $ _declaration d of
		Left (f, _, _) -> name ++ maybe "" ((" :: " ++) . unpack) f
		Right ti -> showTypeInfo ti (fromMaybe err $ declarationTypeName $ _declaration d) name
		where
			name = unpack $ _declarationName d
			err = error "Impossible happened: declarationTypeName"

instance Documented ModuleDeclaration where
	brief = brief . _moduleDeclaration

makeLenses ''Export
makeLenses ''ImportSpec
makeLenses ''ImportList
makeLenses ''Import
makeLenses ''ModuleId
makeLenses ''DeclarationInfo
makeLenses ''TypeInfo
makeLenses ''Declaration
makeLenses ''Module
makeLenses ''ModuleDeclaration
makeLenses ''ExportedDeclaration
makeLenses ''Inspection
makeLenses ''Inspected
