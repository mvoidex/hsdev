{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Symbols.Types (
	Name, nameModule, nameIdent, namePrefix, fromName_, toName_, fromName, toName,
	Module(..), moduleSymbols, exportedSymbols, scopeSymbols, fixitiesMap, moduleFixities, moduleId, moduleDocs, moduleExports, moduleScope, moduleSource,
	Symbol(..), symbolId, symbolDocs, symbolPosition, symbolInfo,
	SymbolInfo(..), functionType, parentClass, parentType, selectorConstructors, typeArgs, typeContext, familyAssociate, symbolType,
	infoOf, nullifyInfo,
	Inspection(..), inspectionAt, inspectionOpts, Inspected(..), inspection, inspectedKey, inspectionTags, inspectionResult, inspected,
	inspectedTup, noTags, ModuleTag(..), InspectedModule, notInspected,
	briefSymbol,

	module HsDev.PackageDb,
	module HsDev.Project,
	module HsDev.Symbols.Class,
	module HsDev.Symbols.Location,
	module HsDev.Symbols.Documented
	) where

import Control.Applicative
import Control.Arrow
import Control.Lens (makeLenses, set, view, Lens', lens, Traversal, Traversal', _Right, _Just)
import Control.Monad
import Control.DeepSeq (NFData(..))
import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Function
import Data.Ord
import Data.String (fromString)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time.Clock.POSIX (POSIXTime)
import Language.Haskell.Exts (QName(..), ModuleName(..), Boxed(..), SpecialCon(..), Fixity(..), Assoc(..))
import qualified Language.Haskell.Exts as Exts (Name(..))
import Text.Format

import Control.Apply.Util (chain)
import HsDev.PackageDb
import HsDev.Project
import HsDev.Symbols.Class
import HsDev.Symbols.Location
import HsDev.Symbols.Documented
import HsDev.Symbols.Parsed
import HsDev.Types
import HsDev.Util ((.::), (.::?), (.::?!), noNulls)

-- | Qualified name
type Name = QName ()

nameModule :: Name -> Maybe Text
nameModule (Qual _ (ModuleName _ m) _) = Just $ fromString m
nameModule _ = Nothing

nameIdent :: Name -> Text
nameIdent (Qual _ _ n) = fromName_ n
nameIdent (UnQual _ n) = fromName_ n
nameIdent s = fromName s

namePrefix :: Name -> Name -> Bool
namePrefix p s = nameModule p == nameModule s && nameIdent p `T.isPrefixOf` nameIdent s

fromName_ :: Exts.Name () -> Text
fromName_ (Exts.Ident _ s') = fromString s'
fromName_ (Exts.Symbol _ s') = fromString s'

toName_ :: Text -> Exts.Name ()
toName_ = Exts.Ident () . T.unpack

toName :: Text -> Name
toName "()" = Special () (UnitCon ())
toName "[]" = Special () (ListCon ())
toName "->" = Special () (FunCon ())
toName "(:)" = Special () (Cons ())
toName "(# #)" = Special () (UnboxedSingleCon ())
toName tup
	| T.all (== ',') noBraces = Special () (TupleCon () Boxed (succ $ T.length noBraces))
	where
		noBraces = T.dropAround (`elem` ['(', ')']) tup
toName n = case T.split (== '.') n of
	[n'] -> UnQual () (Exts.Ident () $ T.unpack n')
	ns -> Qual () (ModuleName () (T.unpack $ T.intercalate "." $ init ns)) (Exts.Ident () (T.unpack $ last ns))

fromName :: Name -> Text
fromName (Qual _ (ModuleName _ m) n) = T.concat [fromString m, ".", fromName_ n]
fromName (UnQual _ n) = fromName_ n
fromName (Special _ c) = case c of
	UnitCon _ -> "()"
	ListCon _ -> "[]"
	FunCon _ -> "->"
	TupleCon _ _ i -> fromString $ "(" ++ replicate (pred i) ',' ++ ")"
	Cons _ -> "(:)"
	UnboxedSingleCon _ -> "(# #)"

instance NFData l => NFData (ModuleName l) where
	rnf (ModuleName l n) = rnf l `seq` rnf n

instance NFData l => NFData (Exts.Name l) where
	rnf (Exts.Ident l s) = rnf l `seq` rnf s
	rnf (Exts.Symbol l s) = rnf l `seq` rnf s

instance NFData Boxed where
	rnf Boxed = ()
	rnf Unboxed = ()

instance NFData l => NFData (SpecialCon l) where
	rnf (UnitCon l) = rnf l
	rnf (ListCon l) = rnf l
	rnf (FunCon l) = rnf l
	rnf (TupleCon l b i) = rnf l `seq` rnf b `seq` rnf i
	rnf (Cons l) = rnf l
	rnf (UnboxedSingleCon l) = rnf l

instance NFData l => NFData (QName l) where
	rnf (Qual l m n) = rnf l `seq` rnf m `seq` rnf n
	rnf (UnQual l n) = rnf l `seq` rnf n
	rnf (Special l s) = rnf l `seq` rnf s

-- | Module
data Module = Module {
	_moduleId :: ModuleId,
	_moduleDocs :: Maybe Text,
	_moduleExports :: [Symbol], -- exported module symbols
	_moduleFixities :: [Fixity], -- fixities of operators
	_moduleScope :: Map Name [Symbol], -- symbols in scope, only for source modules
	_moduleSource :: Maybe Parsed } -- source of module

moduleSymbols :: Traversal' Module Symbol
moduleSymbols f m = (\e s -> m { _moduleExports = e, _moduleScope = s }) <$>
	traverse f (_moduleExports m) <*>
	traverse (traverse f) (_moduleScope m)

exportedSymbols :: Traversal' Module Symbol
exportedSymbols f m = (\e -> m { _moduleExports = e }) <$> traverse f (_moduleExports m)

scopeSymbols :: Traversal' Module (Name, Symbol)
scopeSymbols f m = (\s -> m { _moduleScope = toMap s }) <$> traverse f (fromMap $ _moduleScope m) where
	toMap ns = M.unionsWith (++) [M.singleton n [s] | (n, s) <- ns]
	fromMap ms = [(n, s) | (n, ss) <- M.toList ms, s <- ss]

fixitiesMap :: Lens' Module (Map Name Fixity)
fixitiesMap = lens g' s' where
	g' m = mconcat [M.singleton n f | f@(Fixity _ _ n) <- _moduleFixities m]
	s' m m' = m { _moduleFixities = M.elems m' }

instance ToJSON (Assoc ()) where
	toJSON (AssocNone _) = toJSON ("none" :: String)
	toJSON (AssocLeft _) = toJSON ("left" :: String)
	toJSON (AssocRight _) = toJSON ("right" :: String)

instance FromJSON (Assoc ()) where
	parseJSON = withText "assoc" $ \txt -> msum [
		guard (txt == "none") >> return (AssocNone ()),
		guard (txt == "left") >> return (AssocLeft ()),
		guard (txt == "right") >> return (AssocRight ())]

instance ToJSON Fixity where
	toJSON (Fixity assoc pr n) = object $ noNulls [
		"assoc" .= assoc,
		"prior" .= pr,
		"name" .= fromName n]

instance FromJSON Fixity where
	parseJSON = withObject "fixity" $ \v -> Fixity <$>
		v .:: "assoc" <*>
		v .:: "prior" <*>
		(toName <$> v .:: "name")

instance ToJSON Module where
	toJSON m = object $ noNulls [
		"id" .= _moduleId m,
		"docs" .= _moduleDocs m,
		"exports" .= _moduleExports m,
		"fixities" .= _moduleFixities m]

instance FromJSON Module where
	parseJSON = withObject "module" $ \v -> Module <$>
		v .:: "id" <*>
		v .::? "docs" <*>
		v .::?! "exports" <*>
		v .::?! "fixities" <*>
		pure mempty <*>
		pure Nothing

instance NFData (Assoc ()) where
	rnf (AssocNone _) = ()
	rnf (AssocLeft _) = ()
	rnf (AssocRight _) = ()

instance NFData Fixity where
	rnf (Fixity assoc pr n) = rnf assoc `seq` rnf pr `seq` rnf n

instance NFData Module where
	rnf (Module i d e fs s _) = rnf i `seq` rnf d `seq` rnf e `seq` rnf fs `seq` rnf s

instance Eq Module where
	l == r = _moduleId l == _moduleId r

instance Ord Module where
	compare l r = compare (_moduleId l) (_moduleId r)

instance Show Module where
	show = show . _moduleId

data Symbol = Symbol {
	_symbolId :: SymbolId,
	_symbolDocs :: Maybe Text,
	_symbolPosition :: Maybe Position,
	_symbolInfo :: SymbolInfo }

instance Eq Symbol where
	l == r = (_symbolId l, symbolType l) == (_symbolId r, symbolType r)

instance Ord Symbol where
	compare l r = compare (_symbolId l, symbolType l) (_symbolId r, symbolType r)

instance NFData Symbol where
	rnf (Symbol i d l info) = rnf i `seq` rnf d `seq` rnf l `seq` rnf info

instance Show Symbol where
	show = show . _symbolId

instance ToJSON Symbol where
	toJSON s = object $ noNulls [
		"id" .= _symbolId s,
		"docs" .= _symbolDocs s,
		"pos" .= _symbolPosition s,
		"info" .= _symbolInfo s]

instance FromJSON Symbol where
	parseJSON = withObject "symbol" $ \v -> Symbol <$>
		v .:: "id" <*>
		v .::? "docs" <*>
		v .::? "pos" <*>
		v .:: "info"

-- | Get brief information for completions, without docs and position
briefSymbol :: Lens' Symbol Symbol
briefSymbol = lens to' from' where
	to' :: Symbol -> Symbol
	to' s = s { _symbolDocs = Nothing, _symbolPosition = Nothing }
	from' :: Symbol -> Symbol -> Symbol
	from' s ms = s { _symbolId = _symbolId ms, _symbolDocs = _symbolDocs ms, _symbolPosition = _symbolPosition ms }

data SymbolInfo =
	Function { _functionType :: Maybe Text } |
	Method { _functionType :: Maybe Text, _parentClass :: Text } |
	Selector { _functionType :: Maybe Text, _parentType :: Text, _selectorConstructors :: [Text] } |
	Constructor { _typeArgs :: [Text], _parentType :: Text } |
	Type { _typeArgs :: [Text], _typeContext :: [Text] } |
	NewType { _typeArgs :: [Text], _typeContext :: [Text] } |
	Data { _typeArgs :: [Text], _typeContext :: [Text] } |
	Class { _typeArgs :: [Text], _typeContext :: [Text] } |
	TypeFam { _typeArgs :: [Text], _typeContext :: [Text], _familyAssociate :: Maybe Text } |
	DataFam { _typeArgs :: [Text], _typeContext :: [Text], _familyAssociate :: Maybe Text } |
	PatSyn { _typeArgs :: [Text], _typeContext :: [Text] }
		deriving (Eq, Ord, Read, Show)

instance NFData SymbolInfo where
	rnf (Function ft) = rnf ft
	rnf (Method ft cls) = rnf ft `seq` rnf cls
	rnf (Selector ft t cs) = rnf ft `seq` rnf t `seq` rnf cs
	rnf (Constructor as t) = rnf as `seq` rnf t
	rnf (Type as ctx) = rnf as `seq` rnf ctx
	rnf (NewType as ctx) = rnf as `seq` rnf ctx
	rnf (Data as ctx) = rnf as `seq` rnf ctx
	rnf (Class as ctx) = rnf as `seq` rnf ctx
	rnf (PatSyn as ctx) = rnf as `seq` rnf ctx
	rnf (TypeFam as ctx a) = rnf as `seq` rnf ctx `seq` rnf a
	rnf (DataFam as ctx a) = rnf as `seq` rnf ctx `seq` rnf a

instance ToJSON SymbolInfo where
	toJSON (Function ft) = object [what "function", "type" .= ft]
	toJSON (Method ft cls) = object [what "method", "type" .= ft, "class" .= cls]
	toJSON (Selector ft t cs) = object [what "selector", "type" .= ft, "type" .= t, "constructors" .= cs]
	toJSON (Constructor as t) = object [what "ctor", "args" .= as, "type" .= t]
	toJSON (Type as ctx) = object [what "type", "args" .= as, "ctx" .= ctx]
	toJSON (NewType as ctx) = object [what "newtype", "args" .= as, "ctx" .= ctx]
	toJSON (Data as ctx) = object [what "data", "args" .= as, "ctx" .= ctx]
	toJSON (Class as ctx) = object [what "class", "args" .= as, "ctx" .= ctx]
	toJSON (PatSyn as ctx) = object [what "patsyn", "args" .= as, "ctx" .= ctx]
	toJSON (TypeFam as ctx a) = object [what "type-family", "args" .= as, "ctx" .= ctx, "associate" .= a]
	toJSON (DataFam as ctx a) = object [what "data-family", "args" .= as, "ctx" .= ctx, "associate" .= a]

class EmptySymbolInfo a where
	infoOf :: a -> SymbolInfo

instance EmptySymbolInfo SymbolInfo where
	infoOf = id

instance (Monoid a, EmptySymbolInfo r) => EmptySymbolInfo (a -> r) where
	infoOf f = infoOf $ f mempty

symbolType :: Symbol -> String
symbolType s = case _symbolInfo s of
	Function{} -> "function"
	Method{} -> "method"
	Selector{} -> "selector"
	Constructor{} -> "ctor"
	Type{} -> "type"
	NewType{} -> "newtype"
	Data{} -> "data"
	Class{} -> "class"
	PatSyn{} -> "patsyn"
	TypeFam{} -> "type-family"
	DataFam{} -> "data-family"

what :: String -> Pair
what n = "what" .= n

instance FromJSON SymbolInfo where
	parseJSON = withObject "symbol info" $ \v -> msum [
		gwhat "function" v >> (Function <$> v .::? "type"),
		gwhat "method" v >> (Method <$> v .::? "type" <*> v .:: "class"),
		gwhat "selector" v >> (Selector <$> v .::? "type" <*> v .:: "type" <*> v .::?! "constructors"),
		gwhat "ctor" v >> (Constructor <$> v .::?! "args" <*> v .:: "type"),
		gwhat "type" v >> (Type <$> v .::?! "args" <*> v .::?! "ctx"),
		gwhat "newtype" v >> (NewType <$> v .::?! "args" <*> v .::?! "ctx"),
		gwhat "data" v >> (Data <$> v .::?! "args" <*> v .::?! "ctx"),
		gwhat "class" v >> (Class <$> v .::?! "args" <*> v .::?! "ctx"),
		gwhat "patsyn" v >> (PatSyn <$> v .::?! "args" <*> v .::?! "ctx"),
		gwhat "type-family" v >> (TypeFam <$> v .::?! "args" <*> v .::?! "ctx" <*> v .::? "associate"),
		gwhat "data-family" v >> (DataFam <$> v .::?! "args" <*> v .::?! "ctx" <*> v .::? "associate")]

gwhat :: String -> Object -> Parser ()
gwhat n v = do
	s <- v .:: "what"
	guard (s == n)

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
data Inspected k t a = Inspected {
	_inspection :: Inspection,
	_inspectedKey :: k,
	_inspectionTags :: Set t,
	_inspectionResult :: Either HsDevError a }

inspectedTup :: Inspected k t a -> (Inspection, k, Set t, Maybe a)
inspectedTup (Inspected insp i tags res) = (insp, i, tags, either (const Nothing) Just res)

instance (Eq k, Eq t, Eq a) => Eq (Inspected k t a) where
	(==) = (==) `on` inspectedTup

instance (Ord k, Ord t, Ord a) => Ord (Inspected k t a) where
	compare = comparing inspectedTup

instance Functor (Inspected k t) where
	fmap f insp = insp {
		_inspectionResult = fmap f (_inspectionResult insp) }

instance Foldable (Inspected k t) where
	foldMap f = either mempty f . _inspectionResult

instance Traversable (Inspected k t) where
	traverse f (Inspected insp i ts r) = Inspected insp i ts <$> either (pure . Left) (liftA Right . f) r

instance (NFData k, NFData t, NFData a) => NFData (Inspected k t a) where
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
			OtherLocation src -> ["other location: " ++ src]
			NoLocation -> ["no location"]

instance ToJSON InspectedModule where
	toJSON im = object [
		"inspection" .= _inspection im,
		"location" .= _inspectedKey im,
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

instance Documented ModuleId where
	brief m = brief $ _moduleLocation m
	detailed = brief

instance Documented SymbolId where
	brief s = "{} from {}" ~~ _symbolName s ~~ brief (_symbolModule s)
	detailed = brief

instance Documented Module where
	brief = brief . _moduleId
	detailed m = unlines (brief m : info) where
		info = [
			"\texports: {}" ~~ intercalate ", " (map brief (_moduleExports m))]

instance Documented Symbol where
	brief = brief . _symbolId
	detailed s = unlines [brief s, info] where
		info = case _symbolInfo s of
			Function t -> "\t" ++ intercalate ", " (catMaybes [Just "function", fmap ("type: {}" ~~) t])
			Method t p -> "\t" ++ intercalate ", " (catMaybes [Just "method", fmap ("type: {}" ~~) t, Just $ "parent: {}" ~~ p])
			Selector t p _ -> "\t" ++ intercalate ", " (catMaybes [Just "selector", fmap ("type: {}" ~~) t, Just $ "parent: {}" ~~ p])
			Constructor args p -> "\t" ++ intercalate ", " ["constructor", "args: {}" ~~ T.unwords args, "parent: {}" ~~ p]
			Type args ctx -> "\t" ++ intercalate ", " ["type", "args: {}" ~~ T.unwords args, "ctx: {}" ~~ T.unwords ctx]
			NewType args ctx -> "\t" ++ intercalate ", " ["newtype", "args: {}" ~~ T.unwords args, "ctx: {}" ~~ T.unwords ctx]
			Data args ctx -> "\t" ++ intercalate ", " ["data", "args: {}" ~~ T.unwords args, "ctx: {}" ~~ T.unwords ctx]
			Class args ctx -> "\t" ++ intercalate ", " ["class", "args: {}" ~~ T.unwords args, "ctx: {}" ~~ T.unwords ctx]
			PatSyn args ctx -> "\t" ++ intercalate ", " ["pattern synonym", "args: {}" ~~ T.unwords args, "ctx: {}" ~~ T.unwords ctx]
			TypeFam args ctx _ -> "\t" ++ intercalate ", " ["type family", "args: {}" ~~ T.unwords args, "ctx: {}" ~~ T.unwords ctx]
			DataFam args ctx _ -> "\t" ++ intercalate ", " ["data family", "args: {}" ~~ T.unwords args, "ctx: {}" ~~ T.unwords ctx]

makeLenses ''Module
makeLenses ''Symbol
makeLenses ''SymbolInfo
makeLenses ''Inspection
makeLenses ''Inspected

inspected :: Traversal (Inspected k t a) (Inspected k t b) a b
inspected = inspectionResult . _Right

nullifyInfo :: SymbolInfo -> SymbolInfo
nullifyInfo = chain [
	set functionType mempty,
	set parentClass mempty,
	set parentType mempty,
	set selectorConstructors mempty,
	set typeArgs mempty,
	set typeContext mempty,
	set familyAssociate mempty]

instance Sourced Module where
	sourcedName = moduleId . moduleName
	sourcedDocs = moduleDocs . _Just
	sourcedModule = moduleId

instance Sourced Symbol where
	sourcedName = symbolId . symbolName
	sourcedDocs = symbolDocs . _Just
	sourcedModule = symbolId . symbolModule
	sourcedLocation = symbolPosition . _Just
