{-# LANGUAGE CPP, TemplateHaskell, TypeSynonymInstances, FlexibleInstances, OverloadedStrings, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Symbols.Types (
	Import(..), importPosition, importName, importQualified, importAs,
	Module(..), moduleSymbols, exportedSymbols, scopeSymbols, fixitiesMap, moduleFixities, moduleId, moduleDocs, moduleImports, moduleExports, moduleScope, moduleSource,
	Symbol(..), symbolId, symbolDocs, symbolPosition, symbolInfo,
	SymbolInfo(..), functionType, parentClass, parentType, selectorConstructors, typeArgs, typeContext, familyAssociate, symbolInfoType, symbolType, patternType, patternConstructor,
	Scoped(..), scopeQualifier, scoped,
	SymbolUsage(..), symbolUsed, symbolUsedQualifier, symbolUsedIn, symbolUsedRegion,
	ImportedSymbol(..), importedSymbol, importedFrom,
	infoOf, nullifyInfo,
	Inspection(..), inspectionAt, inspectionOpts, fresh, Inspected(..), inspection, inspectedKey, inspectionTags, inspectionResult, inspected,
	InspectM(..), runInspect, continueInspect, inspect, inspect_, withInspection,
	inspectedTup, noTags, tag, ModuleTag(..), InspectedModule, notInspected,

	module HsDev.PackageDb.Types,
	module HsDev.Project,
	module HsDev.Symbols.Name,
	module HsDev.Symbols.Class,
	module HsDev.Symbols.Location,
	module HsDev.Symbols.Documented
	) where

import Control.Arrow
import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Morph
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.DeepSeq (NFData(..))
import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Maybe.JustIf
import Data.Monoid (Any(..))
import Data.Monoid hiding ((<>))
import Data.Function
import Data.Ord
import Data.Semigroup
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time.Clock.POSIX (POSIXTime)
import Language.Haskell.Exts (QName(..), ModuleName(..), Boxed(..), SpecialCon(..), Fixity(..), Assoc(..))
import qualified Language.Haskell.Exts as Exts (Name(..))
import Text.Format

import Control.Apply.Util (chain)
import HsDev.Display
import HsDev.Error
import HsDev.PackageDb.Types
import HsDev.Project
import HsDev.Symbols.Name
import HsDev.Symbols.Class
import HsDev.Symbols.Location
import HsDev.Symbols.Documented
import HsDev.Symbols.Parsed
import HsDev.Util ((.::), (.::?), (.::?!), noNulls, objectUnion)
import System.Directory.Paths

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
#if MIN_VERSION_haskell_src_exts(1,20,0)
	rnf (ExprHole l) = rnf l
#endif

instance NFData l => NFData (QName l) where
	rnf (Qual l m n) = rnf l `seq` rnf m `seq` rnf n
	rnf (UnQual l n) = rnf l `seq` rnf n
	rnf (Special l s) = rnf l `seq` rnf s

-- | Import
data Import = Import {
	_importPosition :: Position, -- source line of import
	_importName :: Text, -- imported module name
	_importQualified :: Bool, -- is import qualified
	_importAs :: Maybe Text } -- alias of import
		deriving (Eq, Ord)

instance NFData Import where
	rnf (Import p n q a) = rnf p `seq` rnf n `seq` rnf q `seq` rnf a

instance Show Import where
	show (Import _ n q a) = concat $ catMaybes [
		Just "import",
		"qualified" `justIf` q,
		Just $ show n,
		fmap (("as " ++) . show) a]

instance ToJSON Import where
	toJSON (Import p n q a) = object [
		"pos" .= p,
		"name" .= n,
		"qualified" .= q,
		"as" .= a]

instance FromJSON Import where
	parseJSON = withObject "import" $ \v -> Import <$>
		v .:: "pos" <*>
		v .:: "name" <*>
		v .:: "qualified" <*>
		v .:: "as"

-- | Module
data Module = Module {
	_moduleId :: ModuleId,
	_moduleDocs :: Maybe Text,
	_moduleImports :: [Import], -- list of module names imported
	_moduleExports :: [Symbol], -- exported module symbols
	_moduleFixities :: [Fixity], -- fixities of operators
	_moduleScope :: Map Name [Symbol], -- symbols in scope, only for source modules
	_moduleSource :: Maybe Parsed } -- source of module

-- | Make each symbol appear only once
moduleSymbols :: Traversal' Module Symbol
moduleSymbols f m = getBack <$> (each . _1) f revList where
	revList = M.toList $ M.unionsWith mappend $ concat [
		[M.singleton sym ([], Any True) | sym <- _moduleExports m],
		[M.singleton sym ([nm], Any False) | (nm, syms) <- M.toList (_moduleScope m), sym <- syms]]
	getBack syms = m {
		_moduleExports = [sym' | (sym', (_, Any True)) <- syms],
		_moduleScope = M.unionsWith (++) [M.singleton n [sym'] | (sym', (ns, _)) <- syms, n <- ns] }

exportedSymbols :: Traversal' Module Symbol
exportedSymbols f m = (\e -> m { _moduleExports = e }) <$> traverse f (_moduleExports m)

scopeSymbols :: Traversal' Module (Symbol, [Name])
scopeSymbols f m = (\s -> m { _moduleScope = invMap s }) <$> traverse f (M.toList . invMap . M.toList $ _moduleScope m) where
	invMap :: Ord b => [(a, [b])] -> Map b [a]
	invMap es = M.unionsWith (++) [M.singleton v [k] | (k, vs) <- es, v <- vs]

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
		"imports" .= _moduleImports m,
		"exports" .= _moduleExports m,
		"fixities" .= _moduleFixities m]

instance FromJSON Module where
	parseJSON = withObject "module" $ \v -> Module <$>
		v .:: "id" <*>
		v .::? "docs" <*>
		v .::?! "imports" <*>
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
	rnf (Module i d is e fs s msrc) = msrc `seq` rnf i `seq` rnf d `seq` rnf is `seq` rnf e `seq` rnf fs `seq` rnf s

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
	PatConstructor { _typeArgs :: [Text], _patternType :: Maybe Text } |
	PatSelector { _functionType :: Maybe Text, _patternType :: Maybe Text, _patternConstructor :: Text }
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
	rnf (TypeFam as ctx a) = rnf as `seq` rnf ctx `seq` rnf a
	rnf (DataFam as ctx a) = rnf as `seq` rnf ctx `seq` rnf a
	rnf (PatConstructor as t) = rnf as `seq` rnf t
	rnf (PatSelector ft t c) = rnf ft `seq` rnf t `seq` rnf c

instance ToJSON SymbolInfo where
	toJSON (Function ft) = object [what "function", "type" .= ft]
	toJSON (Method ft cls) = object [what "method", "type" .= ft, "class" .= cls]
	toJSON (Selector ft t cs) = object [what "selector", "type" .= ft, "parent" .= t, "constructors" .= cs]
	toJSON (Constructor as t) = object [what "ctor", "args" .= as, "type" .= t]
	toJSON (Type as ctx) = object [what "type", "args" .= as, "ctx" .= ctx]
	toJSON (NewType as ctx) = object [what "newtype", "args" .= as, "ctx" .= ctx]
	toJSON (Data as ctx) = object [what "data", "args" .= as, "ctx" .= ctx]
	toJSON (Class as ctx) = object [what "class", "args" .= as, "ctx" .= ctx]
	toJSON (TypeFam as ctx a) = object [what "type-family", "args" .= as, "ctx" .= ctx, "associate" .= a]
	toJSON (DataFam as ctx a) = object [what "data-family", "args" .= as, "ctx" .= ctx, "associate" .= a]
	toJSON (PatConstructor as t) = object [what "pat-ctor", "args" .= as, "pat-type" .= t]
	toJSON (PatSelector ft t c) = object [what "pat-selector", "type" .= ft, "pat-type" .= t, "constructor" .= c]

class EmptySymbolInfo a where
	infoOf :: a -> SymbolInfo

instance EmptySymbolInfo SymbolInfo where
	infoOf = id

instance (Monoid a, EmptySymbolInfo r) => EmptySymbolInfo (a -> r) where
	infoOf f = infoOf $ f mempty

symbolInfoType :: SymbolInfo -> String
symbolInfoType (Function{}) = "function"
symbolInfoType (Method{}) = "method"
symbolInfoType (Selector{}) = "selector"
symbolInfoType (Constructor{}) = "ctor"
symbolInfoType (Type{}) = "type"
symbolInfoType (NewType{}) = "newtype"
symbolInfoType (Data{}) = "data"
symbolInfoType (Class{}) = "class"
symbolInfoType (TypeFam{}) = "type-family"
symbolInfoType (DataFam{}) = "data-family"
symbolInfoType (PatConstructor{}) = "pat-ctor"
symbolInfoType (PatSelector{}) = "pat-selector"

symbolType :: Symbol -> String
symbolType = symbolInfoType . _symbolInfo

what :: String -> Pair
what n = "what" .= n

instance FromJSON SymbolInfo where
	parseJSON = withObject "symbol info" $ \v -> msum [
		gwhat "function" v >> (Function <$> v .::? "type"),
		gwhat "method" v >> (Method <$> v .::? "type" <*> v .:: "class"),
		gwhat "selector" v >> (Selector <$> v .::? "type" <*> v .:: "parent" <*> v .::?! "constructors"),
		gwhat "ctor" v >> (Constructor <$> v .::?! "args" <*> v .:: "type"),
		gwhat "type" v >> (Type <$> v .::?! "args" <*> v .::?! "ctx"),
		gwhat "newtype" v >> (NewType <$> v .::?! "args" <*> v .::?! "ctx"),
		gwhat "data" v >> (Data <$> v .::?! "args" <*> v .::?! "ctx"),
		gwhat "class" v >> (Class <$> v .::?! "args" <*> v .::?! "ctx"),
		gwhat "type-family" v >> (TypeFam <$> v .::?! "args" <*> v .::?! "ctx" <*> v .::? "associate"),
		gwhat "data-family" v >> (DataFam <$> v .::?! "args" <*> v .::?! "ctx" <*> v .::? "associate"),
		gwhat "pat-ctor" v >> (PatConstructor <$> v .::?! "args" <*> v .::? "pat-type"),
		gwhat "pat-selector" v >> (PatSelector <$> v .::? "type" <*> v .::? "pat-type" <*> v .:: "constructor")]

gwhat :: String -> Object -> Parser ()
gwhat n v = do
	s <- v .:: "what"
	guard (s == n)

-- | Scoped entity with qualifier
data Scoped a = Scoped {
	_scopeQualifier :: Maybe Text,
	_scoped :: a }
		deriving (Eq, Ord)

instance Show a => Show (Scoped a) where
	show (Scoped q s) = maybe "" (\q' -> T.unpack q' ++ ".") q ++ show s

instance ToJSON a => ToJSON (Scoped a) where
	toJSON (Scoped q s) = toJSON s `objectUnion` object (noNulls ["qualifier" .= q])

instance FromJSON a => FromJSON (Scoped a) where
	parseJSON = withObject "scope-symbol" $ \v -> Scoped <$>
		(v .::? "qualifier") <*>
		parseJSON (Object v)

-- | Symbol usage
data SymbolUsage = SymbolUsage {
	_symbolUsed :: Symbol,
	_symbolUsedQualifier :: Maybe Text,
	_symbolUsedIn :: ModuleId,
	_symbolUsedRegion :: Region }
		deriving (Eq, Ord)

instance Show SymbolUsage where
	show (SymbolUsage s _ m p) = show s ++ " at " ++ show m ++ ":" ++ show p

instance ToJSON SymbolUsage where
	toJSON (SymbolUsage s q m p) = object $ noNulls ["symbol" .= s, "qualifier" .= q, "in" .= m, "at" .= p]

instance FromJSON SymbolUsage where
	parseJSON = withObject "symbol-usage" $ \v -> SymbolUsage <$>
		v .:: "symbol" <*>
		v .::? "qualifier" <*>
		v .:: "in" <*>
		v .:: "at"

-- | Symbol with module it's exported from
data ImportedSymbol = ImportedSymbol {
	_importedSymbol :: Symbol,
	_importedFrom :: ModuleId }
		deriving (Eq, Ord)

instance Show ImportedSymbol where
	show (ImportedSymbol s m) = show s ++ " imported from " ++ show m

instance ToJSON ImportedSymbol where
	toJSON (ImportedSymbol s m) = objectUnion (toJSON s) $ object [
		"imported" .= m]

instance FromJSON ImportedSymbol where
	parseJSON = withObject "imported-symbol" $ \v -> ImportedSymbol <$>
		parseJSON (Object v) <*>
		v .:: "imported"

-- | Inspection data
data Inspection =
	-- | No inspection
	InspectionNone |
	-- | Time and flags of inspection
	InspectionAt {
		_inspectionAt :: POSIXTime,
		_inspectionOpts :: [Text] }
			deriving (Eq, Ord)

instance NFData Inspection where
	rnf InspectionNone = ()
	rnf (InspectionAt t fs) = rnf t `seq` rnf fs

instance Show Inspection where
	show InspectionNone = "none"
	show (InspectionAt tm fs) = "mtime " ++ show tm ++ ", flags [" ++ intercalate ", " (map T.unpack fs) ++ "]"

instance Read POSIXTime where
	readsPrec i = map (first (fromIntegral :: Integer -> POSIXTime)) . readsPrec i

instance Semigroup Inspection where
	InspectionNone <> r = r
	l <> InspectionNone = l
	InspectionAt ltm lopts <> InspectionAt rtm ropts
		| ltm >= rtm = InspectionAt ltm lopts
		| otherwise = InspectionAt rtm ropts

instance Monoid Inspection where
	mempty = InspectionNone
	mappend l r = l <> r

instance ToJSON Inspection where
	toJSON InspectionNone = object ["inspected" .= False]
	toJSON (InspectionAt tm fs) = object [
		"mtime" .= (fromRational (toRational tm) :: Double),
		"flags" .= fs]

instance FromJSON Inspection where
	parseJSON = withObject "inspection" $ \v ->
		((const InspectionNone :: Bool -> Inspection) <$> v .:: "inspected") <|>
		(InspectionAt <$> ((fromRational . (toRational :: Double -> Rational)) <$> v .:: "mtime") <*> (v .:: "flags"))

-- | Is left @Inspection@ fresh comparing to right one
fresh :: Inspection -> Inspection -> Bool
fresh InspectionNone InspectionNone = True
fresh InspectionNone _ = False
fresh _ InspectionNone = True
fresh (InspectionAt tm _) (InspectionAt tm' _) = tm' - tm < 0.01

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

instance (ToJSON k, ToJSON t, ToJSON a) => ToJSON (Inspected k t a) where
	toJSON im = object [
		"inspection" .= _inspection im,
		"location" .= _inspectedKey im,
		"tags" .= S.toList (_inspectionTags im),
		either ("error" .=) ("result" .=) (_inspectionResult im)]

instance (FromJSON k, Ord t, FromJSON t, FromJSON a) => FromJSON (Inspected k t a) where
	parseJSON = withObject "inspected" $ \v -> Inspected <$>
		v .:: "inspection" <*>
		v .:: "location" <*>
		(S.fromList <$> (v .::?! "tags")) <*>
		((Left <$> v .:: "error") <|> (Right <$> v .:: "result"))

newtype InspectM k t m a = InspectM { runInspectM :: ReaderT k (ExceptT HsDevError (StateT (Inspection, S.Set t) m)) a }
	deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO, MonadThrow, MonadCatch, MonadReader k, MonadError HsDevError, MonadState (Inspection, S.Set t))

instance MonadTrans (InspectM k t) where
	lift = InspectM . lift . lift . lift

runInspect :: (Monad m, Ord t) => k -> InspectM k t m a -> m (Inspected k t a)
runInspect key act = do
	(res, (insp, ts)) <- flip runStateT (InspectionNone, mempty) . runExceptT . flip runReaderT key . runInspectM $ act
	return $ Inspected insp key ts res

-- | Continue inspection
continueInspect :: (Monad m, Ord t) => Inspected k t a -> (a -> InspectM k t m b) -> m (Inspected k t b)
continueInspect start act = runInspect (_inspectedKey start) $ do
	put (_inspection start, _inspectionTags start)
	val <- either throwError return $ _inspectionResult start
	act val

inspect :: MonadCatch m => m Inspection -> (k -> m a) -> InspectM k t m a
inspect insp act = withInspection insp $ do
	key <- ask
	lift (hsdevCatch (hsdevLiftIO $ act key)) >>= either throwError return

withInspection :: MonadCatch m => m Inspection -> InspectM k t m a -> InspectM k t m a
withInspection insp inner = do
	insp' <- lift insp
	let
		setInsp = modify (set _1 insp')
	catchError (inner <* setInsp) (\e -> setInsp >> throwError e)

inspect_ :: MonadCatch m => m Inspection -> m a -> InspectM k t m a
inspect_ insp = inspect insp . const

-- | Empty tags
noTags :: Set t
noTags = S.empty

-- | One tag
tag :: t -> Set t
tag = S.singleton

data ModuleTag = InferredTypesTag | RefinedDocsTag | OnlyHeaderTag | DirtyTag | ResolvedNamesTag deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance NFData ModuleTag where
	rnf InferredTypesTag = ()
	rnf RefinedDocsTag = ()
	rnf OnlyHeaderTag = ()
	rnf DirtyTag = ()
	rnf ResolvedNamesTag = ()

instance Display ModuleTag where
	display InferredTypesTag = "types"
	display RefinedDocsTag = "docs"
	display OnlyHeaderTag = "header"
	display DirtyTag = "dirty"
	display ResolvedNamesTag = "resolved"
	displayType _ = "module-tag"

instance ToJSON ModuleTag where
	toJSON InferredTypesTag = toJSON ("types" :: String)
	toJSON RefinedDocsTag = toJSON ("docs" :: String)
	toJSON OnlyHeaderTag = toJSON ("header" :: String)
	toJSON DirtyTag = toJSON ("dirty" :: String)
	toJSON ResolvedNamesTag = toJSON ("resolved" :: String)

instance FromJSON ModuleTag where
	parseJSON = withText "module-tag" $ \txt -> msum [
		guard (txt == "types") >> return InferredTypesTag,
		guard (txt == "docs") >> return RefinedDocsTag,
		guard (txt == "header") >> return OnlyHeaderTag,
		guard (txt == "dirty") >> return DirtyTag,
		guard (txt == "resolved") >> return ResolvedNamesTag]

-- | Inspected module
type InspectedModule = Inspected ModuleLocation ModuleTag Module

instance Show InspectedModule where
	show (Inspected i mi ts m) = unlines [either showError show m, "\tinspected: " ++ show i, "\ttags: " ++ intercalate ", " (map show $ S.toList ts)] where
		showError :: HsDevError -> String
		showError e = unlines $ ("\terror: " ++ show e) : case mi of
			FileModule f p -> ["file: " ++ f ^. path, "project: " ++ maybe "" (view (projectPath . path)) p]
			InstalledModule c p n _  -> ["cabal: " ++ show c, "package: " ++ show p, "name: " ++ T.unpack n]
			OtherLocation src -> ["other location: " ++ T.unpack src]
			NoLocation -> ["no location"]

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
	detailed m = T.unlines (brief m : info) where
		info = [
			"\texports: {}" ~~ T.intercalate ", " (map brief (_moduleExports m))]

instance Documented Symbol where
	brief = brief . _symbolId
	detailed s = T.unlines [brief s, info] where
		info = case _symbolInfo s of
			Function t -> "\t" `T.append` T.intercalate ", " (catMaybes [Just "function", fmap ("type: {}" ~~) t])
			Method t p -> "\t" `T.append` T.intercalate ", " (catMaybes [Just "method", fmap ("type: {}" ~~) t, Just $ "parent: {}" ~~ p])
			Selector t p _ -> "\t" `T.append` T.intercalate ", " (catMaybes [Just "selector", fmap ("type: {}" ~~) t, Just $ "parent: {}" ~~ p])
			Constructor args p -> "\t" `T.append` T.intercalate ", " ["constructor", "args: {}" ~~ T.unwords args, "parent: {}" ~~ p]
			Type args ctx -> "\t" `T.append` T.intercalate ", " ["type", "args: {}" ~~ T.unwords args, "ctx: {}" ~~ T.unwords ctx]
			NewType args ctx -> "\t" `T.append` T.intercalate ", " ["newtype", "args: {}" ~~ T.unwords args, "ctx: {}" ~~ T.unwords ctx]
			Data args ctx -> "\t" `T.append` T.intercalate ", " ["data", "args: {}" ~~ T.unwords args, "ctx: {}" ~~ T.unwords ctx]
			Class args ctx -> "\t" `T.append` T.intercalate ", " ["class", "args: {}" ~~ T.unwords args, "ctx: {}" ~~ T.unwords ctx]
			TypeFam args ctx _ -> "\t" `T.append` T.intercalate ", " ["type family", "args: {}" ~~ T.unwords args, "ctx: {}" ~~ T.unwords ctx]
			DataFam args ctx _ -> "\t" `T.append` T.intercalate ", " ["data family", "args: {}" ~~ T.unwords args, "ctx: {}" ~~ T.unwords ctx]
			PatConstructor args p -> "\t" `T.append` T.intercalate ", " (catMaybes [Just "pattern constructor", Just $ "args: {}" ~~ T.unwords args, fmap ("pat-type: {}" ~~) p])
			PatSelector t p _ -> "\t" `T.append` T.intercalate ", " (catMaybes [Just "pattern selector", fmap ("type: {}" ~~) t, fmap ("pat-type: {}" ~~) p])

makeLenses ''Import
makeLenses ''Module
makeLenses ''Symbol
makeLenses ''SymbolInfo
makeLenses ''Scoped
makeLenses ''SymbolUsage
makeLenses ''ImportedSymbol
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
	set familyAssociate mempty,
	set patternType mempty,
	set patternConstructor mempty]

instance Sourced Module where
	sourcedName = moduleId . moduleName
	sourcedDocs = moduleDocs . _Just
	sourcedModule = moduleId

instance Sourced Symbol where
	sourcedName = symbolId . symbolName
	sourcedDocs = symbolDocs . _Just
	sourcedModule = symbolId . symbolModule
	sourcedLocation = symbolPosition . _Just
