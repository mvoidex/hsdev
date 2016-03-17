{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module HsDev.Symbols.Resolve (
	ResolveM(..),ResolvedTree, ResolvedModule(..), resolvedModule, resolvedScope, resolvedExports,
	scopeModule, exportsModule, resolvedTopScope,
	resolve, resolveOne, resolveModule, ExportMap(..), exportMap, exported, resolveImport,
	mergeImported
	) where

import Control.Arrow
import Control.Lens (makeLenses, view, preview, set, _Just)
import Control.Monad.Reader
import Control.Monad.State
import Data.Function (on)
import Data.List (sortBy, groupBy, delete)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe, catMaybes, maybeToList)
import Data.Maybe.JustIf
import Data.Ord (comparing)
import Data.String (fromString)
import Data.Text (Text)

import HsDev.Database
import HsDev.Symbols
import HsDev.Symbols.Util
import HsDev.Util (uniqueBy, ordNub)

-- | Map from name to modules
type ModuleMap = M.Map Text [Module]

-- | Resolve monad uses existing @Database@ and @ResolvedTree@ as state.
newtype ResolveM a = ResolveM { runResolveM :: ReaderT (Database, ModuleMap) (State ResolvedTree) a }
	deriving (Functor, Applicative, Monad, MonadState ResolvedTree, MonadReader (Database, ModuleMap))

-- | Tree of resolved modules
type ResolvedTree = Map ModuleId ResolvedModule

-- | Module with declarations bringed to scope and with exported declarations
data ResolvedModule = ResolvedModule {
	_resolvedModule :: Module,
	_resolvedScope :: [Declaration],
	_resolvedExports :: [Declaration] }

makeLenses ''ResolvedModule

-- | Make @Module@ with scope declarations
scopeModule :: ResolvedModule -> Module
scopeModule r = set moduleDeclarations (view resolvedScope r) (view resolvedModule r)

-- | Make @Module@ with exported only declarations
exportsModule :: ResolvedModule -> Module
exportsModule r = set moduleDeclarations (view resolvedExports r) (view resolvedModule r)

-- | Get top-level scope
resolvedTopScope :: ResolvedModule -> [Declaration]
resolvedTopScope = filter isTop . view resolvedScope where
	isTop :: Declaration -> Bool
	isTop = any (not . view importIsQualified) . fromMaybe [] . view declarationImported

-- | Resolve modules, function is not IO, so all file names must be canonicalized
resolve :: (Traversable t, Foldable t) => Database -> t Module -> t ResolvedModule
resolve db = flip evalState M.empty . flip runReaderT (db, m) . runResolveM . traverse resolveModule where
	m :: ModuleMap
	m = M.fromList $ map ((view moduleName . head) &&& id) $
		groupBy ((==) `on` view moduleName) $
		sortBy (comparing (view moduleName)) $ allModules db

-- | Resolve one module
resolveOne :: Database -> Module -> ResolvedModule
resolveOne db = fromMaybe (error "Resolve: impossible happened") . resolve db . Just

-- | Resolve module
resolveModule :: Module -> ResolveM ResolvedModule
resolveModule m = gets (M.lookup $ view moduleId m) >>= maybe resolveModule' return where
	resolveModule' = save $ case view moduleLocation m of
		InstalledModule {} -> return ResolvedModule {
			_resolvedModule = m,
			_resolvedScope = view moduleDeclarations m,
			_resolvedExports = view moduleDeclarations m }
		_ -> do
			scope' <-
				liftM ((thisDecls ++) . mergeImported . concat) .
				mapM (resolveImport m) .
				(import_ (fromString "Prelude") :) .
				view moduleImports $ m
			let
				exported' = case view moduleExports m of
					Nothing -> thisDecls
					Just exports' -> unique $ concatMap (exported (exportMap scope')) exports'
			return $ ResolvedModule m (sortDeclarations scope') (sortDeclarations exported')
	thisDecls :: [Declaration]
	thisDecls = map (selfDefined . selfImport) $ view moduleDeclarations m
	selfDefined :: Declaration -> Declaration
	selfDefined = set declarationDefined (Just $ view moduleId m)
	selfImport :: Declaration -> Declaration
	selfImport = set declarationImported (Just [import_ $ view moduleName m])
	save :: ResolveM ResolvedModule -> ResolveM ResolvedModule
	save act = do
		rm <- act
		modify $ M.insert (view (resolvedModule . moduleId) rm) rm
		return rm
	unique :: [Declaration] -> [Declaration]
	unique = uniqueBy declId
	declId :: Declaration -> (Text, Maybe ModuleId)
	declId = view declarationName &&& view declarationDefined

-- | Map from name (or module name) to declarations, that can be possibly exported with this name (or module name)
data ExportMap = ExportMap {
	exportMapName :: M.Map (Maybe Text, Text) [Declaration],
	exportMapModule :: M.Map Text [Declaration] }

lookup_ :: Ord a => a -> M.Map a [b] -> [b]
lookup_ k = fromMaybe [] . M.lookup k

exportMap :: [Declaration] -> ExportMap
exportMap decls = ExportMap
	(mkMap byName')
	(mkMap byModule')
	where
		mkMap :: Ord a => (Declaration -> [a]) -> M.Map a [Declaration]
		mkMap fn = toMap $ concatMap fn' decls where
			fn' decl' = zip (fn decl') (repeat decl')
		toMap :: Ord a => [(a, Declaration)] -> M.Map a [Declaration]
		toMap =
			M.fromList . map (first head . unzip) .
			groupBy ((==) `on` fst) . sortBy (comparing fst)
		byName' :: Declaration -> [(Maybe Text, Text)]
		byName' decl' = [(iname, nm) | iname <- inames, nm <- declNames] where
			inames = (if any (not . view importIsQualified) (imported' decl') then (Nothing :) else id) (map Just qnames)
			declNames = view declarationName decl' : maybeToList (preview (declaration . related . _Just) decl')
			qnames = ordNub [nm |
				i <- imported' decl',
				view importIsQualified i,
				nm <- importNames i]
		byModule' :: Declaration -> [Text]
		byModule' decl' = concatMap imports' (imported' decl')
		imported' :: Declaration -> [Import]
		imported' = fromMaybe [] . view declarationImported
		imports' :: Import -> [Text]
		imports' i
			| view importIsQualified i = []
			| otherwise = importNames i

exported :: ExportMap -> Export -> [Declaration]
exported emap (ExportName q n p) = filter check' $ lookup_ (q, n) (exportMapName emap) where
	check' decl' = view declarationName decl' == n || inner' where
		inner' = preview (declaration . related . _Just) decl' == Just n && case p of
			ExportNothing -> False
			ExportAll -> True
			ExportWith ns -> view declarationName decl' `elem` ns
exported emap (ExportModule m) = lookup_ m (exportMapModule emap)

-- | Bring declarations into scope
resolveImport :: Module -> Import -> ResolveM [Declaration]
resolveImport m i = liftM (map $ setImport i) resolveImport' where
	resolveImport' :: ResolveM [Declaration]
	resolveImport' = do
		ms <- case view moduleLocation m of
			FileModule file proj -> do
				db <- asks fst
				let
					proj' = proj >>= refineProject db
				case proj' of
					Nothing -> selectImport i [
						inFile $ importedModulePath (view moduleName m) file (view importModuleName i),
						installed]
					Just p -> selectImport i [
						inProject p,
						inDepsOf' file p]
			InstalledModule pdb _ _ -> selectImport i [inPackageDb pdb]
			ModuleSource _ -> selectImport i [installed]
		fromMaybe [] <$> traverse (liftM (filterImportList . view resolvedExports) . resolveModule) ms
	setImport :: Import -> Declaration -> Declaration
	setImport i' = set declarationImported (Just [i'])
	selectImport :: Import -> [ModuleId -> Bool] -> ResolveM (Maybe Module)
	selectImport i' fs = do
		modsMap <- asks snd
		let
			mods = fromMaybe [] $ M.lookup (view importModuleName i') modsMap
		return $
			listToMaybe $
			newestPackage $
			fromMaybe [] $
			listToMaybe $ dropWhile null
				[filter (f . view moduleId) mods | f <- fs]
	filterImportList :: [Declaration] -> [Declaration]
	filterImportList = case view importList i of
		Nothing -> id
		Just il -> filter (passImportList il . view declarationName)
	deps f p = delete (view projectName p) $ concatMap (view infoDepends) $ fileTargets p f
	inDepsOf' f p m' = any (`inPackage` m') (deps f p)

-- | Merge imported declarations
mergeImported :: [Declaration] -> [Declaration]
mergeImported =
	map merge' .
	groupBy ((==) `on` declId) .
	sortBy (comparing declId)
	where
		declId :: Declaration -> (Text, Maybe ModuleId)
		declId = view declarationName &&& view declarationDefined
		merge' :: [Declaration] -> Declaration
		merge' [] = error "mergeImported: impossible"
		merge' ds@(d:_) = set declarationImported (mconcat $ map (view declarationImported) ds) d
