{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module HsDev.Symbols.Resolve (
	ResolveM(..),ResolvedTree, ResolvedModule(..), resolvedModule, resolvedScope, resolvedExports,
	scopeModule, exportsModule, resolvedTopScope,
	resolve, resolveOne, resolveModule, exported, resolveImport,
	mergeImported
	) where

import Control.Arrow
import Control.Lens (makeLenses, view, preview, set, _Just)
import Control.Monad.Reader
import Control.Monad.State
import Data.Function (on)
import Data.List (sortBy, groupBy, delete, nubBy)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import Data.Maybe.JustIf
import Data.Ord (comparing)
import Data.String (fromString)
import Data.Text (Text)

import HsDev.Database
import HsDev.Symbols
import HsDev.Symbols.Util

-- | Resolve monad uses existing @Database@ and @ResolvedTree@ as state.
newtype ResolveM a = ResolveM { runResolveM :: ReaderT Database (State ResolvedTree) a }
	deriving (Functor, Applicative, Monad, MonadState ResolvedTree, MonadReader Database)

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
resolve db = flip evalState M.empty . flip runReaderT db . runResolveM . traverse resolveModule

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
					Just exports' -> unique $ catMaybes $ exported <$> scope' <*> exports'
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
	unique = nubBy ((==) `on` declId) . sortBy (comparing declId)
	declId :: Declaration -> (Text, Maybe ModuleId)
	declId = view declarationName &&& view declarationDefined

exported :: Declaration -> Export -> Maybe Declaration
exported decl' (ExportName q n p)
	| view declarationName decl' == n = decl' `justIf` checkImport
	| preview (declaration . related . _Just) decl' == Just n = case p of
		ExportNothing -> Nothing
		ExportAll -> Just decl'
		ExportWith ns -> decl' `justIf` (view declarationName decl' `elem` ns)
	| otherwise = Nothing
	where
		checkImport = case q of
			Nothing -> any (not . view importIsQualified) $ fromMaybe [] $ view declarationImported decl'
			Just q' -> any ((q' `elem`) . importNames) $ fromMaybe [] $ view declarationImported decl'
exported decl' (ExportModule m) = decl' `justWhen` (any (unqualBy m) . fromMaybe [] . view declarationImported) where
	unqualBy :: Text -> Import -> Bool
	unqualBy m' i = m' `elem` importNames i && not (view importIsQualified i)

-- | Bring declarations into scope
resolveImport :: Module -> Import -> ResolveM [Declaration]
resolveImport m i = liftM (map $ setImport i) resolveImport' where
	resolveImport' :: ResolveM [Declaration]
	resolveImport' = do
		ms <- case view moduleLocation m of
			FileModule file proj -> do
				db <- ask
				let
					proj' = proj >>= refineProject db
				case proj' of
					Nothing -> selectImport i [
						inFile $ importedModulePath (view moduleName m) file (view importModuleName i),
						installed]
					Just p -> selectImport i [
						inProject p,
						inDepsOf' file p]
			InstalledModule cabal _ _ -> selectImport i [inPackageDb cabal]
			ModuleSource _ -> selectImport i [installed]
		fromMaybe [] <$> traverse (liftM (filterImportList . view resolvedExports) . resolveModule) ms
	setImport :: Import -> Declaration -> Declaration
	setImport i' = set declarationImported (Just [i'])
	selectImport :: Import -> [ModuleId -> Bool] -> ResolveM (Maybe Module)
	selectImport i' fs = do
		db <- ask
		return $
			listToMaybe $
			newestPackage $
			fromMaybe [] $
			listToMaybe $ dropWhile null
				[selectModules (select' f) db | f <- fs]
		where
			select' f md  = view moduleName md == view importModuleName i' && f (view moduleId md)
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
