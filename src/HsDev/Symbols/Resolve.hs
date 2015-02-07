{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module HsDev.Symbols.Resolve (
	ResolveM(..),ResolvedTree, ResolvedModule(..), resolvedModule, resolvedScope, resolvedExports,
	scopeModule, exportsModule, resolvedTopScope,
	resolve, resolveOne, resolveModule, exported, resolveImport,
	mergeImported
	) where

import Control.Applicative
import Control.Arrow
import Control.Lens (makeLenses, view, set, over)
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable (Foldable)
import Data.Function (on)
import Data.List (sortBy, groupBy, find)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, maybeToList, listToMaybe)
import Data.Monoid (mconcat, mappend)
import Data.Ord (comparing)
import Data.String (fromString)
import Data.Text (Text)
import Data.Traversable (Traversable, traverse)

import HsDev.Database
import HsDev.Project
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
		CabalModule {} -> return ResolvedModule {
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
				exports' =
					concatMap (exported scope') .
					fromMaybe [] .
					view moduleExports $ m
			return $ ResolvedModule m (sortDeclarations scope') (sortDeclarations exports')
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

-- | Select declarations exported with @Export@
exported :: [Declaration] -> Export -> [Declaration]
exported ds (ExportName q n _) = maybeToList $ find isExported ds where
	isExported :: Declaration -> Bool
	isExported decl' = view declarationName decl' == n && case q of
		Nothing -> any (not . view importIsQualified) $ fromMaybe [] $ view declarationImported decl'
		Just q' -> any ((== q') . importName) $ fromMaybe [] $ view declarationImported decl'
exported ds (ExportModule m) =
	filter (any (unqualBy m) . fromMaybe [] . view declarationImported) ds
	where
		unqualBy :: Text -> Import -> Bool
		unqualBy m' i = importName i == m' && not (view importIsQualified i)

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
						byCabal]
					Just p -> selectImport i [
						inProject p,
						inDepsOf' file p]
			CabalModule cabal _ _ -> selectImport i [inCabal cabal]
			ModuleSource _ -> selectImport i [byCabal]
		liftM (filterImportList . concatMap (view resolvedExports)) $ mapM resolveModule ms
	setImport :: Import -> Declaration -> Declaration
	setImport i' = over declarationImported (Just [i'] `mappend`)
	selectImport :: Import -> [ModuleId -> Bool] -> ResolveM [Module]
	selectImport i' fs = do
		db <- ask
		return $
			fromMaybe [] $
			listToMaybe $ dropWhile null $
			[selectModules (select' f) db | f <- byImport i' : fs]
		where
			select' f md  = view moduleName md == view importModuleName i' && f (view moduleId md)
	filterImportList :: [Declaration] -> [Declaration]
	filterImportList = case view importList i of
		Nothing -> id
		Just il -> filter (passImportList il . view declarationName)
	byImport :: Import -> ModuleId -> Bool
	byImport i' m' = view importModuleName i' == view moduleIdName m'
	deps f p = maybe [] (view infoDepends) $ fileTarget p f
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
