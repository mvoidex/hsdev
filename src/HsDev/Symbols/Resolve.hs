{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HsDev.Symbols.Resolve (
	ResolveM(..), ResolvedTree, ResolvedModule(..), scopeModule, exportsModule, resolvedTopScope,
	resolve, resolveOne, resolveModule, exported, resolveImport,
	mergeImported
	) where

import Control.Applicative
import Control.Arrow
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable (Foldable)
import Data.Function (on)
import Data.List (sortBy, groupBy, find)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, maybeToList)
import Data.Monoid (mconcat, mappend)
import Data.Ord (comparing)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (Traversable, traverse)
import System.FilePath

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
	resolvedModule :: Module,
	resolvedScope :: [Declaration],
	resolvedExports :: [Declaration] }

-- | Make @Module@ with scope declarations
scopeModule :: ResolvedModule -> Module
scopeModule r = (resolvedModule r) { moduleDeclarations = declarationMap (resolvedScope r) }

-- | Make @Module@ with exported only declarations
exportsModule :: ResolvedModule -> Module
exportsModule r = (resolvedModule r) { moduleDeclarations = declarationMap (resolvedExports r) }

-- | Get top-level scope
resolvedTopScope :: ResolvedModule -> [Declaration]
resolvedTopScope = filter isTop . resolvedScope where
	isTop :: Declaration -> Bool
	isTop = any (not . importIsQualified) . fromMaybe [] . declarationImported

-- | Resolve modules, function is not IO, so all file names must be canonicalized
resolve :: (Traversable t, Foldable t) => Database -> t Module -> t ResolvedModule
resolve db = flip evalState M.empty . flip runReaderT db . runResolveM . traverse resolveModule

-- | Resolve one module
resolveOne :: Database -> Module -> ResolvedModule
resolveOne db = fromMaybe (error "Resolve: impossible happened") . resolve db . Just

-- | Resolve module
resolveModule :: Module -> ResolveM ResolvedModule
resolveModule m = gets (M.lookup $ moduleId m) >>= maybe resolveModule' return where
	resolveModule' = save $ case moduleLocation m of
		CabalModule {} -> return ResolvedModule {
			resolvedModule = m,
			resolvedScope = M.elems $ moduleDeclarations m,
			resolvedExports = M.elems $ moduleDeclarations m }
		_ -> do
			scope' <-
				liftM ((thisDecls ++) . mergeImported . concat) .
				mapM (resolveImport m) .
				(import_ (fromString "Prelude") :) .
				moduleImports $ m
			let
				exports' =
					concatMap (exported scope') .
					fromMaybe [] .
					moduleExports $ m
			return $ ResolvedModule m scope' exports'
	thisDecls :: [Declaration]
	thisDecls = map selfImport $ M.elems $ moduleDeclarations m
	selfImport :: Declaration -> Declaration
	selfImport d = d { declarationImported = Just [import_ $ moduleName m] }
	save :: ResolveM ResolvedModule -> ResolveM ResolvedModule
	save act = do
		rm <- act
		modify $ M.insert (moduleId (resolvedModule rm)) rm
		return rm

-- | Select declarations exported with @Export@
exported :: [Declaration] -> Export -> [Declaration]
exported ds (ExportName q n) = maybeToList $ find isExported ds where
	isExported :: Declaration -> Bool
	isExported decl' = declarationName decl' == n && case q of
		Nothing -> any (not . importIsQualified) $ fromMaybe [] $ declarationImported decl'
		Just q' -> any ((== q') . importName) $ fromMaybe [] $ declarationImported decl'
exported ds (ExportModule m) =
	filter (any (unqualBy m) . fromMaybe [] . declarationImported) ds
	where
		unqualBy :: Text -> Import -> Bool
		unqualBy m' i = importName i == m' && not (importIsQualified i)

-- | Bring declarations into scope
resolveImport :: Module -> Import -> ResolveM [Declaration]
resolveImport m i = liftM (map $ setImport i) resolveImport' where
	resolveImport' :: ResolveM [Declaration]
	resolveImport' = do
		ms <- case moduleLocation m of
			FileModule file proj -> do
				db <- ask
				let
					proj' = proj >>= refineProject db
				case proj' of
					Nothing -> selectImport i [
						inFile $ importedModuleFilePath m file i,
						byCabal]
					Just p -> selectImport i [
						inProject p,
						inDepsOf' file p]
			CabalModule cabal _ _ -> selectImport i [inCabal cabal]
			ModuleSource _ -> selectImport i [byCabal]
		liftM (concatMap resolvedExports) $ mapM resolveModule ms
	setImport :: Import -> Declaration -> Declaration
	setImport i' d' = d' { declarationImported = Just [i'] `mappend` declarationImported d' }
	selectImport :: Import -> [ModuleId -> Bool] -> ResolveM [Module]
	selectImport i' fs = liftM (selectModules select') ask where
		select' md = moduleName md == importModuleName i' && any ($ moduleId md) (byImport i' : fs)
	byImport :: Import -> ModuleId -> Bool
	byImport i' m' = importModuleName i' == moduleIdName m'
	importedModuleFilePath :: Module -> FilePath -> Import -> FilePath
	importedModuleFilePath m' f' i' =
		(`addExtension` "hs") . joinPath .
		(++ ipath) . reverse . drop (length mpath) .
		reverse $ fpath
		where
			mpath = map T.unpack $ T.split (== '.') $ moduleName m'
			ipath = map T.unpack $ T.split (== '.') $ importModuleName i'
			fpath = splitDirectories $ dropExtension f'
	deps f p = maybe [] infoDepends $ fileTarget p f
	inDepsOf' f p m' = any (`inPackage` m') (deps f p)

-- | Merge imported declarations
mergeImported :: [Declaration] -> [Declaration]
mergeImported =
	map merge' .
	groupBy ((==) `on` declId) .
	sortBy (comparing declId)
	where
		declId :: Declaration -> (Text, Maybe ModuleId)
		declId = declarationName &&& declarationDefined
		merge' :: [Declaration] -> Declaration
		merge' [] = error "mergeImported: impossible"
		merge' ds@(d:_) = d { declarationImported = mconcat $ map declarationImported ds }
