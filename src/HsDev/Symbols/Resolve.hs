{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HsDev.Symbols.Resolve (
	ResolvedModule(..),
	ModuleTree,
	ResolveM(..),
	resolve
	) where

import Control.Applicative
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Foldable (Foldable, toList)
import Data.Traversable (Traversable, traverse)

import HsDev.Symbols
--import HsDev.Symbols.Util

-- | Declarations in scope of module
data ResolvedModule = ResolvedModule {
	resolved :: Module,
	-- | String : declaration exported from (or self), ModuleDeclaration : declaration defined
	scope :: Maybe [(String, ModuleDeclaration)] }

type ModuleTree = Map String ResolvedModule

newtype ResolveM a = ResolveM { runResolveM :: State ModuleTree a }
	deriving (Functor, Applicative, Monad, MonadState ModuleTree)

-- | Resolve modules
resolve :: (Traversable t, Foldable t) => t Module -> t ResolvedModule
resolve ms = (`evalState` tree') . runResolveM . traverse resolveModule $ ms where
	tree' = M.fromList [(moduleName m, ResolvedModule m Nothing) | m <- toList ms]

lookupTree :: String -> ResolveM Module
lookupTree mname = gets (maybe emptyModule resolved . M.lookup mname) where
	emptyModule = Module mname Nothing (ModuleSource Nothing) Nothing [] M.empty

-- | Resolve step
resolveModule :: Module -> ResolveM ResolvedModule
resolveModule m = (ResolvedModule m . Just) <$> resolveScope m

-- | Resolve step
resolveScope :: Module -> ResolveM [(String, ModuleDeclaration)]
resolveScope m = 
	gets (M.lookup (moduleName m) >=> scope) >>=
	maybe resolve' return >>=
	save
	where
		resolve' = do
			decls <- liftM concat $ mapM resolveImport (moduleImports m)
			return $ selfDecls ++ decls

		selfDecls = [(moduleName m, d) | d <- moduleModuleDeclarations m]

		save :: [(String, ModuleDeclaration)] -> ResolveM [(String, ModuleDeclaration)]
		save decls = do
			modify (M.insert (moduleName m) . ResolvedModule m . Just $ decls)
			return decls

-- | Resolve exported module declarations
resolveExports :: Module -> ResolveM [(String, ModuleDeclaration)]
resolveExports m = do
	decls <- resolveScope m
	return [(moduleName m, decl') | (mname, decl') <- decls, spec <- specs, spec `exports` (mname, decl')]
	where
		specs = fromMaybe [ExportModule $ moduleName m] (moduleExports m)

-- | Bring declarations into scope
resolveImport :: Import -> ResolveM [(String, ModuleDeclaration)]
resolveImport i = lookupTree (importModuleName i) >>= liftM (filter (imports i . snd)) . resolveExports

-- | Does import imports specified declaration
imports :: Import -> ModuleDeclaration -> Bool
imports i decl' = case importList i of
	Nothing -> True
	Just (ImportList hidden_ specs) -> (declarationName (moduleDeclaration decl') `elem` specs) /= hidden_

-- | Does export exports declaration
exports :: Export -> (String, ModuleDeclaration) -> Bool
exports (ExportName n) (_, decl') = declarationName (moduleDeclaration decl') == n
exports (ExportModule m) (mname, _) = m == mname
