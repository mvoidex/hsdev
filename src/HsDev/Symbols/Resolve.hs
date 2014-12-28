{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HsDev.Symbols.Resolve (
	ModuleTree,
	ResolveM(..),
	resolve,
	resolveModule, resolveScope, resolveExports, resolveImport
	) where

import Control.Applicative
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Foldable (Foldable, toList)
import Data.Traversable (Traversable, traverse)
import Data.Text (Text)

import HsDev.Symbols

type ModuleTree = Map ModuleId Module

newtype ResolveM a = ResolveM { runResolveM :: State ModuleTree a }
	deriving (Functor, Applicative, Monad, MonadState ModuleTree)

-- | Resolve modules
resolve :: (Traversable t, Foldable t) => ModuleTree -> t Module -> t Module
resolve db = (`evalState` db) . runResolveM . traverse resolveModule

lookupTree :: ModuleId -> ResolveM Module
lookupTree m = undefined -- TODO: Implement

-- | Resolve step
resolveModule :: Module -> ResolveM Module
resolveModule m = undefined -- TODO: Implement

-- | Resolve step
resolveScope :: Module -> ResolveM [Declaration]
resolveScope m = undefined -- TODO: Implement
	-- gets (M.lookup (moduleId m) >=> scope) >>=
	-- maybe resolve' return >>=
	-- save
	-- where
	-- 	resolve' = do
	-- 		decls <- liftM concat $ mapM resolveImport (moduleImports m)
	-- 		return $ selfDecls ++ decls

	-- 	selfDecls = [(moduleName m, d) | d <- moduleModuleDeclarations m]

	-- 	save :: [(Text, ModuleDeclaration)] -> ResolveM [(Text, ModuleDeclaration)]
	-- 	save decls = do
	-- 		modify (M.insert (moduleName m) . ResolvedModule m . Just $ decls)
	-- 		return decls

-- | Resolve exported module declarations
resolveExports :: Module -> ResolveM [Declaration]
resolveExports m = do
	decls <-  (M.elems . moduleDeclarations) <$> resolveModule m
	return [decl' | decl' <- decls, spec <- specs, spec `exports` decl']
	where
		specs = fromMaybe [ExportModule $ moduleName m] (moduleExports m)

-- | Bring declarations into scope
resolveImport :: Import -> ResolveM [Declaration]
resolveImport i = undefined -- lookupTree (importModuleName i) >>= liftM (filter (imports i . snd)) . resolveExports

-- | Does import imports specified declaration
imports :: Import -> ModuleDeclaration -> Bool
imports i decl' = case importList i of
	Nothing -> True
	Just (ImportList hidden_ specs) -> (declarationName (moduleDeclaration decl') `elem` specs) /= hidden_

-- | Does export exports declaration
exports :: Export -> Declaration -> Bool
exports (ExportName n) decl' = declarationName decl' == n
exports (ExportModule m) _ = undefined -- m == mname -- TODO: We need `imported from` for declaration
