module HsDev.Tools.GhcMod.InferType (
	untyped, inferType, inferTypes,
	GhcModT
	) where

import Control.Monad.Error
import Data.Traversable (traverse)

import HsDev.Cabal
import HsDev.Symbols
import HsDev.Tools.GhcMod

-- | Is declaration untyped
untyped :: DeclarationInfo -> Bool
untyped (Function Nothing _) = True
untyped _ = False

-- | Infer type of declaration
inferType :: [String] -> Cabal -> FilePath -> Declaration -> GhcModT IO Declaration
inferType opts cabal src decl'
	| untyped (declaration decl') = infer
	| otherwise = return decl'
	where
		infer = do
			inferred <- liftM declaration $ info opts cabal src (declarationName decl')
			return decl' {
				declaration = setType (declaration decl') (getType inferred) }

		setType :: DeclarationInfo -> Maybe String -> DeclarationInfo
		setType (Function _ ds) newType = Function newType ds
		setType dinfo _ = dinfo

		getType :: DeclarationInfo -> Maybe String
		getType (Function fType _) = fType
		getType _ = Nothing

-- | Infer types for module
inferTypes :: [String] -> Cabal -> Module -> GhcModT IO Module
inferTypes opts cabal m = case moduleLocation m of
	FileModule src p -> do
		inferredDecls <- traverse (inferType opts cabal src) $ moduleDeclarations m
		return m { moduleDeclarations = inferredDecls }
	_ -> throwError $ strMsg "Type infer works only for source files"
