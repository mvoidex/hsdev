module HsDev.Tools.GhcMod.InferType (
	untyped, inferType, inferTypes
	) where

import Control.Monad.Error
import Data.Traversable (traverse)

import HsDev.Symbols
import HsDev.Tools.GhcMod

-- | Is declaration untyped
untyped :: DeclarationInfo -> Bool
untyped (Function Nothing) = True
untyped _ = False

-- | Infer type of declaration
inferType :: [String] -> FilePath -> String -> Declaration -> ErrorT String IO Declaration
inferType opts src mname decl
	| untyped (declaration decl) = infer
	| otherwise = return decl
	where
		infer = do
			inferred <- liftM declaration $ info opts src mname (declarationName decl) Cabal
			return decl {
				declaration = setType (declaration decl) (getType inferred) }

		setType :: DeclarationInfo -> Maybe String -> DeclarationInfo
		setType (Function _) newType = Function newType
		setType info _ = info

		getType :: DeclarationInfo -> Maybe String
		getType (Function fType) = fType
		getType _ = Nothing

-- | Infer types for module
inferTypes :: [String] -> Module -> ErrorT String IO Module
inferTypes opts m = case moduleLocation m of
	FileModule src _ -> do
		inferredDecls <- traverse (inferType opts src (moduleName m)) $ moduleDeclarations m
		return m { moduleDeclarations = inferredDecls }
	_ -> throwError "Type infer  works only for source files"
