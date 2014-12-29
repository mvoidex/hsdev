module HsDev.Tools.GhcMod.InferType (
	untyped, inferType, inferTypes,
	GhcModT
	) where

import Control.Applicative
import Control.Monad.Error
import Data.Maybe (listToMaybe)
import Data.String (fromString)
import qualified Data.Text as T (unpack)
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
			inferred <- ((getType . declaration) <$> byInfo) <|> byTypeOf
			return decl' {
				declaration = setType (declaration decl') inferred }

		byInfo = info opts cabal src (T.unpack $ declarationName decl')
		byTypeOf = case declarationPosition decl' of
			Nothing -> fail "No position"
			Just (Position l c) -> (fmap typedType . listToMaybe) <$> typeOf opts cabal src l c

		setType :: DeclarationInfo -> Maybe String -> DeclarationInfo
		setType (Function _ ds) newType = Function (fmap fromString newType) ds
		setType dinfo _ = dinfo

		getType :: DeclarationInfo -> Maybe String
		getType (Function fType _) = fmap T.unpack fType
		getType _ = Nothing

-- | Infer types for module
inferTypes :: [String] -> Cabal -> Module -> GhcModT IO Module
inferTypes opts cabal m = case moduleLocation m of
	FileModule src _ -> do
		inferredDecls <- traverse (\d -> inferType opts cabal src d <|> return d) $
			moduleDeclarations m
		return m { moduleDeclarations = inferredDecls }
	_ -> throwError $ strMsg "Type infer works only for source files"
