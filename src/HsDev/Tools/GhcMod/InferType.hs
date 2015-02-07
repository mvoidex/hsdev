module HsDev.Tools.GhcMod.InferType (
	untyped, inferType, inferTypes,
	GhcModT,
	infer
	) where

import Control.Applicative
import Control.Lens (view, preview, set, _Just)
import Control.Monad.Error
import Data.Maybe (listToMaybe)
import Data.String (fromString)
import qualified Data.Text as T (unpack)
import Data.Traversable (traverse)
import qualified Language.Haskell.GhcMod as GhcMod

import HsDev.Cabal
import HsDev.Symbols
import HsDev.Tools.GhcMod
import HsDev.Util (withCurrentDirectory)

-- | Is declaration untyped
untyped :: DeclarationInfo -> Bool
untyped (Function Nothing _ _) = True
untyped _ = False

-- | Infer type of declaration
inferType :: [String] -> Cabal -> FilePath -> Declaration -> GhcModT IO Declaration
inferType opts cabal src decl'
	| untyped (view declaration decl') = doInfer
	| otherwise = return decl'
	where
		doInfer = do
			inferred <- ((preview $ declaration . functionType . _Just) <$> byInfo) <|> (fmap fromString <$> byTypeOf)
			return $ set (declaration . functionType) inferred decl'

		byInfo = info opts cabal src (T.unpack $ view declarationName decl')
		byTypeOf = case view declarationPosition decl' of
			Nothing -> fail "No position"
			Just (Position l c) -> (fmap typedType . listToMaybe) <$> typeOf opts cabal src l c

-- | Infer types for module
inferTypes :: [String] -> Cabal -> Module -> GhcModT IO Module
inferTypes opts cabal m = case view moduleLocation m of
	FileModule src _ -> do
		inferredDecls <- traverse (\d -> inferType opts cabal src d <|> return d) $
			view moduleDeclarations m
		return $ set moduleDeclarations inferredDecls m
	_ -> throwError $ strMsg "Type infer works only for source files"

-- | Infer type in module
infer :: [String] -> Cabal -> Module -> ErrorT String IO Module
infer opts cabal m = case view moduleLocation m of
	FileModule src _ -> mapErrorT (withCurrentDirectory (sourceModuleRoot (view moduleName m) src)) $
		runGhcMod GhcMod.defaultOptions $ inferTypes opts cabal m
	_ -> throwError $ strMsg "Type infer works only for source files"
