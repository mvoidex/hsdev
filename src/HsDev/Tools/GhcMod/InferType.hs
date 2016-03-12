module HsDev.Tools.GhcMod.InferType (
	untyped, inferType, inferTypes,
	GhcModT,
	infer
	) where

import Control.Applicative
import Control.Lens (view, preview, set, _Just)
import Control.Monad.Except
import Data.Maybe (listToMaybe)
import Data.String (fromString)
import qualified Data.Text as T (unpack)
import qualified Language.Haskell.GhcMod as GhcMod

import HsDev.PackageDb
import HsDev.Symbols
import HsDev.Tools.GhcMod
import HsDev.Util (withCurrentDirectory)

-- | Is declaration untyped
untyped :: DeclarationInfo -> Bool
untyped (Function Nothing _ _) = True
untyped _ = False

-- | Infer type of declaration
inferType :: [String] -> PackageDbStack -> FilePath -> Declaration -> GhcModT IO Declaration
inferType opts pdbs src decl'
	| untyped (view declaration decl') = doInfer
	| otherwise = return decl'
	where
		doInfer = do
			inferred <- ((preview $ declaration . functionType . _Just) <$> byInfo) <|> (fmap fromString <$> byTypeOf)
			return $ set (declaration . functionType) inferred decl'

		byInfo = info opts pdbs src (T.unpack $ view declarationName decl')
		byTypeOf = case view declarationPosition decl' of
			Nothing -> fail "No position"
			Just (Position l c) -> (fmap typedType . listToMaybe) <$> typeOf opts pdbs src l c

-- | Infer types for module
inferTypes :: [String] -> PackageDbStack -> Module -> GhcModT IO Module
inferTypes opts pdbs m = case view moduleLocation m of
	FileModule src _ -> do
		inferredDecls <- traverse (\d -> inferType opts pdbs src d <|> return d) $
			view moduleDeclarations m
		return $ set moduleDeclarations inferredDecls m
	_ -> fail "Type infer works only for source files"

-- | Infer type in module
infer :: [String] -> PackageDbStack -> Module -> ExceptT String IO Module
infer opts pdbs m = case view moduleLocation m of
	FileModule src _ -> mapExceptT (withCurrentDirectory (sourceModuleRoot (view moduleName m) src)) $
		runGhcMod GhcMod.defaultOptions $ inferTypes opts pdbs m
	_ -> throwError "Type infer works only for source files"
