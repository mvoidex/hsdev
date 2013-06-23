{-# LANGUAGE OverloadedStrings #-}

module HsDev.Symbols.JSON (
	-- * Encode
	encodeDeclaration,
	encodeModuleHead,
	encodeModule,

	-- * Decode
	decodeDeclaration,
	decodeModuleHead,
	decodeModule
	) where

import Control.Arrow
import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.Types
import Data.Map (Map)
import qualified Data.Map as M
import Data.Traversable (traverse)

import HsDev.Symbols
import HsDev.Project
import HsDev.Cache ()

encodeDeclaration :: Symbol Declaration -> Value
encodeDeclaration s = object $ sym ++ decl (symbol s) where
	sym = [
		"name" .= symbolName s,
		"module" .= fmap encodeModuleHead (symbolModule s),
		"docs" .= symbolDocs s,
		"location" .= fmap encodeLocation (symbolLocation s)]
	decl (Function t) = ["what" .= str "function", "type" .= t]
	decl (Type ti) = ("what" .= str "type") : info ti
	decl (NewType ti) = ("what" .= str "newtype") : info ti
	decl (Data ti) = ("what" .= str "data") : info ti
	decl (Class ti) = ("what" .= str "class") : info ti
	info ti = ["ctx" .= typeInfoContext ti, "args" .= typeInfoArgs ti, "definition" .= typeInfoDefinition ti]

encodeModuleHead :: Symbol Module -> Value
encodeModuleHead m = object $ sym ++ moduleCts (symbol m) where
	sym = [
		"name" .= symbolName m,
		"location" .= fmap encodeLocation (symbolLocation m)]
	moduleCts m' = [
		"cabal" .= moduleCabal m']

encodeModule :: Symbol Module -> Value
encodeModule m = object $ sym ++ moduleCts (symbol m) where
	sym = [
		"name" .= symbolName m,
		"location" .= fmap encodeLocation (symbolLocation m)]
	moduleCts m' = [
		"exports" .= moduleExports m',
		"imports" .= map encodeImport (M.elems $ moduleImports m'),
		"decls" .= map encodeDeclaration (M.elems $ moduleDeclarations m'),
		"cabal" .= moduleCabal m']

encodeLocation :: Location -> Value
encodeLocation loc = object [
	"file" .= locationFile loc,
	"line" .= locationLine loc,
	"column" .= locationColumn loc,
	"project" .= fmap projectCabal (locationProject loc),
	"mtime" .= locationTimeStamp loc]

encodeImport :: Import -> Value
encodeImport i = object [
	"module" .= importModuleName i,
	"qualified" .= importIsQualified i,
	"as" .= importAs i,
	"location" .= fmap encodeLocation (importLocation i)]

decodeDeclaration :: Value -> Parser (Symbol Declaration)
decodeDeclaration = withObject "declaration" $ \v -> Symbol <$>
	(v .: "name") <*>
	((v .: "module") >>= traverse decodeModuleHead) <*>
	(v .: "docs") <*>
	((v .: "location") >>= traverse decodeLocation) <*>
	pure [] <*>
	decodeDecl v
	where
		decodeDecl :: Object -> Parser Declaration
		decodeDecl v = do
			what <- v .: "what"
			case (what :: String) of
				"function" -> Function <$> (v .: "type")
				"type" -> Type <$> info
				"newtype" -> NewType <$> info
				"data" -> Data <$> info
				"class" -> Class <$> info
				_ -> mzero
			where
				info :: Parser TypeInfo
				info = TypeInfo <$>
					(v .: "ctx") <*>
					(v .: "args") <*>
					(v .: "definition")

decodeModuleHead :: Value -> Parser (Symbol Module)
decodeModuleHead = withObject "module" $ \v -> Symbol <$>
	(v .: "name") <*>
	pure Nothing <*>
	pure Nothing <*>
	((v .: "location") >>= traverse decodeLocation) <*>
	pure [] <*>
	(Module [] M.empty M.empty <$> (v .: "cabal"))

decodeModule :: Value -> Parser (Symbol Module)
decodeModule = withObject "module" $ \v -> Symbol <$>
	(v .: "name") <*>
	pure Nothing <*>
	pure Nothing <*>
	((v .: "location") >>= traverse decodeLocation) <*>
	pure [] <*>
	(Module <$>
		(v .: "exports") <*>
		((v .: "imports") >>= fmap mapImports . mapM decodeImport) <*>
		((v .: "decls") >>= fmap mapDecls . mapM decodeDeclaration) <*>
		(v .: "cabal"))
	where
		mapImports :: [Import] -> Map String Import
		mapImports = M.fromList . map (importModuleName &&& id)

		mapDecls :: [Symbol Declaration] -> Map String (Symbol Declaration)
		mapDecls = M.fromList . map (symbolName &&& id)

decodeLocation :: Value -> Parser Location
decodeLocation = withObject "location" $ \v -> Location <$>
	(v .: "file") <*>
	(v .: "line") <*>
	(v .: "column") <*>
	fmap (fmap project) (v .: "project") <*>
	(v .: "mtime")

decodeImport :: Value -> Parser Import
decodeImport = withObject "import" $ \v -> Import <$>
	(v .: "module") <*>
	(v .: "qualified") <*>
	(v .: "as") <*>
	((v .: "location") >>= traverse decodeLocation)

str :: String -> String
str = id
