{-# LANGUAGE OverloadedStrings #-}

module HsDev.Symbols.JSON (
	-- * Encode
	encodeModuleDeclaration,
	encodeModuleHead,
	encodeModule,
	encodeLocation,

	-- * Decode
	decodeModuleDeclaration,
	decodeModuleHead,
	decodeModule,
	decodeLocation
	) where

import Control.Arrow
import Control.Applicative
import Control.Monad (join)
import Data.Aeson
import Data.Aeson.Types
import Data.Map (Map)
import qualified Data.Map as M
import Data.Traversable (traverse)
import Text.Read (readMaybe)

import HsDev.Symbols
import HsDev.Project
import HsDev.Cache ()

encodeModuleDeclaration :: ModuleDeclaration -> Value
encodeModuleDeclaration m = object $ sym ++ decl (declaration . moduleDeclaration $ m) where
	sym = [
		"name" .= symbolName m,
		"module" .= encodeModuleHead (declarationModuleId m),
		"docs" .= symbolDocs m,
		"location" .= fmap encodePosition (declarationPosition $ moduleDeclaration m)]
	decl (Function t) = ["what" .= str "function", "type" .= t]
	decl (Type ti) = ("what" .= str "type") : info ti
	decl (NewType ti) = ("what" .= str "newtype") : info ti
	decl (Data ti) = ("what" .= str "data") : info ti
	decl (Class ti) = ("what" .= str "class") : info ti
	info ti = ["ctx" .= typeInfoContext ti, "args" .= typeInfoArgs ti, "def" .= typeInfoDefinition ti]

encodeModuleHead :: ModuleId -> Value
encodeModuleHead m = object $ sym where
	sym = [
		"name" .= moduleIdName m,
		"location" .= encodeModuleLocation (moduleIdLocation m)]

encodeModule :: Module -> Value
encodeModule m = object $ sym ++ moduleCts m where
	sym = [
		"name" .= symbolName m,
		"location" .= encodeModuleLocation (moduleLocation m)]
	moduleCts m' = [
		"exports" .= moduleExports m',
		"imports" .= map encodeImport (M.elems $ moduleImports m'),
		"decls" .= map encodeModuleDeclaration (moduleModuleDeclarations m')]

encodeModuleLocation :: ModuleLocation -> Value
encodeModuleLocation (FileModule f p) = object ["file" .= f, "project" .= fmap projectCabal p]
encodeModuleLocation (CabalModule c p n) = object ["cabal" .= c, "package" .= fmap show p, "name" .= n]
encodeModuleLocation (MemoryModule m) = object ["mem" .= m]

encodePosition :: Position -> Value
encodePosition (Position l c) = object [
	"line" .= l,
	"column" .= c]

encodeLocation :: Location -> Value
encodeLocation (Location l p) = object [
	"loc" .= encodeModuleLocation l,
	"pos" .= fmap encodePosition p]

encodeImport :: Import -> Value
encodeImport i = object [
	"module" .= importModuleName i,
	"qualified" .= importIsQualified i,
	"as" .= importAs i,
	"location" .= fmap encodePosition (importPosition i)]

decodeModuleDeclaration :: Value -> Parser ModuleDeclaration
decodeModuleDeclaration = withObject "declaration" $ \v -> ModuleDeclaration <$>
	((v .: "module") >>= decodeModuleHead) <*>
	(Declaration <$>
		v .: "name" <*>
		v .: "docs" <*>
		((v .: "location") >>= traverse decodePosition) <*>
		decodeDecl v)
	where
		decodeDecl :: Object -> Parser DeclarationInfo
		decodeDecl v = do
			what <- v .: "what"
			case (what :: String) of
				"function" -> Function <$> v .: "type"
				_ -> declarationTypeCtor what <$> info
			where
				info :: Parser TypeInfo
				info = TypeInfo <$> v .: "ctx" <*> v .: "args" <*> v .: "def"

decodeModuleHead :: Value -> Parser ModuleId
decodeModuleHead = withObject "module id" $ \v -> ModuleId <$>
	(v .: "name") <*>
	((v .: "location") >>= decodeModuleLocation)

decodeModule :: Value -> Parser Module
decodeModule = withObject "module" $ \v -> Module <$>
	(v .: "name") <*>
	pure Nothing <*>
	((v .: "location") >>= decodeModuleLocation) <*>
	(v .: "exports") <*>
	((v .: "imports") >>= fmap mapImports . mapM decodeImport) <*>
	((v .: "decls") >>= fmap mapDecls . mapM decodeModuleDeclaration)
	where
		mapImports :: [Import] -> Map String Import
		mapImports = M.fromList . map (importModuleName &&& id)

		mapDecls :: [ModuleDeclaration] -> Map String Declaration
		mapDecls = M.fromList . map ((declarationName &&& id) . moduleDeclaration)

decodeModuleLocation :: Value -> Parser ModuleLocation
decodeModuleLocation = withObject "module location" $ \v ->
	(FileModule <$> v .: "file" <*> (fmap project <$> v .: "project")) <|>
	(CabalModule <$> v .: "cabal" <*> ((join . readMaybe) <$> v .: "package") <*> v .: "name") <|>
	(MemoryModule <$> v .: "mem")

decodePosition :: Value -> Parser Position
decodePosition = withObject "position" $ \v -> Position <$> v .: "line" <*> v .: "column"

decodeLocation :: Value -> Parser Location
decodeLocation = withObject "location" $ \v -> Location <$> v .: "loc" <*> v .: "pos"

decodeImport :: Value -> Parser Import
decodeImport = withObject "import" $ \v -> Import <$>
	v .: "module" <*>
	v .: "qualified" <*>
	v .: "as" <*>
	((v .: "location") >>= traverse decodePosition)

str :: String -> String
str = id
