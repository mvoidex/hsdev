{-# LANGUAGE OverloadedStrings #-}

module HsDev.Symbols.JSON (
	encodeDeclaration,
	encodeModuleHead,
	encodeModule
	) where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map as M

import HsDev.Symbols
import HsDev.Project

encodeDeclaration :: Symbol Declaration -> Value
encodeDeclaration s = object $ sym ++ decl (symbol s) where
	sym = [
		"name" .= symbolName s,
		"module" .= fmap encodeModuleHead (symbolModule s),
		"docs" .= symbolDocs s,
		"location" .= fmap encodeLocation (symbolLocation s)]
	decl (Function t) = ["what" .= str "function", "type" .= t]
	decl (Type ti) = ["what" .= str "type"] ++ info ti
	decl (NewType ti) = ["what" .= str "newtype"] ++ info ti
	decl (Data ti) = ["what" .= str "data"] ++ info ti
	decl (Class ti) = ["what" .= str "class"] ++ info ti
	info ti = ["ctx" .= typeInfoContext ti, "args" .= typeInfoArgs ti, "definition" .= typeInfoDefinition ti]

encodeModuleHead :: Symbol Module -> Value
encodeModuleHead m = object $ sym ++ module' (symbol m) where
	sym = [
		"name" .= symbolName m,
		"location" .= fmap encodeLocation (symbolLocation m)]
	module' m' = [
		"cabal" .= fmap show (moduleCabal m')]

encodeModule :: Symbol Module -> Value
encodeModule m = object $ sym ++ module' (symbol m) where
	sym = [
		"name" .= symbolName m,
		"location" .= fmap encodeLocation (symbolLocation m)]
	module' m' = [
		"exports" .= moduleExports m',
		"imports" .= (map encodeImport $ M.elems $ moduleImports m'),
		"decls" .= (map encodeDeclaration $ M.elems $ moduleDeclarations m'),
		"cabal" .= fmap show (moduleCabal m')]

encodeLocation :: Location -> Value
encodeLocation loc = object [
	"file" .= locationFile loc,
	"line" .= locationLine loc,
	"column" .= locationColumn loc,
	"project" .= fmap projectCabal (locationProject loc)]

encodeImport :: Import -> Value
encodeImport i = object [
	"module" .= importModuleName i,
	"qualified" .= importIsQualified i,
	"as" .= importAs i]

str :: String -> String
str = id
