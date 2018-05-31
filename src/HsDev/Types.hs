{-# LANGUAGE OverloadedStrings #-}

module HsDev.Types (
	HsDevError(..)
	) where

import Control.Exception
import Control.DeepSeq (NFData(..))
import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import Data.Semigroup
import Data.Typeable
import Data.Text (Text)
import Text.Format

import HsDev.Symbols.Location
import System.Directory.Paths

-- | hsdev exception type
data HsDevError =
	HsDevFailure |
	ModuleNotSource ModuleLocation |
	BrowseNoModuleInfo String |
	FileNotFound Path |
	ToolNotFound String |
	ProjectNotFound Text |
	PackageNotFound Text |
	ToolError String String |
	NotInspected ModuleLocation |
	InspectError String |
	InspectCabalError FilePath String |
	IOFailed String |
	GhcError String |
	RequestError String String |
	ResponseError String String |
	SQLiteError String |
	OtherError String |
	UnhandledError String
		deriving (Typeable)

instance NFData HsDevError where
	rnf HsDevFailure = ()
	rnf (ModuleNotSource mloc) = rnf mloc
	rnf (BrowseNoModuleInfo m) = rnf m
	rnf (FileNotFound f) = rnf f
	rnf (ToolNotFound t) = rnf t
	rnf (ProjectNotFound p) = rnf p
	rnf (PackageNotFound p) = rnf p
	rnf (ToolError t e) = rnf t `seq` rnf e
	rnf (NotInspected mloc) = rnf mloc
	rnf (InspectError e) = rnf e
	rnf (InspectCabalError c e) = rnf c `seq` rnf e
	rnf (IOFailed e) = rnf e
	rnf (GhcError e) = rnf e
	rnf (RequestError e r) = rnf e `seq` rnf r
	rnf (ResponseError e r) = rnf e `seq` rnf r
	rnf (SQLiteError e) = rnf e
	rnf (OtherError e) = rnf e
	rnf (UnhandledError e) = rnf e

instance Show HsDevError where
	show HsDevFailure = format "failure"
	show (ModuleNotSource mloc) = format "module is not source: {}" ~~ show mloc
	show (BrowseNoModuleInfo m) = format "can't find module info for {}" ~~ m
	show (FileNotFound f) = format "file '{}' not found" ~~ f
	show (ToolNotFound t) = format "tool '{}' not found" ~~ t
	show (ProjectNotFound p) = format "project '{}' not found" ~~ p
	show (PackageNotFound p) = format "package '{}' not found" ~~ p
	show (ToolError t e) = format "tool '{}' failed: {}" ~~ t ~~ e
	show (NotInspected mloc) = "module not inspected: {}" ~~ show mloc
	show (InspectError e) = format "failed to inspect: {}" ~~ e
	show (InspectCabalError c e) = format "failed to inspect cabal {}: {}" ~~ c ~~ e
	show (IOFailed e) = format "io exception: {}" ~~ e
	show (GhcError e) = format "ghc exception: {}" ~~ e
	show (RequestError e r) = format "request error: {}, request: {}" ~~ e ~~ r
	show (ResponseError e r) = format "response error: {}, response: {}" ~~ e ~~ r
	show (SQLiteError e) = format "sqlite error: {}" ~~ e
	show (OtherError e) = e
	show (UnhandledError e) = e

instance Semigroup HsDevError where
	_ <> r = r

instance Monoid HsDevError where
	mempty = HsDevFailure
	mappend l r = l <> r

instance Formattable HsDevError where

jsonErr :: String -> [Pair] -> Value
jsonErr e = object . (("error" .= e) :)

instance ToJSON HsDevError where
	toJSON HsDevFailure = jsonErr "failure" []
	toJSON (ModuleNotSource mloc) = jsonErr "module is not source" ["module" .= mloc]
	toJSON (BrowseNoModuleInfo m) = jsonErr "no module info" ["module" .= m]
	toJSON (FileNotFound f) = jsonErr "file not found" ["file" .= f]
	toJSON (ToolNotFound t) = jsonErr "tool not found" ["tool" .= t]
	toJSON (ProjectNotFound p) = jsonErr "project not found" ["project" .= p]
	toJSON (PackageNotFound p) = jsonErr "package not found" ["package" .= p]
	toJSON (ToolError t e) = jsonErr "tool error" ["tool" .= t, "msg" .= e]
	toJSON (NotInspected mloc) = jsonErr "module not inspected" ["module" .= mloc]
	toJSON (InspectError e) = jsonErr "inspect error" ["msg" .= e]
	toJSON (InspectCabalError c e) = jsonErr "inspect cabal error" ["cabal" .= c, "msg" .= e]
	toJSON (IOFailed e) = jsonErr "io error" ["msg" .= e]
	toJSON (GhcError e) = jsonErr "ghc error" ["msg" .= e]
	toJSON (RequestError e r) = jsonErr "request error" ["msg" .= e, "request" .= r]
	toJSON (ResponseError e r) = jsonErr "response error" ["msg" .= e, "response" .= r]
	toJSON (SQLiteError e) = jsonErr "sqlite error" ["msg" .= e]
	toJSON (OtherError e) = jsonErr "other error" ["msg" .= e]
	toJSON (UnhandledError e) = jsonErr "unhandled error" ["msg" .= e]

instance FromJSON HsDevError where
	parseJSON = withObject "hsdev-error" $ \v -> do
		err <- v .: "error" :: Parser String
		case err of
			"failure" -> pure HsDevFailure
			"module is not source" -> ModuleNotSource <$> v .: "module"
			"no module info" -> BrowseNoModuleInfo <$> v .: "module"
			"file not found" -> FileNotFound <$> v .: "file"
			"tool not found" -> ToolNotFound <$> v .: "tool"
			"project not found" -> ProjectNotFound <$> v .: "project"
			"package not found" -> PackageNotFound <$> v .: "package"
			"tool error" -> ToolError <$> v .: "tool" <*> v .: "msg"
			"module not inspected" -> NotInspected <$> v .: "module"
			"inspect error" -> InspectError <$> v .: "msg"
			"inspect cabal error" -> InspectCabalError <$> v .: "cabal" <*> v .: "msg"
			"io error" -> IOFailed <$> v .: "msg"
			"ghc error" -> GhcError <$> v .: "msg"
			"request error" -> RequestError <$> v .: "msg" <*> v .: "request"
			"response error" -> ResponseError <$> v .: "msg" <*> v .: "response"
			"sqlite error" -> SQLiteError <$> v .: "msg"
			"other error" -> OtherError <$> v .: "msg"
			"unhandled error" -> UnhandledError <$> v .: "msg"
			_ -> fail "invalid error"

instance Exception HsDevError
