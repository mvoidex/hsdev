{-# LANGUAGE OverloadedStrings #-}

module HsDev.Types (
	HsDevError(..)
	) where

import Control.Exception
import Control.DeepSeq (NFData(..))
import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import Data.Typeable
import Text.Format

import HsDev.Symbols.Location

-- | hsdev exception type
data HsDevError =
	ModuleNotSource ModuleLocation |
	BrowseNoModuleInfo String |
	FileNotFound FilePath |
	ToolNotFound String |
	ToolError String String |
	NotInspected |
	InspectError String |
	InspectCabalError FilePath String |
	IOFailed String |
	OtherError String
		deriving (Typeable)

instance NFData HsDevError where
	rnf (ModuleNotSource mloc) = rnf mloc
	rnf (BrowseNoModuleInfo m) = rnf m
	rnf (FileNotFound f) = rnf f
	rnf (ToolNotFound t) = rnf t
	rnf (ToolError t e) = rnf t `seq` rnf e
	rnf NotInspected = ()
	rnf (InspectError e) = rnf e
	rnf (InspectCabalError c e) = rnf c `seq` rnf e
	rnf (IOFailed e) = rnf e
	rnf (OtherError e) = rnf e

instance Show HsDevError where
	show (ModuleNotSource mloc) = format "module is not source: {}" ~~ show mloc
	show (BrowseNoModuleInfo m) = format "can't find module info for {}" ~~ m
	show (FileNotFound f) = format "file '{}' not found" ~~ f
	show (ToolNotFound t) = format "tool '{}' not found" ~~ t
	show (ToolError t e) = format "tool '{}' failed: {}" ~~ t ~~ e
	show NotInspected = "not inspected"
	show (InspectError e) = format "failed to inspect: {}" ~~ e
	show (InspectCabalError c e) = format "failed to inspect cabal {}: {}" ~~ c ~~ e
	show (IOFailed e) = format "io exception: {}" ~~ e
	show (OtherError e) = e

jsonErr :: String -> [Pair] -> Value
jsonErr e = object . (("error" .= e) :)

instance ToJSON HsDevError where
	toJSON (ModuleNotSource mloc) = jsonErr "module is not source" ["module" .= mloc]
	toJSON (BrowseNoModuleInfo m) = jsonErr "no module info" ["module" .= m]
	toJSON (FileNotFound f) = jsonErr "file not found" ["file" .= f]
	toJSON (ToolNotFound t) = jsonErr "tool not found" ["tool" .= t]
	toJSON (ToolError t e) = jsonErr "tool error" ["tool" .= t, "msg" .= e]
	toJSON NotInspected = jsonErr "not inspected" []
	toJSON (InspectError e) = jsonErr "inspect error" ["msg" .= e]
	toJSON (InspectCabalError c e) = jsonErr "inspect cabal error" ["cabal" .= c, "msg" .= e]
	toJSON (IOFailed e) = jsonErr "io error" ["msg" .= e]
	toJSON (OtherError e) = jsonErr "other error" ["msg" .= e]

instance FromJSON HsDevError where
	parseJSON = withObject "hsdev-error" $ \v -> do
		err <- v .: "error" :: Parser String
		case err of
			"module is not source" -> ModuleNotSource <$> v .: "module"
			"no module info" -> BrowseNoModuleInfo <$> v .: "module"
			"file not found" -> FileNotFound <$> v .: "file"
			"tool not found" -> ToolNotFound <$> v .: "tool"
			"tool error" -> ToolError <$> v .: "tool" <*> v .: "msg"
			"not inspected" -> pure NotInspected
			"inspect error" -> InspectError <$> v .: "msg"
			"inspect cabal error" -> InspectCabalError <$> v .: "cabal" <*> v .: "msg"
			"io error" -> IOFailed <$> v .: "msg"
			"other error" -> OtherError <$> v .: "msg"
			_ -> fail "invalid error"

instance Exception HsDevError
