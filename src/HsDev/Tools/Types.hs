{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module HsDev.Tools.Types (
	Severity(..),
	Note(..), noteSource, noteRegion, noteLevel, note,
	OutputMessage(..), message, messageSuggestion, outputMessage
	) where

import Control.DeepSeq (NFData(..))
import Control.Lens (makeLenses)
import Control.Monad
import Data.Aeson hiding (Error)
import Data.Text (Text)

import System.Directory.Paths
import HsDev.Symbols.Location
import HsDev.Util ((.::), (.::?))

-- | Note severity
data Severity = Error | Warning | Hint deriving (Enum, Bounded, Eq, Ord, Read, Show)

instance NFData Severity where
	rnf Error = ()
	rnf Warning = ()
	rnf Hint = ()

instance ToJSON Severity where
	toJSON Error = toJSON ("error" :: String)
	toJSON Warning = toJSON ("warning" :: String)
	toJSON Hint = toJSON ("hint" :: String)

instance FromJSON Severity where
	parseJSON v = do
		s <- parseJSON v
		msum [
			guard (s == ("error" :: String)) >> return Error,
			guard (s == ("warning" :: String)) >> return Warning,
			guard (s == ("hint" :: String)) >> return Hint,
			fail $ "Unknown severity: " ++ s]

-- | Note over some region
data Note a = Note {
	_noteSource :: ModuleLocation,
	_noteRegion :: Region,
	_noteLevel :: Maybe Severity,
	_note :: a }
		deriving (Eq, Show)

makeLenses ''Note

instance Functor Note where
	fmap f (Note s r l n) = Note s r l (f n)

instance NFData a => NFData (Note a) where
	rnf (Note s r l n) = rnf s `seq` rnf r `seq` rnf l `seq` rnf n

instance ToJSON a => ToJSON (Note a) where
	toJSON (Note s r l n) = object [
		"source" .= s,
		"region" .= r,
		"level" .= l,
		"note" .= n]

instance FromJSON a => FromJSON (Note a) where
	parseJSON = withObject "note" $ \v -> Note <$>
		v .:: "source" <*>
		v .:: "region" <*>
		v .::? "level" <*>
		v .:: "note"

instance RecalcTabs (Note a) where
	recalcTabs cts n' (Note s r l n) = Note s (recalcTabs cts n' r) l n
	calcTabs cts n' (Note s r l n) = Note s (calcTabs cts n' r) l n

instance Paths (Note a) where
	paths f (Note s r l n) = Note <$> paths f s <*> pure r <*> pure l <*> pure n

-- | Output message from some tool (ghc, ghc-mod, hlint) with optional suggestion
data OutputMessage = OutputMessage {
	_message :: Text,
	_messageSuggestion :: Maybe Text }
		deriving (Eq, Ord, Read, Show)

instance NFData OutputMessage where
	rnf (OutputMessage m s) = rnf m `seq` rnf s

instance ToJSON OutputMessage where
	toJSON (OutputMessage m s) = object [
		"message" .= m,
		"suggestion" .= s]

instance FromJSON OutputMessage where
	parseJSON = withObject "output-message" $ \v -> OutputMessage <$>
		v .:: "message" <*>
		v .:: "suggestion"

outputMessage :: Text -> OutputMessage
outputMessage msg = OutputMessage msg Nothing

makeLenses ''OutputMessage
