{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module HsDev.Tools.Refact (
	Refact(..), refactMessage, refactAction,
	refact, update,

	replace, cut, paste,

	fromRegion, fromPosition
	) where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Text (Text)
import Data.Text.Region hiding (Region(..), update)
import qualified Data.Text.Region as R

import HsDev.Symbols.Location
import HsDev.Util

data Refact = Refact {
	_refactMessage :: Text,
	_refactAction :: Replace Text }
		deriving (Eq, Show)

instance ToJSON Refact where
	toJSON (Refact msg cor) = object [
		"message" .= msg,
		"action" .= cor]

instance FromJSON Refact where
	parseJSON = withObject "correction" $ \v -> Refact <$>
		v .:: "message" <*>
		v .:: "action"

makeLenses ''Refact

instance Regioned Refact where
	regions = refactAction . regions

refact :: [Refact] -> Text -> Text
refact rs = apply act where
	act = Edit (rs ^.. each . refactAction)

update :: Regioned a => [Refact] -> [a] -> [a]
update rs = map (R.update act) where
	act = Edit (rs ^.. each . refactAction)

fromRegion :: Region -> R.Region
fromRegion (Region f t) = fromPosition f `till` fromPosition t

fromPosition :: Position -> Point
fromPosition (Position l c) = pt (pred l) (pred c)
