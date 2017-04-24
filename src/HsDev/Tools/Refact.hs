{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module HsDev.Tools.Refact (
	Refact(..), refactMessage, refactAction,
	refact, update
	) where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Text.Region hiding (Region(..), update)
import qualified Data.Text.Region as R

import HsDev.Util

data Refact = Refact {
	_refactMessage :: String,
	_refactAction :: Replace String }
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

refact :: [Refact] -> String -> String
refact rs = apply act where
	act = Edit (rs ^.. each . refactAction)

update :: Regioned a => [Refact] -> [a] -> [a]
update rs = map (R.update act) where
	act = Edit (rs ^.. each . refactAction)
