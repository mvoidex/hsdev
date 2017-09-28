{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Tools.AutoFix (
	Correction(..), correctionMessage, corrector,
	corrections,
	autoFix,
	CorrectorMatch,
	correctors,
	match,
	findCorrector,

	module Data.Text.Region,
	module HsDev.Tools.Types
	) where

import Control.Applicative
import Control.Lens hiding ((.=), at)
import Data.Aeson
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text.Region hiding (Region(..))
import qualified Data.Text.Region as R

import HsDev.Symbols.Location (Position(..), Region(..))
import HsDev.Tools.Base
import HsDev.Tools.Types
import HsDev.Util ((.::))

data Correction = Correction {
	_correctionMessage :: String,
	_corrector :: Replace String }
		deriving (Eq, Show)

instance ToJSON Correction where
	toJSON (Correction msg cor) = object [
		"message" .= msg,
		"corrector" .= cor]

instance FromJSON Correction where
	parseJSON = withObject "correction" $ \v -> Correction <$>
		v .:: "message" <*>
		v .:: "corrector"

makeLenses ''Correction

corrections :: [Note OutputMessage] -> [Note Correction]
corrections = mapMaybe toCorrection where
	toCorrection :: Note OutputMessage -> Maybe (Note Correction)
	toCorrection n = useSuggestion <|> findCorrector n where
		-- Use existing suggestion
		useSuggestion :: Maybe (Note Correction)
		useSuggestion = do
			sugg <- view (note . messageSuggestion) n
			return $ set
				note
				(Correction
					(view (note . message) n)
					(replace (fromRegion $ view noteRegion n) sugg))
				n

-- | Apply corrections
autoFix :: [Note Correction] -> ([Note Correction], Maybe String) -> ([Note Correction], Maybe String)
autoFix ns (upd, mcts) = (over (each . note . corrector . replaceRegion) (update act) upd, over _Just (apply act) mcts) where
	act = Edit (ns ^.. each . note . corrector)

type CorrectorMatch = Note OutputMessage -> Maybe (Note Correction)

correctors :: [CorrectorMatch]
correctors = [
	match "^The (?:qualified )?import of .([\\w\\.]+). is redundant" $ \_ rgn -> Correction -- There are different quotes in Windows/Linux
		"Redundant import"
		(cut
			(expandLines rgn)),
	match "^(.*?)\nFound:\n  (.*?)\nWhy not:\n  (.*?)$" $ \g rgn -> Correction
		(g `at` 1)
		(replace
			((rgn ^. regionFrom) `regionSize` pt 0 (length $ g `at` 2))
			(g `at` 3))]

match :: String -> ((Int -> Maybe String) -> R.Region -> Correction) -> CorrectorMatch
match pat f n = do
	g <- matchRx pat (view (note . message) n)
	return $ set note (f g (fromRegion $ view noteRegion n)) n

findCorrector :: Note OutputMessage -> Maybe (Note Correction)
findCorrector n = listToMaybe $ mapMaybe ($ n) correctors

fromRegion :: Region -> R.Region
fromRegion (Region f t) = fromPosition f `till` fromPosition t

fromPosition :: Position -> Point
fromPosition (Position l c) = pt (pred l) (pred c)
