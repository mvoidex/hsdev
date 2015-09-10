{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module HsDev.Tools.AutoFix (
	Correction(..), correctionMessage, corrector,
	correct, corrections,
	autoFix_, autoFix,
	CorrectorMatch,
	correctors,
	match,
	findCorrector,

	module Data.Text.Region,
	module HsDev.Tools.Types
	) where

import Control.Applicative
import Control.Lens (makeLenses, set, view, (^.))
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
		deriving (Eq)

instance ToJSON Correction where
	toJSON (Correction msg cor) = object [
		"message" .= msg,
		"corrector" .= cor]

instance FromJSON Correction where
	parseJSON = withObject "correction" $ \v -> Correction <$>
		v .:: "message" <*>
		v .:: "corrector"

instance ApplyMap Correction where
	applyMap m (Correction msg c) = Correction msg (applyMap m c)

makeLenses ''Correction

correct :: Correction -> EditM String ()
correct c = run (Chain [_corrector c])

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
					(replace (fromRegion $ view noteRegion n) (by sugg)))
				n

-- | Apply corrections
autoFix_ :: [Correction] -> EditM String ()
autoFix_ = grouped . mapM_ correct

-- | Apply corrections and update rest correction positions
autoFix :: [Correction] -> [Correction] -> EditM String [Correction]
autoFix fix' up' = grouped $ autoFix_ fix' >> mapM update up'

type CorrectorMatch = Note OutputMessage -> Maybe (Note Correction)

correctors :: [CorrectorMatch]
correctors = [
	match "^The (?:qualified )?import of `([\\w\\.]+)' is redundant" $ \_ rgn -> Correction
		"Redundant import"
		(cut
			(expandLines rgn)),
	match "^(.*?)\nFound:\n  (.*?)\nWhy not:\n  (.*?)$" $ \g rgn -> Correction
		(g `at` 1)
		(replace
			((rgn ^. regionFrom) `regionSize` pt 0 (length $ g `at` 2))
			(by $ g `at` 3))]

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
