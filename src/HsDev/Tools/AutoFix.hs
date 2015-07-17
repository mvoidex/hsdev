{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module HsDev.Tools.AutoFix (
	Correction(..), correctionMessage, corrector,
	correct, corrections,
	autoFix_, autoFix, updateRange,
	CorrectorMatch,
	correctors,
	match,
	findCorrector,

	module Data.Mark,
	module HsDev.Tools.Types
	) where

import Control.Applicative
import Control.Lens (makeLenses, set, view)
import Data.Aeson
import Data.Maybe (listToMaybe, mapMaybe)

import Data.Mark hiding (at, length)
import HsDev.Symbols.Location
import HsDev.Tools.Base
import HsDev.Tools.Types
import HsDev.Util ((.::))

data Correction = Correction {
	_correctionMessage :: String,
	_corrector :: Replace String () }
		deriving (Eq, Read, Show)

instance ToJSON Correction where
	toJSON (Correction msg cor) = object [
		"message" .= msg,
		"corrector" .= cor]

instance FromJSON Correction where
	parseJSON = withObject "correction" $ \v -> Correction <$>
		v .:: "message" <*>
		v .:: "corrector"

makeLenses ''Correction

correct :: Correction -> EditM String ()
correct c = run [_corrector c]

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
autoFix_ :: [Correction] -> EditM String ()
autoFix_ = mapM_ correct

-- | Apply corrections and update rest correction positions
autoFix :: [Correction] -> [Correction] -> EditM String [Correction]
autoFix fix' up' = autoFix_ fix' >> mapM updateRange up'

updateRange :: Correction -> EditM String Correction
updateRange corr = do
	region' <- mapRange $ replaceRange (_corrector corr)
	return $ corr { _corrector = (_corrector corr) { replaceRange = region' } }

type CorrectorMatch = Note OutputMessage -> Maybe (Note Correction)

correctors :: [CorrectorMatch]
correctors = [
	match "^The import of `([\\w\\.]+)' is redundant" $ \_ rgn -> Correction
		"Redunant import"
		(replace
			(rangeFrom rgn `rangeSize` linesSize 1)
			""),
	match "^(.*?)\nFound:\n  (.*?)\nWhy not:\n  (.*?)$" $ \g rgn -> Correction
		(g `at` 1)
		(replace
			(rangeFrom rgn `rangeSize` stringSize (length $ g `at` 2))
			(g `at` 3))]

match :: String -> ((Int -> Maybe String) -> Range -> Correction) -> CorrectorMatch
match pat f n = do
	g <- matchRx pat (view (note . message) n)
	return $ set note (f g (fromRegion $ view noteRegion n)) n

findCorrector :: Note OutputMessage -> Maybe (Note Correction)
findCorrector n = listToMaybe $ mapMaybe ($ n) correctors

fromRegion :: Region -> Range
fromRegion (Region f t) = fromPosition f `till` fromPosition t

fromPosition :: Position -> Point
fromPosition (Position l c) = point (pred l) (pred c)
