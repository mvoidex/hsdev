{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Tools.AutoFix (
	corrections,
	CorrectorMatch,
	correctors,
	match,
	findCorrector,

	module Data.Text.Region,
	module HsDev.Tools.Refact,
	module HsDev.Tools.Types
	) where

import Control.Applicative
import Control.Lens hiding ((.=), at)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Lens (unpacked)
import Data.Text.Region hiding (Region(..), update)
import qualified Data.Text.Region as R

import HsDev.Tools.Refact
import HsDev.Tools.Base
import HsDev.Tools.Types

instance Regioned a => Regioned (Note a) where
	regions = note . regions

corrections :: [Note OutputMessage] -> [Note Refact]
corrections = mapMaybe toRefact where
	toRefact :: Note OutputMessage -> Maybe (Note Refact)
	toRefact n = useSuggestion <|> findCorrector n where
		-- Use existing suggestion
		useSuggestion :: Maybe (Note Refact)
		useSuggestion = do
			sugg <- view (note . messageSuggestion) n
			return $ set
				note
				(Refact
					(view (note . message) n)
					(replace (fromRegion $ view noteRegion n) sugg))
				n

type CorrectorMatch = Note OutputMessage -> Maybe (Note Refact)

correctors :: [CorrectorMatch]
correctors = [
	match "^The (?:qualified )?import of .([\\w\\.]+). is redundant" $ \_ rgn -> Refact -- There are different quotes in Windows/Linux
		"Redundant import"
		(cut
			(expandLines rgn)),
	match "^(.*?)\nFound:\n  (.*?)\nWhy not:\n  (.*?)$" $ \g rgn -> Refact
		(g `at` 1)
		(replace
			((rgn ^. regionFrom) `regionSize` pt 0 (contentsLength $ g `at` 2))
			(g `at` 3))]

match :: String -> ((Int -> Maybe Text) -> R.Region -> Refact) -> CorrectorMatch
match pat f n = do
	g <- matchRx pat (view (note . message . unpacked) n)
	return $ set note (f (fmap fromString . g) (fromRegion $ view noteRegion n)) n

findCorrector :: Note OutputMessage -> Maybe (Note Refact)
findCorrector n = listToMaybe $ mapMaybe ($ n) correctors
