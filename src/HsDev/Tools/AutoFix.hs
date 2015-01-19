{-# LANGUAGE OverloadedStrings #-}

module HsDev.Tools.AutoFix (
	Correction(..),
	correct, corrections,
	autoFix,
	CorrectorMatch,
	correctors,
	match,
	findCorrector
	) where

import Control.Applicative
import Data.Aeson
import Data.Maybe (listToMaybe, mapMaybe)

import Data.Mark hiding (at)
import HsDev.Symbols.Location (Location(..), Position(..))
import HsDev.Tools.Base (matchRx, at)
import HsDev.Tools.GhcMod
import HsDev.Util ((.::))

data Correction = Correction {
	description :: String,
	message :: String,
	solution :: String,
	corrector :: [Edit Char] }
		deriving (Eq, Read, Show)

instance ToJSON Correction where
	toJSON (Correction desc msg sol cor) = object [
		"description" .= desc,
		"message" .= msg,
		"solution" .= sol,
		"corrector" .= cor]

instance FromJSON Correction where
	parseJSON = withObject "correction" $ \v -> Correction <$>
		v .:: "description" <*>
		v .:: "message" <*>
		v .:: "solution" <*>
		v .:: "corrector"

correct :: Correction -> EditM Char ()
correct = apply . corrector

corrections :: [OutputMessage] -> [Correction]
corrections = mapMaybe toCorrection where
	toCorrection :: OutputMessage -> Maybe Correction
	toCorrection msg = do
		Position l c <- locationPosition (errorLocation msg)
		let
			pt = Point (pred l) (pred c)
		findCorrector pt (errorMessage msg)

autoFix :: String -> [Correction] -> String
autoFix cts corrs = untext $ evalEdit (text cts) (mapM_ correct corrs)

type CorrectorMatch = Point -> String -> Maybe Correction

correctors :: [CorrectorMatch]
correctors = [
	match "^The import of `([\\w\\.]+)' is redundant" $ \g pt -> Correction
		("Redundant import: " ++ (g `at` 1)) ""
		"Remove import"
		[eraser (pt `regionSize` linesSize 1)],
	match "Found:\n  (.*?)\nWhy not:\n  (.*?)$" $ \g pt -> Correction
		("Replace '" ++ (g `at` 1) ++ "' with '" ++ (g `at` 2) ++ "'") ""
		"Replace with suggestion"
		[replacer (pt `regionSize` stringSize (length $ g `at` 1)) (text $ g `at` 2)]]

match :: String -> ((Int -> Maybe String) -> Point -> Correction) -> CorrectorMatch
match pat f pt str = do
	g <- matchRx pat str
	return (f g pt) { message = str }

findCorrector :: Point -> String -> Maybe Correction
findCorrector pt msg = listToMaybe $ mapMaybe (\corr -> corr pt msg) correctors
