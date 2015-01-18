{-# LANGUAGE OverloadedStrings #-}

module HsDev.Tools.AutoFix (
	Correction(..),
	correct, corrections,
	autoFix,
	Corrector, CorrectorMatch,
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
	corrector :: Corrector }

instance ToJSON Correction where
	toJSON (Correction desc msg _) = object ["description" .= desc, "message" .= msg]

instance FromJSON Correction where
	parseJSON = withObject "correction" $ \v -> Correction <$> v .:: "description" <*> v .:: "message" <*> corr v where
		corr v = do
			m <- v .:: "message"
			maybe
				(fail "Can't find corrector for this message")
				(return . corrector . fst) $
				findCorrector m

correct :: Mark Correction -> EditM Char ()
correct (Mark c r) = corrector c r

corrections :: [OutputMessage] -> [Mark Correction]
corrections = mapMaybe toMark where
	toMark :: OutputMessage -> Maybe (Mark Correction)
	toMark msg = do
		Position l c <- locationPosition (errorLocation msg)
		let
			pt = Point (pred l) (pred c)
		(corr, sz) <- findCorrector (errorMessage msg)
		return $ Mark corr (regionSize pt sz)

autoFix :: String -> EditM Char () -> String
autoFix cts correct' = untext $ evalEdit (text cts) correct'

type Corrector = Region -> EditM Char ()
type CorrectorMatch = String -> Maybe (Correction, Size)

correctors :: [CorrectorMatch]
correctors = [
	match "^The import of `([\\w\\.]+)' is redundant" $ \g ->
		("Redundant import: " ++ (g `at` 1), erase, linesSize 1),
	match "Found:\n  (.*?)\nWhy not:\n  (.*?)$" $ \g ->
		("Replace '" ++ (g `at` 1) ++ "' with '" ++ (g `at` 2) ++ "'", \rgn -> replace rgn (text $ g `at` 2), stringSize $ length (g `at` 1))]

match :: String -> ((Int -> Maybe String) -> (String, Corrector, Size)) -> CorrectorMatch
match pat f str = do
	g <- matchRx pat str
	let
		(desc, corr, sz) = f g
	return $ (Correction desc str corr, sz)

findCorrector :: String -> Maybe (Correction, Size)
findCorrector msg = listToMaybe $ mapMaybe ($ msg) correctors
