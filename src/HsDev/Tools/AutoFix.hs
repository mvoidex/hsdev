{-# LANGUAGE OverloadedStrings #-}

module HsDev.Tools.AutoFix (
	Correction(..),
	correct, corrections,
	applyFix, updateRegion, autoFix, fixUpdate,
	CorrectorMatch,
	correctors,
	match,
	findCorrector
	) where

import Control.Applicative
import Data.Aeson
import Data.Maybe (listToMaybe, mapMaybe)

import Data.Mark hiding (at)
import HsDev.Symbols.Location (Location(..), Position(..), moduleSource)
import HsDev.Tools.Base (matchRx, at)
import HsDev.Tools.GhcMod
import HsDev.Util ((.::))

data Correction = Correction {
	correctionFile :: FilePath,
	correctionType :: String,
	description :: String,
	message :: String,
	solution :: String,
	corrector :: [Edit Char] }
		deriving (Eq, Read, Show)

instance ToJSON Correction where
	toJSON (Correction f t desc msg sol cor) = object [
		"file" .= f,
		"type" .= t,
		"description" .= desc,
		"message" .= msg,
		"solution" .= sol,
		"corrector" .= cor]

instance FromJSON Correction where
	parseJSON = withObject "correction" $ \v -> Correction <$>
		v .:: "file" <*>
		v .:: "type" <*>
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
		file <- moduleSource $ locationModule (errorLocation msg)
		Position l c <- locationPosition (errorLocation msg)
		let
			pt = Point (pred l) (pred c)
		findCorrector file pt (errorMessage msg)

applyFix :: [Correction] -> EditM Char [String]
applyFix corrs = mapM_ correct corrs >> editResult

updateRegion :: Correction -> EditM Char Correction
updateRegion corr = do
	c' <- sequence [Edit <$> mapRegion r <*> pure cts | Edit r cts <- corrector corr]
	return $ corr { corrector = c' }

autoFix :: String -> [Correction] -> String
autoFix cts corrs = untext $ runEdit (text cts) (applyFix corrs)

fixUpdate :: String -> [Correction] -> [Correction] -> (String, [Correction])
fixUpdate cts fix' up' = runEdit (text cts) $ (,) <$> (untext <$> applyFix fix') <*> mapM updateRegion up'

type CorrectorMatch = FilePath -> Point -> String -> Maybe Correction

correctors :: [CorrectorMatch]
correctors = [
	match "^The import of `([\\w\\.]+)' is redundant" $ \g file pt -> Correction file
		"Redundant import"
		("Redundant import: " ++ (g `at` 1)) ""
		"Remove import"
		[eraser (pt `regionSize` linesSize 1)],
	match "Found:\n  (.*?)\nWhy not:\n  (.*?)$" $ \g file pt -> Correction file
		"Why not?"
		("Replace '" ++ (g `at` 1) ++ "' with '" ++ (g `at` 2) ++ "'") ""
		"Replace with suggestion"
		[replacer (pt `regionSize` stringSize (length $ g `at` 1)) (text $ g `at` 2)]]

match :: String -> ((Int -> Maybe String) -> FilePath -> Point -> Correction) -> CorrectorMatch
match pat f file pt str = do
	g <- matchRx pat str
	return (f g file pt) { correctionFile = file, message = str }

findCorrector :: FilePath -> Point -> String -> Maybe Correction
findCorrector file pt msg = listToMaybe $ mapMaybe (\corr -> corr file pt msg) correctors
