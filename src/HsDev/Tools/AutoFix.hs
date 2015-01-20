{-# LANGUAGE OverloadedStrings #-}

module HsDev.Tools.AutoFix (
	Correction(..),
	correct, corrections,
	autoFix_, autoFix, updateRegion,
	CorrectorMatch,
	correctors,
	match,
	findCorrector,

	Canonicalize(..),

	module Data.Mark
	) where

import Control.Applicative
import Data.Aeson
import Data.Maybe (listToMaybe, mapMaybe)

import Data.Mark hiding (at, Editable(..))
import HsDev.Symbols (Canonicalize(..))
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
	corrector :: [Replace String] }
		deriving (Eq, Read, Show)

instance Canonicalize Correction where
	canonicalize c = do
		f' <- canonicalize (correctionFile c)
		return c { correctionFile = f' }

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

correct :: Correction -> EditM String ()
correct = run . corrector

corrections :: [OutputMessage] -> [Correction]
corrections = mapMaybe toCorrection where
	toCorrection :: OutputMessage -> Maybe Correction
	toCorrection msg = do
		file <- moduleSource $ locationModule (errorLocation msg)
		Position l c <- locationPosition (errorLocation msg)
		let
			pt = Point (pred l) (pred c)
		findCorrector file pt (errorMessage msg)

-- | Apply corrections
autoFix_ :: [Correction] -> EditM String ()
autoFix_ = mapM_ correct

-- | Apply corrections and update rest correction positions
autoFix :: [Correction] -> [Correction] -> EditM String [Correction]
autoFix fix' up' = autoFix_ fix' >> mapM updateRegion up'

updateRegion :: Correction -> EditM String Correction
updateRegion corr = do
	c' <- sequence [Replace <$> mapRegion r <*> pure cts | Replace r cts <- corrector corr]
	return $ corr { corrector = c' }

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
		[replacer (pt `regionSize` stringSize (length $ g `at` 1)) (g `at` 2)]]

match :: String -> ((Int -> Maybe String) -> FilePath -> Point -> Correction) -> CorrectorMatch
match pat f file pt str = do
	g <- matchRx pat str
	return (f g file pt) { correctionFile = file, message = str }

findCorrector :: FilePath -> Point -> String -> Maybe Correction
findCorrector file pt msg = listToMaybe $ mapMaybe (\corr -> corr file pt msg) correctors
