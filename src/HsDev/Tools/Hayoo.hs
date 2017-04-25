{-# LANGUAGE OverloadedStrings #-}

module HsDev.Tools.Hayoo (
	-- * Types
	HayooResult(..), HayooSymbol(..),
	hayooAsSymbol,
	-- * Search help online
	hayoo,
	-- * Utils
	untagDescription,

	-- * Reexportss
	module Control.Monad.Except
	) where

import Control.Arrow
import Control.Applicative
import Control.Lens (lens)
import Control.Monad.Except

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Either
import Data.Maybe (listToMaybe, fromJust)
import Network.HTTP
import Data.String (fromString)
import qualified Data.Text as T (unpack, unlines)

import HsDev.Symbols
import HsDev.Tools.Base (replaceRx)
import HsDev.Util

-- | Hayoo response
data HayooResult = HayooResult {
	resultMax :: Int,
	resultOffset :: Int,
	resultCount :: Int,
	resultResult :: [HayooSymbol] }
		deriving (Eq, Ord, Read, Show)

-- | Hayoo symbol
data HayooSymbol = HayooSymbol {
	resultUri :: String,
	tag :: String,
	hayooPackage :: String,
	hayooName :: String,
	hayooSource :: String,
	hayooDescription :: String,
	hayooSignature :: String,
	hayooModules :: [String],
	hayooScore :: Double,
	hayooType :: String }
		deriving (Eq, Ord, Read, Show)

newtype HayooValue = HayooValue { hayooValue :: Either Value HayooSymbol }

instance FromJSON HayooResult where
	parseJSON = withObject "hayoo response" $ \v -> HayooResult <$>
		(v .:: "max") <*>
		(v .:: "offset") <*>
		(v .:: "count") <*>
		((rights . map hayooValue) <$> (v .:: "result"))

instance Sourced HayooSymbol where
	sourcedName = lens g' s' where
		g' = fromString . hayooName
		s' sym n = sym { hayooName = T.unpack n }
	sourcedModule = lens g' s' where
		g' h = ModuleId nm (OtherLocation $ fromString $ resultUri h) where
			nm = maybe mempty fromString $ listToMaybe $ hayooModules h
		s' h _ = h
	sourcedDocs f h = (\d' -> h { hayooDescription = T.unpack d' }) <$> f (fromString $ hayooDescription h)

instance Documented HayooSymbol where
	brief f
		| hayooType f == "function" = fromString $ hayooName f ++ " :: " ++ hayooSignature f
		| otherwise = fromString $ hayooType f ++ " " ++ hayooName f
	detailed f = T.unlines $ defaultDetailed f ++ map fromString online where
		online = [
			"", "Hayoo online documentation", "",
			"Package: " ++ hayooPackage f,
			"Hackage URL: " ++ resultUri f]

instance FromJSON HayooSymbol where
	parseJSON = withObject "symbol" $ \v -> HayooSymbol <$>
		(v .:: "resultUri") <*>
		(v .:: "tag") <*>
		(v .:: "resultPackage") <*>
		(v .:: "resultName") <*>
		(v .:: "resultSource") <*>
		(v .:: "resultDescription") <*>
		(v .:: "resultSignature") <*>
		(v .:: "resultModules") <*>
		(v .:: "resultScore") <*>
		(v .:: "resultType")

instance FromJSON HayooValue where
	parseJSON v = HayooValue <$> ((Right <$> parseJSON v) <|> pure (Left v))

-- | 'HayooFunction' as 'Symbol'
hayooAsSymbol :: HayooSymbol -> Maybe Symbol
hayooAsSymbol f
	| hayooType f `elem` ["function", "type", "newtype", "data", "class"] = Just Symbol {
		_symbolId = SymbolId {
			_symbolName = fromString $ hayooName f,
			_symbolModule = ModuleId {
				_moduleName = fromString $ head $ hayooModules f,
				_moduleLocation = OtherLocation (fromString $ resultUri f) } },
		_symbolDocs = Just (fromString $ addOnline $ untagDescription $ hayooDescription f),
		_symbolPosition = Nothing,
		_symbolInfo = info }
	| otherwise = Nothing
	where
		-- Add other info
		addOnline d = unlines [
			d, "",
			"Hayoo online documentation",
			"",
			"Package: " ++ hayooPackage f,
			"Hackage URL: " ++ resultUri f]

		info
			| hayooType f == "function" = Function (Just $ fromString $ hayooSignature f)
			| hayooType f `elem` ["type", "newtype", "data", "class"] = (fromJust $ lookup (hayooType f) ctors) [] []
			| otherwise = error "Impossible"
		ctors = [("type", Type), ("newtype", NewType), ("data", Data), ("class", Class)]

-- | Search hayoo
hayoo :: String -> Maybe Int -> ExceptT String IO HayooResult
hayoo q page = do
	resp <- ExceptT $ (show +++ rspBody) <$> simpleHTTP (getRequest $ maybe id addPage page $ "http://hayoo.fh-wedel.de/json/?query=" ++ urlEncode q)
	ExceptT $ return $ eitherDecode $ L.pack resp
	where
		addPage :: Int -> String -> String
		addPage p s = s ++ "&page=" ++ show p

-- | Remove tags in description
untagDescription :: String -> String
untagDescription = replaceRx "</?\\w+[^>]*>" ""
