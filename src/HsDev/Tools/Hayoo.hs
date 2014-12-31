{-# LANGUAGE OverloadedStrings #-}

module HsDev.Tools.Hayoo (
	-- * Types
	HayooResult(..), HayooSymbol(..),
	hayooAsDeclaration,
	-- * Search help online
	hayoo,
	-- * Utils
	untagDescription
	) where

import Control.Arrow
import Control.Applicative
import Control.Monad.Error

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Either
import Network.HTTP
import Data.String (fromString)

import HsDev.Symbols
import HsDev.Symbols.Documented
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

instance Symbol HayooSymbol where
	symbolName = fromString . hayooName
	symbolQualifiedName f = fromString $ case hayooModules f of
		[] -> hayooName f
		(m:_) -> m ++ "." ++ hayooName f
	symbolDocs = Just . fromString . hayooDescription
	symbolLocation r = Location (ModuleSource $ Just $ resultUri r) Nothing where

instance Documented HayooSymbol where
	brief f
		| hayooType f == "function" = hayooName f ++ " :: " ++ hayooSignature f
		| otherwise = hayooType f ++ " " ++ hayooName f
	detailed f = unlines $ defaultDetailed f ++ online where
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

-- | 'HayooFunction' as 'Declaration'
hayooAsDeclaration :: HayooSymbol -> Maybe ModuleDeclaration
hayooAsDeclaration f
	| hayooType f `elem` ["function", "type", "newtype", "data", "class"] = Just ModuleDeclaration {
		declarationModuleId = ModuleId {
			moduleIdName = fromString $ head $ hayooModules f,
			moduleIdLocation = ModuleSource (Just $ resultUri f) },
		moduleDeclaration = Declaration {
			declarationName = fromString $ hayooName f,
			declarationDefined = Nothing,
			declarationImported = Nothing,
			declarationDocs = Just (fromString $ addOnline $ untagDescription $ hayooDescription f),
			declarationPosition = Nothing,
			declaration = declInfo } }
	| otherwise = Nothing
	where
		-- Add other info
		addOnline d = unlines [
			d, "",
			"Hayoo online documentation",
			"",
			"Package: " ++ hayooPackage f,
			"Hackage URL: " ++ resultUri f]

		declInfo
			| hayooType f == "function" = Function (Just $ fromString $ hayooSignature f) []
			| hayooType f `elem` ["type", "newtype", "data", "class"] = declarationTypeCtor (hayooType f) $ TypeInfo Nothing [] Nothing
			| otherwise = error "Impossible"

-- | Search hayoo
hayoo :: String -> Maybe Int -> ErrorT String IO HayooResult
hayoo q page = do
	resp <- ErrorT $ (show +++ rspBody) <$> simpleHTTP (getRequest $ maybe id addPage page $ "http://hayoo.fh-wedel.de/json/?query=" ++ urlEncode q)
	ErrorT $ return $ eitherDecode $ L.pack resp
	where
		addPage :: Int -> String -> String
		addPage p s = s ++ "&page=" ++ show p

-- | Remove tags in description
untagDescription :: String -> String
untagDescription = replaceRx "</?\\w+[^>]*>" ""
