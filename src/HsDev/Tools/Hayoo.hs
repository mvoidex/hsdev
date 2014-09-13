{-# LANGUAGE OverloadedStrings #-}

module HsDev.Tools.Hayoo (
	-- * Types
	HayooResult(..), HayooFunction(..), HayooCompletion(..), HayooName(..),
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
import Network.HTTP
import Text.RegexPR (gsubRegexPR)

import HsDev.Symbols
import HsDev.Symbols.Documented
import HsDev.Util

-- | Hayoo response
data HayooResult = HayooResult {
	hayooMessage :: String,
	hayooHits :: Int,
	hayooFunctions :: [HayooFunction],
	hayooCompletions :: [HayooCompletion],
	hayooModules :: [HayooName],
	hayooPackages :: [HayooName] }
		deriving (Eq, Ord, Read, Show)

instance FromJSON HayooResult where
	parseJSON = withObject "hayoo response" $ \v -> HayooResult <$>
		(v .:: "message") <*>
		(v .:: "hits") <*>
		(v .:: "functions") <*>
		(v .:: "completions") <*>
		(v .:: "modules") <*>
		(v .:: "packages")

-- | Hayoo function information
data HayooFunction = HayooFunction {
	hayooName :: String,
	hayooSignature :: String,
	hayooModule :: String,
	hayooPackage :: String,
	hayooHackage :: String,
	hayooDescription :: String }
		deriving (Eq, Ord, Read, Show)

instance Symbol HayooFunction where
	symbolName = hayooName
	symbolQualifiedName f = hayooModule f ++ "." ++ hayooName f
	symbolDocs = Just . hayooDescription
	symbolLocation r = Location (ModuleSource $ Just $ hayooHackage r) Nothing where

instance Documented HayooFunction where
	brief f
		| hayooSignature f `elem` ["type", "newtype", "data", "class"] =
			hayooSignature f ++ " " ++ hayooName f
		| otherwise = hayooName f ++ " :: " ++ hayooSignature f
	detailed f = unlines $ defaultDetailed f ++ online where
		online = [
			"", "Hayoo online documentation", "",
			"Package: " ++ hayooPackage f,
			"Hackage URL: " ++ hayooHackage f]

instance FromJSON HayooFunction where
	parseJSON = withObject "function" $ \v -> HayooFunction <$>
		(v .:: "name") <*>
		(v .:: "signature") <*>
		(v .:: "module") <*>
		(v .:: "package") <*>
		(v .:: "uri") <*>
		(v .:: "description")

-- | Hayoo completions
data HayooCompletion = HayooCompletion {
	completionWord :: String,
	completionCount :: Int }
		deriving (Eq, Ord, Read, Show)

instance FromJSON HayooCompletion where
	parseJSON = withObject "completion" $ \v -> HayooCompletion <$>
		(v .:: "word") <*>
		(v .:: "count")

-- | Hayoo name
data HayooName = HayooName {
	nameName :: String,
	nameCount :: Int }
		deriving (Eq, Ord, Read, Show)

instance FromJSON HayooName where
	parseJSON = withObject "name" $ \v -> HayooName <$>
		(v .:: "name") <*>
		(v .:: "count")

-- | 'HayooFunction' as 'Declaration'
hayooAsDeclaration :: HayooFunction -> ModuleDeclaration
hayooAsDeclaration f = ModuleDeclaration {
	declarationModuleId = ModuleId {
		moduleIdName = hayooModule f,
		moduleIdLocation = ModuleSource (Just $ hayooHackage f) },
	moduleDeclaration = Declaration {
		declarationName = hayooName f,
		declarationDocs = Just (addOnline $ untagDescription $ hayooDescription f),
		declarationPosition = Nothing,
		declaration = declInfo } }
	where
		-- Add other info
		addOnline d = unlines $ [
			d, "",
			"Hayoo online documentation",
			"",
			"Package: " ++ hayooPackage f,
			"Hackage URL: " ++ hayooHackage f]

		declInfo
			| hayooSignature f `elem` ["type", "newtype", "data", "class"]
				= declarationTypeCtor (hayooSignature f) $ TypeInfo Nothing [] Nothing
			| otherwise = Function (Just $ hayooSignature f) []

-- | Search hayoo
hayoo :: String -> ErrorT String IO HayooResult
hayoo q = do
	resp <- ErrorT $ fmap (show +++ rspBody) $ simpleHTTP (getRequest $ "http://hayoo.fh-wedel.de/?query=" ++ urlEncode q)
	ErrorT $ return $ eitherDecode $ L.pack resp

-- | Remove tags in description
untagDescription :: String -> String
untagDescription = gsubRegexPR "</?\\w+[^>]*>" ""
