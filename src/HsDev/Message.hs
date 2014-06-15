{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module HsDev.Message (
	Message(..), Chain(..),
	chainEnd, isChainEnd, splitChain, messages
	) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Aeson
import qualified Data.HashMap.Strict as HM (null)
import Data.List (unfoldr)
import Data.Maybe (fromJust, isNothing)

import HsDev.Util ((.::))

-- | Message with id to link request and response
data Message a = Message {
	message :: a,
	messageId :: String }

instance ToJSON a => ToJSON (Message a) where
	toJSON (Message m i) = object [
		"message" .= m,
		"id" .= i]

instance FromJSON a => FromJSON (Message a) where
	parseJSON = withObject "message" $ \v -> Message <$> v .:: "message" <*> v .:: "id"

-- | One line of chain of messages, where Chain Nothing
newtype Chain a = Chain { unChain :: Maybe a } deriving (Eq, Ord, Functor)

instance ToJSON a => ToJSON (Chain a) where
	toJSON = maybe Null toJSON . unChain

instance FromJSON a => FromJSON (Chain a) where
	parseJSON Null = return $ Chain Nothing
	parseJSON v = fmap (Chain . Just) $ parseJSON v

chainEnd :: Chain a
chainEnd = Chain Nothing

isChainEnd :: Chain a -> Bool
isChainEnd = isNothing . unChain

-- | Split chain of messages by end mark to list of connected messages
splitChain :: [Chain a] -> [[a]]
splitChain = unfoldr break' where
	break' :: [Chain a] -> Maybe ([a], [Chain a])
	break' [] = Nothing
	break' cs = Just $ (map (fromJust . unChain) *** drop 1) $ break isChainEnd cs

-- | Get message chains by id
messages :: [Message (Chain a)] -> String -> [[a]]
messages msgs i = splitChain . map message . filter ((== i) . messageId) $ msgs
