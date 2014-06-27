{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module HsDev.Message (
	Message(..),
	messages
	) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Aeson
import qualified Data.HashMap.Strict as HM (null)
import Data.List (unfoldr)
import Data.Maybe (fromJust, isNothing)

import HsDev.Util ((.::), objectUnion)

-- | Message with id to link request and response
data Message a = Message {
	message :: a,
	messageId :: String }

instance ToJSON a => ToJSON (Message a) where
	toJSON (Message m i) = toJSON m `objectUnion` object ["id" .= i]

instance FromJSON a => FromJSON (Message a) where
	parseJSON = withObject "message" $ \v -> Message <$> parseJSON (Object v) <*> v .:: "id"

-- | Get messages with id
messages :: [Message a] -> String -> [a]
messages msgs i = map message . filter ((== i) . messageId) $ msgs
