{-# LANGUAGE OverloadedStrings, DeriveFunctor, TypeSynonymInstances, FlexibleInstances, TemplateHaskell #-}

module HsDev.Server.Message (
	Message(..), messageId, message,
	messagesById,
	Notification(..), Result(..), ResultPart(..),
	Response(..), isNotification, notification, result, responseError, resultPart,
	groupResponses, responsesById
	) where

import Control.Arrow (first)
import Control.Applicative
import Control.Lens (makeLenses)
import Control.Monad (join)
import Data.Aeson hiding (Error, Result)
import Data.Aeson.Types (Pair)
import Data.Either (lefts, isRight)
import Data.List (unfoldr)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (unpack)

import HsDev.Util ((.::), (.::?), objectUnion)

-- | Message with id to link request and response
data Message a = Message {
	_messageId :: Maybe String,
	_message :: a }
		deriving (Eq, Ord, Functor)

makeLenses ''Message

instance ToJSON a => ToJSON (Message a) where
	toJSON (Message i m) = object ["id" .= i] `objectUnion` toJSON m

instance FromJSON a => FromJSON (Message a) where
	parseJSON = withObject "message" $ \v ->
		Message <$> fmap join (v .::? "id") <*> parseJSON (Object v)

instance Foldable Message where
	foldMap f (Message _ m) = f m

instance Traversable Message where
	traverse f (Message i m) = Message i <$> f m

-- | Get messages by id
messagesById :: Maybe String -> [Message a] -> [a]
messagesById i = map _message . filter ((== i) . _messageId)

-- | Notification from server
data Notification = Notification Value

instance ToJSON Notification where
	toJSON (Notification v) = object ["notify" .= v]

instance FromJSON Notification where
	parseJSON = withObject "notification" $ \v -> Notification <$> v .:: "notify"

-- | Result from server
data Result =
	Result Value |
	-- ^ Result
	Error String (Map String Value)
	-- ^ Error

instance ToJSON Result where
	toJSON (Result r) = object ["result" .= r]
	toJSON (Error msg rs) = object [
		"error" .= msg,
		"details" .= toJSON rs]

instance FromJSON Result where
	parseJSON = withObject "result" $ \v ->
		(Result <$> v .:: "result") <|>
		(Error <$> v .:: "error" <*> v .:: "details")

-- | Part of result list, returns via notification
data ResultPart = ResultPart Value

instance ToJSON ResultPart where
	toJSON (ResultPart r) = object ["result-part" .= r]

instance FromJSON ResultPart where
	parseJSON = withObject "result-part" $ \v -> ResultPart <$> v .:: "result-part"

newtype Response = Response { unResponse :: Either Notification Result }

isNotification :: Response -> Bool
isNotification = either (const True) (const False) . unResponse

notification :: ToJSON a => a -> Response
notification = Response . Left . Notification . toJSON

result :: ToJSON a => a -> Response
result = Response . Right . Result . toJSON

responseError :: String -> [Pair] -> Response
responseError e ds = Response $ Right $ Error e $ M.fromList $ map (first unpack) ds

resultPart :: ToJSON a => a  -> Notification
resultPart = Notification . toJSON . ResultPart . toJSON

instance ToJSON Response where
	toJSON (Response (Left n)) = toJSON n
	toJSON (Response (Right r)) = toJSON r

instance FromJSON Response where
	parseJSON v = Response <$> ((Left <$> parseJSON v) <|> (Right <$> parseJSON v))

groupResponses :: [Response] -> [([Notification], Result)]
groupResponses = unfoldr break' where
	break' :: [Response] -> Maybe (([Notification], Result), [Response])
	break' [] = Nothing
	break' cs =  Just ((lefts (map unResponse ns), r), drop 1 cs') where
		(ns, cs') = break (isRight . unResponse) cs
		r = case cs' of
			(Response (Right r') : _) -> r'
			[] -> Error "groupResponses: no result" mempty
			_ -> error "groupResponses: impossible happened"

responsesById :: Maybe String -> [Message Response] -> [([Notification], Result)]
responsesById i = groupResponses . messagesById i
