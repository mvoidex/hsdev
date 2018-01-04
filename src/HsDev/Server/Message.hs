{-# LANGUAGE OverloadedStrings, DeriveFunctor, TypeSynonymInstances, FlexibleInstances, TemplateHaskell #-}

module HsDev.Server.Message (
	Message(..), messageId, message,
	messagesById,
	Notification(..), Result(..), ResultPart(..),
	Response(..), isNotification, result, responseError,
	groupResponses,
	decodeMessage, encodeMessage,

	module HsDev.Server.Message.Lisp
	) where

import Control.Applicative
import Control.Lens (makeLenses)
import Control.Monad (join)
import Data.Aeson hiding (Error, Result)
import Data.Either (lefts, isRight)
import Data.List (unfoldr)
import Data.ByteString.Lazy.Char8 (ByteString)

import HsDev.Types
import HsDev.Util ((.::), (.::?), objectUnion)
import HsDev.Server.Message.Lisp

-- | Message with id to link request and response
data Message a = Message {
	_messageId :: Maybe String,
	_message :: a }
		deriving (Eq, Ord, Show, Functor)

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
data Notification = Notification Value deriving (Eq, Show)

instance ToJSON Notification where
	toJSON (Notification v) = object ["notify" .= v]

instance FromJSON Notification where
	parseJSON = withObject "notification" $ \v -> Notification <$> v .:: "notify"

-- | Result from server
data Result =
	Result Value |
	-- ^ Result
	Error HsDevError
	-- ^ Error
		deriving (Show)

instance ToJSON Result where
	toJSON (Result r) = object ["result" .= r]
	toJSON (Error e) = toJSON e

instance FromJSON Result where
	parseJSON j = (withObject "result" (\v -> (Result <$> v .:: "result")) j) <|> (Error <$> parseJSON j)

-- | Part of result list, returns via notification
data ResultPart = ResultPart Value

instance ToJSON ResultPart where
	toJSON (ResultPart r) = object ["result-part" .= r]

instance FromJSON ResultPart where
	parseJSON = withObject "result-part" $ \v -> ResultPart <$> v .:: "result-part"

newtype Response = Response { unResponse :: Either Notification Result } deriving (Show)

isNotification :: Response -> Bool
isNotification = either (const True) (const False) . unResponse

result :: ToJSON a => a -> Response
result = Response . Right . Result . toJSON

responseError :: HsDevError -> Response
responseError = Response . Right . Error

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
			[] -> Error $ OtherError "groupResponses: no result"
			_ -> error "groupResponses: impossible happened"

-- | Decode lisp or json request
decodeMessage :: FromJSON a => ByteString -> Either (Msg String) (Msg (Message a))
decodeMessage = decodeMsg

encodeMessage :: ToJSON a => Msg (Message a) -> ByteString
encodeMessage = encodeMsg
