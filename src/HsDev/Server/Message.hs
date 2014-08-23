{-# LANGUAGE OverloadedStrings, DeriveFunctor, TypeSynonymInstances, FlexibleInstances #-}

module HsDev.Server.Message (
	Message(..),
	messagesById,
	Request(..), requestToArgs,
	withOpts, withoutOpts,
	Notification(..), Result(..),
	Response(..), notification, result, responseError,
	groupResponses, responsesById
	) where

import Control.Arrow (first)
import Control.Applicative
import Control.Monad (join)
import Data.Aeson hiding (Error, Result)
import Data.Aeson.Types (Pair)
import Data.Either (lefts, isRight)
import Data.List (unfoldr)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid (mempty, mconcat)
import Data.Foldable (Foldable(foldMap))
import Data.Text (unpack)
import Data.Traversable (Traversable(traverse))

import System.Console.Args hiding (withOpts)
import HsDev.Util ((.::), (.::?), objectUnion)

-- | Message with id to link request and response
data Message a = Message {
	messageId :: Maybe String,
	message :: a }
		deriving (Eq, Ord, Functor)

instance ToJSON a => ToJSON (Message a) where
	toJSON (Message i m) = object ["id" .= i] `objectUnion` toJSON m

instance FromJSON a => FromJSON (Message a) where
	parseJSON = withObject "message" $ \v ->
		Message <$> (fmap join (v .::? "id")) <*> parseJSON (Object v)

instance Foldable Message where
	foldMap f (Message i m) = f m

instance Traversable Message where
	traverse f (Message i m) = Message i <$> f m

-- | Get messages by id
messagesById :: Maybe String -> [Message a] -> [a]
messagesById i = map message . filter ((== i) . messageId)

-- | Request from client
data Request = Request {
	requestCommand :: String,
	requestArgs :: [String],
	requestOpts :: Opts String }

requestToArgs :: Request -> Args
requestToArgs (Request c as opts) = Args (words c ++ as) opts

instance ToJSON Request where
	toJSON (Request c as os) = object [
		"command" .= c,
		"args" .= as,
		"opts" .= os]

instance FromJSON Request where
	parseJSON = withObject "request" $ \v -> Request <$>
		v .:: "command" <*>
		(fromMaybe [] <$> v .::? "args") <*>
		(fromMaybe mempty <$> v .::? "opts")

-- | Add options to request
withOpts :: Request -> [Opts String] -> Request
withOpts r os = r {
	requestOpts = mconcat (requestOpts r : os) }

-- | Remove options from request
withoutOpts :: Request -> [String] -> Request
withoutOpts r os = r {
	requestOpts = Opts $ foldr (.) id (map M.delete os) $ getOpts (requestOpts r) }

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
	toJSON (Result r) = object [
		"result" .= r]
	toJSON (Error msg rs) = object [
		"error" .= msg,
		"details" .= toJSON rs]

instance FromJSON Result where
	parseJSON = withObject "result" $ \v -> foldr1 (<|>) [
		Result <$> v .:: "result",
		Error <$> v .:: "error" <*> v .:: "details"]

type Response = Either Notification Result

notification :: ToJSON a => a -> Response
notification = Left . Notification . toJSON

result :: ToJSON a => a -> Response
result = Right . Result . toJSON

responseError :: String -> [Pair] -> Response
responseError e ds = Right $ Error e $ M.fromList $ map (first unpack) ds

instance ToJSON Response where
	toJSON (Left n) = toJSON n
	toJSON (Right r) = toJSON r

instance FromJSON Response where
	parseJSON v = (Left <$> parseJSON v) <|> (Right <$> parseJSON v)

groupResponses :: [Response] -> [([Notification], Result)]
groupResponses = unfoldr break' where
	break' :: [Response] -> Maybe (([Notification], Result), [Response])
	break' [] = Nothing
	break' cs =  Just ((lefts ns, r), drop 1 cs') where
		(ns, cs') = break isRight cs
		r = case cs' of
			(Right r' : _) -> r'
			[] -> Error "groupResponses: no result" mempty

responsesById :: Maybe String -> [Message Response] -> [([Notification], Result)]
responsesById i = groupResponses . messagesById i
