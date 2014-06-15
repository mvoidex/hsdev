{-# LANGUAGE OverloadedStrings #-}

module HsDev.Client.Request (
	Request(..)
	) where

import Control.Applicative
import Data.Aeson
import Data.Maybe
import Data.Monoid (mempty)

import System.Console.Command

import HsDev.Util ((.::), (.::?))

data Request = Request {
	requestCommand :: [String],
	requestArgs :: [String],
	requestOpts :: Opts String }

instance ToJSON Request where
	toJSON (Request n as os) = object [
		"command" .= n,
		"args" .= as,
		"opts" .= os]

instance FromJSON Request where
	parseJSON = withObject "request" $ \v -> Request <$>
		v .:: "command" <*>
		(fromMaybe [] <$> v .::? "args") <*>
		(fromMaybe mempty <$> v .::? "opts")
