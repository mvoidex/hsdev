module HsDev.Server.Response (
	Response(..)
	) where

import Data.Aeson

data Response = Response

instance ToJSON Response where
	toJSON Response = undefined

instance FromJSON Response where
	parseJSON = withObject "response" undefined
