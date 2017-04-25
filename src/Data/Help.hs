module Data.Help (
	Help(..),
	indent, indentHelp, detailed, indented
	) where

import Data.Text (Text)
import qualified Data.Text as T

class Help a where
	brief :: a -> Text
	help :: a -> [Text]

indent :: Text -> Text
indent = T.cons '\t'

indentHelp :: [Text] -> [Text]
indentHelp [] = []
indentHelp (h:hs) = h : map indent hs

detailed :: Help a => a -> [Text]
detailed v = brief v : help v

indented :: Help a => a -> [Text]
indented = indentHelp . detailed
