module Data.Help (
	Help(..),
	indent, indentHelp, detailed, indented
	) where

class Help a where
	brief :: a -> String
	help :: a -> [String]

indent :: String -> String
indent = ('\t':)

indentHelp :: [String] -> [String]
indentHelp [] = []
indentHelp (h:hs) = h : map indent hs

detailed :: Help a => a -> [String]
detailed v = brief v : help v

indented :: Help a => a -> [String]
indented = indentHelp . detailed
