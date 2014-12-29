module HsDev.Tools.Ghc.Prelude (
	reduce, one, trim,
	rx, srx
	) where

import Data.Char (isSpace)
import Data.List (intercalate, unfoldr)
import Text.Regex.PCRE

reduce :: ([a] -> a) -> [a] -> [a]
reduce = (return .)

one :: a -> [a]
one = return

trim :: String -> String
trim = p . p where
	p = reverse . dropWhile isSpace

rx :: String -> String -> Maybe String
rx r s = case r =~ s of
	"" -> Nothing
	res -> Just res

srx :: String -> String -> String -> String
srx r w = intercalate w . splitRx r

splitRx :: String -> String -> [String]
splitRx pat = unfoldr split' . Just where
	split' :: Maybe String -> Maybe (String, Maybe String)
	split' Nothing = Nothing
	split' (Just str) = case str =~ pat of
		(pre, "", "") -> Just (pre, Nothing)
		(pre, _, post) -> Just (pre, Just post)
