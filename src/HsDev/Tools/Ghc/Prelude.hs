module HsDev.Tools.Ghc.Prelude (
	reduce, one, trim,
	rx, srx, splitRx,

	module Data.Char,
	module Data.List,
	module Data.Maybe
	) where

import Data.Array (assocs)
import Data.Char
import Data.List
import Data.Maybe
import Text.Regex.PCRE

reduce :: ([a] -> a) -> [a] -> [a]
reduce = (return .)

one :: a -> [a]
one = return

trim :: String -> String
trim = p . p where
	p = reverse . dropWhile isSpace

rx :: String -> String -> Maybe String
rx r s = case s =~ r of
	"" -> Nothing
	res -> Just res

srx :: String -> String -> String -> String
srx pat s = concat . unfoldr split' . Just where
	split' :: Maybe String -> Maybe (String, Maybe String)
	split' Nothing = Nothing
	split' (Just str) = case mrMatch r of
		"" -> Just (mrBefore r, Nothing)
		_ -> Just (mrBefore r ++ subst, Just $ mrAfter r)
		where
			r = str =~ pat
			groups = filter (not . null . snd) $ assocs $ mrSubs r
			look i = lookup i groups
			subst = subst' s where
				subst' :: String -> String
				subst' "" = ""
				subst' "\\" = "\\"
				subst' ('\\':'\\':ss') = '\\' : subst' ss'
				subst' ('\\':ss') = case span isDigit ss' of
					([], _) -> '\\' : subst' ss'
					(num, tl) -> fromMaybe "" (look $ read num) ++ subst' tl
				subst' (s':ss') = s' : subst' ss'

splitRx :: String -> String -> [String]
splitRx pat = unfoldr split' . Just where
	split' :: Maybe String -> Maybe (String, Maybe String)
	split' Nothing = Nothing
	split' (Just str) = case mrMatch r of
		"" -> Just (mrBefore r, Nothing)
		_ -> Just (mrBefore r, Just $ mrAfter r)
		where
			r = str =~ pat
