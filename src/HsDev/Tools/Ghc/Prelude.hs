module HsDev.Tools.Ghc.Prelude (
	reduce, trim,
	-- * Regexes
	rx, srx, splitRx,
	-- * Case
	lowerCase, upperCase, titleCase, camelCase, underscoreCase,

	module Control.Lens,
	module Data.Char,
	module Data.List,
	module Data.Maybe
	) where

import Control.Lens
import Data.Array (assocs)
import Data.Char
import Data.List hiding (uncons)
import Data.Maybe
import Text.Regex.PCRE

-- | Reduce list to one element
reduce :: ([a] -> a) -> [a] -> [a]
reduce = (return .)

-- | Trim string
trim :: String -> String
trim = p . p where
	p = reverse . dropWhile isSpace

-- | Match regex
rx :: String -> String -> Maybe String
rx r s = case s =~ r of
	"" -> Nothing
	res -> Just res

-- | Replace regex
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

-- | Split by regex
splitRx :: String -> String -> [String]
splitRx pat = unfoldr split' . Just where
	split' :: Maybe String -> Maybe (String, Maybe String)
	split' Nothing = Nothing
	split' (Just str) = case mrMatch r of
		"" -> Just (mrBefore r, Nothing)
		_ -> Just (mrBefore r, Just $ mrAfter r)
		where
			r = str =~ pat

lowerCase :: String -> String
lowerCase = map toLower

upperCase :: String -> String
upperCase = map toUpper

-- | Convert to title case
titleCase :: String -> String
titleCase = over _head toUpper

-- | Convert to camel case
camelCase :: String -> String
camelCase = concatMap titleCase . splitRx "[\\s_]+"

-- | Convert to underscore case
underscoreCase :: String -> String
underscoreCase = intercalate "_" . map lowerCase . unfoldr break' where
	break' :: String -> Maybe (String, String)
	break' str = do
		(s, ss) <- uncons str
		let
			(h, tl) = break isUpper ss
		return (s:h, tl)
