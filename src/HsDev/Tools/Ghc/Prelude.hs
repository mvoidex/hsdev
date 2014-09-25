module HsDev.Tools.Ghc.Prelude (
	reduce, one, trim,
	rx, srx
	) where

import Data.Char (isSpace)
import Text.RegexPR

reduce :: ([a] -> a) -> [a] -> [a]
reduce = (return .)

one :: a -> [a]
one = return

trim :: String -> String
trim = p . p where
	p = reverse . dropWhile isSpace

rx :: String -> String -> Maybe String
rx r = fmap (fst . fst) . matchRegexPR r

srx :: String -> String -> String -> String
srx = gsubRegexPR
