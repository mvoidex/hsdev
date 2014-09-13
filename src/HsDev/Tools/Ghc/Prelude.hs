module HsDev.Tools.Ghc.Prelude (
	reduce, one, trim
	) where

import Data.Char (isSpace)

reduce :: ([a] -> a) -> [a] -> [a]
reduce = (return .)

one :: a -> [a]
one = return

trim :: String -> String
trim = p . p where
	p = reverse . dropWhile isSpace
