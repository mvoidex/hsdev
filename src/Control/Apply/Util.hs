module Control.Apply.Util (
	(&), chain, with
	) where

import Data.List (foldr)

(&) :: a -> (a -> b) -> b
(&) = flip ($)

chain :: [a -> a] -> a -> a
chain = foldr (.) id

-- | Flipped version of 'chain', which can be used like this:
--
-- >foo `with` [f, g, h]
with :: a -> [a -> a] -> a
with = flip chain
