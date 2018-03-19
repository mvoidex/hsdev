module Control.Apply.Util (
	(&), chain
	) where

(&) :: a -> (a -> b) -> b
(&) = flip ($)

chain :: [a -> a] -> a -> a
chain = foldr (.) id
