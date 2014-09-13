module HsDev.Tools.Ghc.Prelude (
	reduce, one
	) where

reduce :: ([a] -> a) -> [a] -> [a]
reduce = (return .)

one :: a -> [a]
one = return
