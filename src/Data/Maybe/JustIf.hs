module Data.Maybe.JustIf (
	justIf
	) where

-- | Return @Just@ if True
justIf :: a -> Bool -> Maybe a
x `justIf` True = Just x
_ `justIf` False = Nothing
