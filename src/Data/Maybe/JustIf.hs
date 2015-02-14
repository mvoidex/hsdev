module Data.Maybe.JustIf (
	justIf, justWhen, soJust
	) where

-- | Return @Just@ if True
justIf :: a -> Bool -> Maybe a
x `justIf` True = Just x
_ `justIf` False = Nothing

-- | Return @Just@ if @f x = True@
justWhen :: a -> (a -> Bool) -> Maybe a
x `justWhen` p = x `justIf` p x

-- | Flipped version of @justIf@
soJust :: Bool -> a -> Maybe a
soJust = flip justIf
