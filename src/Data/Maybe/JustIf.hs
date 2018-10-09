module Data.Maybe.JustIf (
	justIf,
	whenJust, whenJustM
	) where

import Control.Monad

-- | Return @Just@ if True
justIf :: a -> Bool -> Maybe a
x `justIf` True = Just x
_ `justIf` False = Nothing

-- | Calls function when value is @Just@
whenJust :: Applicative m => Maybe a -> (a -> m b) -> m ()
whenJust v fn = maybe (pure ()) (void . fn) v

-- | Calls function when monad returns @Just@
whenJustM :: Monad m => m (Maybe a) -> (a -> m b) -> m ()
whenJustM mv fn = mv >>= (`whenJust` fn)
