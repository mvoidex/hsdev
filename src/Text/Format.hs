{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

-- | Format module
--
-- >"My name is $, I am ${age} years old, I am from $" %~ ("Vasya" % ("age" %= 20) % "Moscow")
-- >"My name is Vasya, I am 20 years old"
module Text.Format (
	Format(..), FormatArgs, Hole(..),
	(%~), (~~), (%), (%=)
	) where

import Control.Arrow (first)
import Data.List (delete, isPrefixOf)
import Text.Regex.PCRE

class Format a where
	format :: a -> String

instance Format String where
	format = id
instance Format Int where
	format = show
instance Format Integer where
	format = show

type FormatArgs = [(Maybe String, String)]

class Hole a where
	hole :: a -> FormatArgs

instance Hole FormatArgs where
	hole = id

instance Format a => Hole a where
	hole v = [(Nothing, format v)]

instance Format a => Hole (String, a) where
	hole (n, v) = [(Just n, format v)]

instance Hole [(String, String)] where
	hole = map (first Just)

infixr 1 %~

(%~) :: Hole a => String -> a -> Either String String
fmt %~ hargs = case fmt =~ "\\$(\\{([a-zA-Z]+)\\})?" of
	(pre, "", "", []) -> Right pre
	(pre, _, post, []) -> Right $ pre ++ post
	(pre, _, post, gs) -> do
		let
			name = case gs of
				_:name':_ -> name'
				_ -> ""
		if null name && "$" `isPrefixOf` post
			then do
				post' <- tail post %~ hargs
				return $ pre ++ "$" ++ post'
			else do
				(arg', args') <- split' name
				post' <- post %~ args'
				return $ pre ++ arg' ++ post'
	where
		args = hole hargs

		split' :: String -> Either String (String, FormatArgs)
		split' n = maybe
			(Left $ maybe
				(concat [
					"Format: not enough arguments for format string '",
					fmt,
					"'"])
				("Format argument '$' not found" ~~) n')
			(\v -> Right (v, delete (n', v) args))
			(lookup n' args)
			where
				n'
					| null n = Nothing
					| otherwise = Just n

infixr 1 ~~

(~~) :: Hole a => String -> a -> String
fmt ~~ hargs = either error id $ fmt %~ hargs

infixr 5 %

(%) :: (Hole a, Hole b) => a -> b -> FormatArgs
x % y = hole x ++ hole y

infixr 1 %=

(%=) :: Format a => String -> a -> (String, String)
name %= value = (name, format value)
