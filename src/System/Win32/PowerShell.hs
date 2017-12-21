{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase #-}

module System.Win32.PowerShell (
	-- * Escape functions
	translate, translateArg,
	quote, quoteDouble,
	escape
	) where

import Data.Char (isAlphaNum)

translate :: String -> String
translate s
	| all (\ch -> isAlphaNum ch || ch `elem` "-_") s = s
	| otherwise = '"' : snd (foldr escape' (True, "\"") s)
	where
		escape' '"' (_, s') = (True, '\\' : '"' : s')
		escape' '\\' (True, s') = (True, '\\' : '\\' : s')
		escape' '\\' (False, s') = (False, '\\' : s')
		escape' c (_, s') = (False, c : s')

translateArg :: String -> String
translateArg s
	| all isAlphaNum s = s
	| otherwise = "'" ++ translate s ++ "'"

quote :: String -> String
quote s = "'" ++ concatMap (\case { '\'' -> "''"; ch -> [ch] }) s ++ "'"

quoteDouble :: String -> String
quoteDouble s = "\"" ++ concatMap (\case { '"' -> "\"\""; ch -> [ch] }) s ++ "\""

escape :: (String -> String) -> String -> String
escape fn s
	| all (\ch -> isAlphaNum ch || ch `elem` "-_") s = s
	| otherwise = fn s
