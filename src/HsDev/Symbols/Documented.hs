module HsDev.Symbols.Documented (
	Documented(..)
	) where

import HsDev.Symbols.Class

-- | Documented symbol
class Symbol a => Documented a where
	brief :: a -> String
	detailed :: a -> String
	detailed s = unlines $ header ++ docs ++ loc where
		header = [brief s, ""]
		docs = maybe [] return $ symbolDocs s
		loc = ["Defined at " ++ show (symbolLocation s)]
