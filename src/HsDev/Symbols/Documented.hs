module HsDev.Symbols.Documented (
	Documented(..),
	defaultDetailed
	) where

import Data.Text (unpack)

import HsDev.Symbols.Class

-- | Documented symbol
class Symbol a => Documented a where
	brief :: a -> String
	detailed :: a -> String
	detailed = unlines . defaultDetailed

-- | Default detailed docs
defaultDetailed :: Documented a => a -> [String]
defaultDetailed s = header ++ docs ++ loc where
	header = [brief s, ""]
	docs = maybe [] (return . unpack) $ symbolDocs s
	loc
		| null mloc = []
		| otherwise = ["Defined at " ++ mloc]
	mloc = show (symbolLocation s)
