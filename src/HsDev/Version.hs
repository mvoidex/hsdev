{-# LANGUAGE TemplateHaskell #-}

module HsDev.Version (
	cabalVersion
	) where

import Data.Char
import Data.List
import Data.Maybe
import Language.Haskell.TH

cabalVersion :: ExpQ
cabalVersion = do
	s <- runIO (readFile "hsdev.cabal")
	let
		version = listToMaybe $ map (dropWhile isSpace) $ mapMaybe (stripPrefix "version:") $ lines s
	maybe (fail "Can't detect version") (\v -> [e| v |]) version
