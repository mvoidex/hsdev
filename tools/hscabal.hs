module Main (
	main
	) where

import HsDev.Tools.Cabal

import Tool

main :: IO ()
main = toolMain "hscabal" [
	jsonCmd_ "list" ["packages..."] "list hackage packages" cabalList]
