module Main (
	main
	) where

import HsDev.Tools.Cabal

import Tool

main :: IO ()
main = toolMain "hscabal" "cabal tool" (many (strArgument (metavar "package"))) $ printExceptT . printResult . cabalList
