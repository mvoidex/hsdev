module Main (
	main
	) where

import Control.Monad (liftM)
import HsDev.Tools.Hayoo

import Tool

main :: IO ()
main = toolMain "hshayoo" [
	jsonCmd_ "" ["query"] "search in hayoo" hayoo']
	where
		hayoo' [q] = liftM (map hayooAsDeclaration . hayooFunctions) $ hayoo q
		hayoo' _ = toolError "Invalid arguments"
