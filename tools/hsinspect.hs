{-# LANGUAGE OverloadedStrings #-}

module Main (
	main
	) where

import HsDev.Project (readProject)
import HsDev.Scan (scanModule)
import HsDev.Symbols.Location (ModuleLocation(..), Cabal(..))

import Tool

main :: IO ()
main = toolMain "hsinspect" [
	jsonCmd ["module"] ["module name"] "inspect installed module" [ghcOpts] inspectModule',
	jsonCmd ["file"] ["source file"] "inspect file" [ghcOpts] inspectFile',
	jsonCmd_ ["cabal"] ["project file"] "inspect .cabal file" inspectCabal']
	where
		ghcOpts = option_ ['g'] "ghc" (req "ghc options") "options to pass to GHC"
		ghcs = list "ghc"

		inspectModule' opts [mname] = scanModule (ghcs opts) (CabalModule Cabal Nothing mname)
		inspectModule' _ _ = toolError "Specify module name"

		inspectFile' opts [fname] = scanModule (ghcs opts) (FileModule fname Nothing)
		inspectFile' _ _ = toolError "Specify source file name"

		inspectCabal' [fcabal] = readProject fcabal
		inspectCabal' _ = toolError "Specify project .cabal file"
