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
	jsonCmd "module" ["module name"] [ghcOpts] "inspect installed module" inspectModule',
	jsonCmd "file" ["source file"] [ghcOpts] "inspect file" inspectFile',
	jsonCmd_ "cabal" ["project file"] "inspect .cabal file" inspectCabal']
	where
		ghcOpts = list "ghc" "option" `short` ['g'] `desc` "options to pass to GHC"
		ghcs = listArg "ghc"

		inspectModule' (Args [mname] opts) = scanModule (ghcs opts) (CabalModule Cabal Nothing mname)
		inspectModule' _ = toolError "Specify module name"

		inspectFile' (Args [fname] opts) = scanModule (ghcs opts) (FileModule fname Nothing)
		inspectFile' _ = toolError "Specify source file name"

		inspectCabal' [fcabal] = readProject fcabal
		inspectCabal' _ = toolError "Specify project .cabal file"
