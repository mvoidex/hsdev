{-# LANGUAGE OverloadedStrings #-}

module Main (
	main
	) where

import Control.Monad.IO.Class
import HsDev.Project (readProject)
import HsDev.Scan (scanModule)
import HsDev.Inspect (inspectContents)
import HsDev.Symbols.Location (ModuleLocation(..), Cabal(..))

import Tool

main :: IO ()
main = toolMain "hsinspect" [
	jsonCmd "module" ["module name"] [ghcOpts] "inspect installed module" inspectModule',
	jsonCmd "file" ["source file"] [ghcOpts] "inspect file" inspectFile',
	jsonCmd "input" [] [ghcOpts] "inspect input" inspectInput',
	jsonCmd_ "cabal" ["project file"] "inspect .cabal file" inspectCabal']
	where
		ghcOpts = list "ghc" "option" `short` ['g'] `desc` "options to pass to GHC"
		ghcs = listArg "ghc"

		inspectModule' (Args [mname] opts) = scanModule (ghcs opts) (CabalModule Cabal Nothing mname)
		inspectModule' _ = toolError "Specify module name"

		inspectFile' (Args [fname] opts) = scanModule (ghcs opts) (FileModule fname Nothing)
		inspectFile' _ = toolError "Specify source file name"

		inspectInput' (Args [] opts) = liftIO getContents >>= inspectContents "stdin" (ghcs opts)
		inspectInput' _ = toolError "Invalid arguments"

		inspectCabal' [fcabal] = readProject fcabal
		inspectCabal' _ = toolError "Specify project .cabal file"
