{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Main (
	main
	) where

import Control.Applicative
import Control.Monad (liftM)
import Control.Monad.IO.Class
import Data.Aeson (toJSON)
import System.FilePath (takeExtension)

import HsDev.Project (readProject)
import HsDev.Scan (scanModule)
import HsDev.Inspect (inspectContents)
import HsDev.Symbols.Location (ModuleLocation(..), Cabal(..))

import Tool

main :: IO ()
main = toolMain "hsinspect" [
	jsonCmd "" ["what"] [ghcOpts] "depending of what <what> is, inspect installed module, source file (.hs), cabal file (.cabal) or contents, passes as input if no <what> specified" inspect']
	where
		ghcOpts = list "ghc" "option" `short` ['g'] `desc` "options to pass to GHC"
		ghcs = listArg "ghc"

		inspect' (Args [] opts) = liftIO getContents >>= liftM toJSON . inspectContents "stdin" (ghcs opts)
		inspect' (Args [fname@(takeExtension -> ".hs")] opts) = toJSON <$> scanModule (ghcs opts) (FileModule fname Nothing)
		inspect' (Args [fcabal@(takeExtension -> ".cabal")] _) = toJSON <$> readProject fcabal
		inspect' (Args [mname] opts) = toJSON <$> scanModule (ghcs opts) (CabalModule Cabal Nothing mname)
		inspect' _ = toolError "Specify module name or file name (.hs or .cabal)"
