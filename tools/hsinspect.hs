{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Main (
	main
	) where

import Control.Monad (liftM, (>=>))
import Control.Monad.IO.Class
import Data.Aeson (toJSON)
import System.Directory (canonicalizePath)
import System.FilePath (takeExtension)

import HsDev.Project (readProject)
import HsDev.Scan (scanModule, scanModify)
import HsDev.Inspect (inspectContents, inspectDocs)
import HsDev.Symbols.Location (ModuleLocation(..), Cabal(..))
import HsDev.Tools.GhcMod.InferType (infer)

import Tool

main :: IO ()
main = toolMain "hsinspect" [
	jsonCmd "" ["what"] [ghcOpts] "depending of what <what> is, inspect installed module, source file (.hs), cabal file (.cabal) or contents, passes as input if no <what> specified" inspect']
	where
		ghcOpts = list "ghc" "option" `short` ['g'] `desc` "options to pass to GHC"
		ghcs = listArg "ghc"

		inspect' (Args [] opts) = liftIO getContents >>= liftM toJSON . inspectContents "stdin" (ghcs opts)
		inspect' (Args [fname@(takeExtension -> ".hs")] opts) = do
			fname' <- liftIO $ canonicalizePath fname
			im <- scanModule (ghcs opts) (FileModule fname' Nothing) Nothing
			let
				scanAdditional =
					scanModify (\opts' _ -> inspectDocs opts') >=>
					scanModify infer
			toJSON <$> scanAdditional im
		inspect' (Args [fcabal@(takeExtension -> ".cabal")] _) = do
			fcabal' <- liftIO $ canonicalizePath fcabal
			toJSON <$> readProject fcabal'
		inspect' (Args [mname] opts) = toJSON <$> scanModule (ghcs opts) (CabalModule Cabal Nothing mname) Nothing
		inspect' _ = toolError "Specify module name or file name (.hs or .cabal)"
