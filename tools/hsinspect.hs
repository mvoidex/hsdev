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
import HsDev.Inspect (inspectContents, inspectDocs, getDefines)
import HsDev.Symbols.Location (ModuleLocation(..), Cabal(..))
import HsDev.Tools.GhcMod.InferType (infer)

import Tool

data Opts = Opts (Maybe String) [String]

opts :: Parser Opts
opts = Opts <$>
	optional (strArgument (metavar "what" <> help "depending of what <what> is, inspect installed module, source file (.hs), cabal file (.cabal) or contents, passes as input if no <what> specified")) <*>
	many (strOption (metavar "GHC_OPT" <> long "ghc" <> short 'g' <> help "options to pass to GHC"))

main :: IO ()
main = toolMain "hsinspect" "haskell inspect" opts (printExceptT . printResult . inspect') where
	inspect' (Opts Nothing ghcs) = do
		cts <- liftIO getContents
		defs <- liftIO getDefines
		liftM toJSON $ inspectContents "stdin" defs ghcs cts
	inspect' (Opts (Just fname@(takeExtension -> ".hs")) ghcs) = do
		fname' <- liftIO $ canonicalizePath fname
		defs <- liftIO getDefines
		im <- scanModule defs ghcs (FileModule fname' Nothing) Nothing
		let
			scanAdditional =
				scanModify (\opts' _ -> inspectDocs opts') >=>
				scanModify infer
		toJSON <$> scanAdditional im
	inspect' (Opts (Just fcabal@(takeExtension -> ".cabal")) _) = do
		fcabal' <- liftIO $ canonicalizePath fcabal
		toJSON <$> readProject fcabal'
	inspect' (Opts (Just mname) ghcs) = toJSON <$> scanModule [] ghcs (CabalModule Cabal Nothing mname) Nothing
