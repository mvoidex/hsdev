{-# LANGUAGE OverloadedStrings #-}

module Main (
	main
	) where

import Control.Monad.Error (ErrorT, runErrorT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as L (unpack)
import System.Environment (getArgs)
import System.IO
import System.Console.GetOpt

import System.Command

import HsDev.Project (readProject)
import HsDev.Scan (scanModule)
import HsDev.Symbols.Location (ModuleLocation(..), Cabal(..))
import HsDev.Symbols.JSON ()

commands :: [Command (IO ())]
commands = addHelp "hsinspect" id [
	cmd ["module"] ["module name"] "inspect installed module" [ghcOpts, prettyOpt] inspectModule',
	cmd ["file"] ["source file"] "inspect file" [ghcOpts, prettyOpt] inspectFile',
	cmd ["cabal"] ["project file"] "inspect .cabal file" [prettyOpt] inspectCabal']
	where
		ghcOpts = Option ['g'] ["ghc"] (ReqArg (opt "ghc") "ghc options") "options to pass to GHC"
		prettyOpt = Option [] ["pretty"] (NoArg $ opt "pretty" "") "pretty JSON output"

		isPretty = hasOpt "pretty"
		ghcs = askOpts "ghc"

		output :: ToJSON a => Opts -> ErrorT String IO a -> IO ()
		output as act = do
			r <- runErrorT act
			putStrLn $ either (toStr . errorStr) toStr r
			where
				toStr :: ToJSON a => a -> String
				toStr
					| isPretty as = L.unpack . encodePretty
					| otherwise = L.unpack . encode

		err :: String -> ErrorT String IO ()
		err = throwError

		inspectModule' as [mname] = output as $ scanModule (ghcs as) (CabalModule Cabal Nothing mname)
		inspectModule' as _ = output as $ err "Specify module name"

		inspectFile' as [fname] = output as $ scanModule (ghcs as) (FileModule fname Nothing)
		inspectFile' as _ = output as $ err "Specify source file name"

		inspectCabal' as [fcabal] = output as $ readProject fcabal
		inspectCabal' as _ = output as $ err "Specify project .cabal file"

main :: IO ()
main = do
	hSetBuffering stdout LineBuffering
	hSetEncoding stdout utf8
	as <- getArgs
	case as of
		[] -> printUsage
		_ -> run commands onUnknown onError as
	where
		onError :: [String] -> IO ()
		onError = mapM_ putStrLn

		onUnknown :: IO ()
		onUnknown = do
			putStrLn "Unknown command"
			printUsage

errorStr :: String -> Value
errorStr s = object ["error" .= s]

printUsage :: IO ()
printUsage = mapM_ (putStrLn . ('\t':) . ("hsinspect " ++) . brief) commands
