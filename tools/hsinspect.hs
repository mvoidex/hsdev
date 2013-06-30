{-# LANGUAGE OverloadedStrings #-}

module Main (
	main
	) where

import Control.Exception (handle, SomeException)
import Control.Monad ((>=>))
import Control.Monad.Error (ErrorT, runErrorT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L (unpack)
import System.Environment (getArgs)
import System.IO
import System.Console.GetOpt

import System.Command

import HsDev.Inspect (inspectFile)
import HsDev.Project (readProject)
import HsDev.Tools.GhcMod (browse)
import HsDev.Tools.HDocs (loadDocs)
import HsDev.Symbols.JSON (encodeModule)
import HsDev.Project.JSON (encodeProject)

commands :: [Command (IO ())]
commands = addhelp "hsinspect" id printUsage [
	cmd ["module"] [] "inspect installed module" [
		Option ['g'] ["ghc"] (ReqArg return "opt") "option to pass to GHC"] $
			\ghc_opts as -> oneArg as encodeModule (browse ghc_opts >=> (liftIO . loadDocs ghc_opts)),
	cmd ["file"] [] "inspect file" [
		Option ['g'] ["ghc"] (ReqArg return "opt") "option to pass to GHC"] $
			\ghc_opts as -> oneArg as encodeModule (inspectFile ghc_opts),
	cmd_ ["cabal"] [] "inspect cabal" $ \as -> oneArg as encodeProject readProject]
	where
		oneArg :: [String] -> (a -> Value) -> (String -> ErrorT String IO a) -> IO ()
		oneArg [] _ _ = putJSON $ errorStr "Not enough arguments"
		oneArg [s] toj onArg = do
			r <- runErrorT $ onArg s
			putJSON $ either errorStr toj r
		oneArg _ _ _ = putJSON $ errorStr "Too much arguments"

main :: IO ()
main = handle handleError $ do
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

		handleError :: SomeException -> IO ()
		handleError = putJSON . errorStr . show

putJSON :: Value -> IO ()
putJSON = putStrLn . L.unpack . encode

errorStr :: String -> Value
errorStr s = object ["error" .= s]

printUsage :: IO ()
printUsage = mapM_ (putStrLn . ('\t':) . ("hsinspect " ++) . brief) commands
