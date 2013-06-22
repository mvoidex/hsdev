{-# LANGUAGE OverloadedStrings #-}

module Main (
	main
	) where

import Control.Exception (handle, SomeException, throwIO)
import Control.Monad
import Control.Monad.Error (runErrorT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L (unpack)
import Data.String (fromString)
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
		Option ['g'] ["ghc"] (ReqArg return "opt") "option to pass to ghc in ghc-mod and hdocs"] $ \ghc_opts as -> case as of
			[] -> putStrLn "Specify at least one module"
			modules -> do
				rs <- forM modules $ \m -> do
					r <- runErrorT (browse m >>= liftIO . loadDocs ghc_opts)
					return (fromString m .= either errorStr encodeModule r)
				putJSON $ object rs,
	cmd_ ["file"] [] "inspect file" $ \as -> case as of
		[] -> putStrLn "Specify at least one file"
		files -> do
			rs <- forM files $ \f -> do
				r <- runErrorT $ inspectFile f
				return (fromString f .= either errorStr encodeModule r)
			putJSON $ object rs,
	cmd_ ["cabal"] [] "inspect cabal" $ \as -> case as of
		[] -> putStrLn "Specify at least one file"
		cabals -> do
			cs <- forM cabals $ \c -> do
				r <- runErrorT $ readProject c
				return (fromString c .= either errorStr encodeProject r)
			putJSON $ object cs]

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
		handleError e = do
			putJSON . errorStr . show $ e
			throwIO e

putJSON :: Value -> IO ()
putJSON = putStrLn . L.unpack . encode

errorStr :: String -> Value
errorStr s = object ["error" .= s]

printUsage :: IO ()
printUsage = mapM_ (putStrLn . ('\t':) . ("hsinspect " ++) . brief) commands
