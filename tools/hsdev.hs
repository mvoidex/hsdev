module Main (
	main
	) where

import Control.Monad
import Network.Socket
import System.Environment
import System.Exit
import System.IO

import System.Console.Cmd hiding (brief)
import qualified System.Console.Cmd as C (brief)

import Types
import Commands

main :: IO ()
main = withSocketsDo $ do
	hSetBuffering stdout LineBuffering
	hSetEncoding stdout utf8
	as <- getArgs
	when (null as) $ do
		printUsage
		exitSuccess
	let
		asr = if last as == "-?" then "help" : init as else as 
	run mainCommands onDef onError asr
	where
		onError :: String -> IO ()
		onError errs = do
			putStrLn errs
			exitFailure

		onDef :: IO ()
		onDef = do
			putStrLn "Unknown command"
			exitFailure

printUsage :: IO ()
printUsage = mapM_ (putStrLn . ('\t':) . ("hsdev " ++) . C.brief) mainCommands
