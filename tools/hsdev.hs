module Main (
	main
	) where

import Control.Monad
import Network.Socket
import System.Environment
import System.Exit
import System.IO

import System.Console.Command hiding (brief)
import qualified System.Console.Command as C (brief)

import Types
import Commands

main :: IO ()
main = withSocketsDo $ do
	hSetBuffering stdout LineBuffering
	hSetEncoding stdout utf8
	as <- getArgs
	when (null as) $ do
		printMainUsage
		exitSuccess
	let
		asr = if last as == "-?" then "help" : init as else as 
	run mainCommands onDef onError asr
	where
		onError :: [String] -> IO ()
		onError errs = do
			mapM_ putStrLn errs
			exitFailure

		onDef :: IO ()
		onDef = do
			putStrLn "Unknown command"
			exitFailure

printMainUsage :: IO ()
printMainUsage = do
	mapM_ (putStrLn . ('\t':) . ("hsdev " ++) . C.brief) mainCommands
	putStrLn "\thsdev [--port=number] [--pretty] [--stdin] interactive command... -- send command to server, use flag stdin to pass data argument through stdin"

printUsage :: IO ()
printUsage = mapM_ (putStrLn . ('\t':) . ("hsdev " ++) . C.brief) commands
