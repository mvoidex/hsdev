{-# LANGUAGE TemplateHaskell #-}

module Main (
	main
	) where

import Control.Applicative
import Control.Monad
import Network.Socket (withSocketsDo)
import System.Environment (getArgs)
import System.Exit
import System.IO
import Text.Read (readMaybe)

import Control.Apply.Util (chain)
import System.Console.Cmd
import qualified System.Console.Cmd as C (brief)

import qualified HsDev.Client.Commands as Client (commands)
import qualified HsDev.Server.Commands as Server
import HsDev.Version

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

mainCommands :: [Cmd (IO ())]
mainCommands = withHelp "hsdev" (printWith putStrLn) $ concat [
	[cmd "version" [] [] "hsdev version" version'],
	map (chain [validateOpts, noArgs]) Server.commands,
	map Server.clientCmd Client.commands]
	where
		version' _ = putStrLn $cabalVersion

printUsage :: IO ()
printUsage = mapM_ (putStrLn . ('\t':) . ("hsdev " ++) . C.brief) mainCommands

-- | Check that specified options are numbers
validateNums :: [String] -> Cmd a -> Cmd a
validateNums ns = validateArgs (check . namedArgs) where
	check os = forM_ ns $ \n -> case (readMaybe :: String -> Maybe Int) <$> arg n os of
		Just Nothing -> failMatch "Must be a number"
		_ -> return ()

-- | Check, that 'port' and 'timeout' are numbers
validateOpts :: Cmd a -> Cmd a
validateOpts = validateNums ["port", "timeout"]

-- | Ensure no positional arguments provided
noArgs :: Cmd a -> Cmd a
noArgs = validateArgs (noPos . posArgs) where
	noPos ps =
		guard (null ps)
		`mplus`
		failMatch "positional arguments are not expected"
