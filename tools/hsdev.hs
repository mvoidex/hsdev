{-# LANGUAGE TemplateHaskell #-}

module Main (
	main
	) where

import Control.Exception
import Control.Monad
import Network.Socket (withSocketsDo)
import Options.Applicative
import System.Environment (getArgs)
import System.Exit
import System.IO
import Text.Read (readMaybe)

import Control.Apply.Util (chain)

import HsDev.Server.Commands (ServerCommand, runServerCommand)
import HsDev.Server.Types
import HsDev.Version

main :: IO ()
main = handle logErr' $ withSocketsDo $ do
	hSetBuffering stdout LineBuffering
	hSetEncoding stdout utf8
	as <- getArgs
	let
		asr = if not (null as) && head as == "help" then init as ++ ["-?"] else as
	case parseArgs "hsdev" (info cmdP (progDesc "hsdev tool")) asr of
		Left e -> putStrLn e >> exitFailure
		Right scmd -> runServerCommand scmd
	where
		logErr' (SomeException e) = putStrLn $ "exception " ++ show e
