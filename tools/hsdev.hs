{-# LANGUAGE TemplateHaskell #-}

module Main (
	main
	) where

import Control.Exception
import Network.Socket (withSocketsDo)
import Options.Applicative
import System.Environment (getArgs)
import System.Exit
import System.IO

import HsDev.Server.Commands (runServerCommand)
import HsDev.Server.Types
import HsDev.Util

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
