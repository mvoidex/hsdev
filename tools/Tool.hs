{-# LANGUAGE OverloadedStrings #-}

module Tool (
	-- * Tool
	toolMain, usage,
	-- * Json command
	jsonCmd, jsonCmd_,
	-- * Errors
	toolError,
	-- * Options
	prettyOpt, isPretty,

	module System.Console.Cmd
	) where

import Control.Monad.Error (ErrorT, runErrorT, throwError)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as L (ByteString, putStrLn)
import System.Environment
import System.IO

import HsDev.Tools.Base (ToolM)

import System.Console.Cmd

-- | Run tool with commands
toolMain :: String -> [Cmd (IO ())] -> IO ()
toolMain name commands = do
	hSetBuffering stdout LineBuffering
	hSetEncoding stdout utf8
	as <- getArgs
	case as of
		[] -> usage name toolCmds
		_ -> run toolCmds unknownCmd onError as
	where
		onError :: String -> IO ()
		onError = putStrLn

		unknownCmd :: IO ()
		unknownCmd = putStrLn "Unknown command" >> usage name toolCmds

		toolCmds = withHelp name (printWith putStrLn) commands

-- | Print usage
usage :: String -> [Cmd (IO ())] -> IO ()
usage toolName = mapM_ (putStrLn . ('\t':) . ((toolName ++ " ") ++) . brief)

-- | Command with JSONable result
jsonCmd :: ToJSON a => String -> [String] -> [Opt] -> String -> (Args -> ToolM a) -> Cmd (IO ())
jsonCmd name pos os descr act = cmd name pos (prettyOpt : os) descr $ \(Args as opts) -> do
	r <- runErrorT $ act (Args as opts)
	L.putStrLn $ either (toStr opts . errorStr) (toStr opts) r
	where
		toStr :: ToJSON a => Opts String -> a -> L.ByteString
		toStr opts
			| isPretty opts = encodePretty
			| otherwise = encode

		errorStr :: String -> Value
		errorStr s = object ["error" .= s]

-- | `jsonCmd` with the only '--pretty' option
jsonCmd_ :: ToJSON a => String -> [String] -> String -> ([String] -> ToolM a) -> Cmd (IO ())
jsonCmd_ name pos descr act = jsonCmd name pos [] descr (act . posArgs)

-- | Fail with error
toolError :: String -> ToolM a
toolError = throwError

prettyOpt :: Opt
prettyOpt = flag "pretty" `desc` "pretty JSON output"

isPretty :: Opts String -> Bool
isPretty = flagSet "pretty"
