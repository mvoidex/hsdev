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

	module System.Console.Command
	) where

import Control.Monad.Error (ErrorT, runErrorT, throwError)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as L (ByteString, putStrLn)
import System.Console.GetOpt
import System.Environment
import System.IO

import HsDev.Tools.Base (ToolM)

import System.Console.Command

-- | Run tool with commands
toolMain :: String -> [Command (IO ())] -> IO ()
toolMain name commands = do
	hSetBuffering stdout LineBuffering
	hSetEncoding stdout utf8
	as <- getArgs
	case as of
		[] -> usage name toolCmds
		_ -> run toolCmds unknownCmd onError as
	where
		onError :: [String] -> IO ()
		onError = mapM_ putStrLn

		unknownCmd :: IO ()
		unknownCmd = putStrLn "Unknown command" >> usage name toolCmds

		toolCmds = addHelp name id commands

-- | Print usage
usage :: String -> [Command (IO ())] -> IO ()
usage toolName = mapM_ (putStrLn . ('\t':) . ((toolName ++ " ") ++) . brief)

-- | Command with JSONable result
jsonCmd :: ToJSON a => [String] -> [String] -> String -> [OptDescr (Opts String)] -> (Opts String -> [String] -> ToolM a) -> Command (IO ())
jsonCmd name posArgs descr as act = cmd name posArgs descr (prettyOpt : as) $ \opts as -> do
	r <- runErrorT $ act opts as
	L.putStrLn $ either (toStr opts . errorStr) (toStr opts) r
	where
		toStr :: ToJSON a => Opts String -> a -> L.ByteString
		toStr opts
			| isPretty opts = encodePretty
			| otherwise = encode

		errorStr :: String -> Value
		errorStr s = object ["error" .= s]

-- | `jsonCmd` with the only '--pretty' option
jsonCmd_ :: ToJSON a => [String] -> [String] -> String -> ([String] -> ToolM a) -> Command (IO ())
jsonCmd_ name posArgs descr = jsonCmd name posArgs descr [] . const

-- | Fail with error
toolError :: String -> ToolM a
toolError = throwError

prettyOpt :: OptDescr (Opts String)
prettyOpt = option_ [] "pretty" no "pretty JSON output"

isPretty :: Opts String -> Bool
isPretty = flag "pretty"
