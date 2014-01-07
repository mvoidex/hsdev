module Main (
	main
	) where

import Control.Exception (finally)
import Control.Monad (void)
import Control.Monad.Error
import System.Console.GetOpt (OptDescr)
import System.Directory
import System.Environment (getArgs)
import Text.Read (readMaybe)

import HsDev.Tools.ClearImports (clearImports)
import HsDev.Symbols (locateSourceDir)

import System.Command

opts :: [OptDescr Opts]
opts = [
	option_ ['g'] "ghc" (req "ghc options") "options for GHC",
	option_ [] "hide-import-list" flag "hide import list",
	option_ [] "max-import-list" (req "max import list length") "hide long import lists"]

cmds :: [Command (IO ())]
cmds = addHelp "hsclearimports" id [
	cmd [] ["file"] "clear imports in haskell source" opts clear]
	where
		clear :: Opts -> [String] -> IO ()
		clear as [f] = do
			file <- canonicalizePath f
			mroot <- locateSourceDir file
			cur <- getCurrentDirectory
			flip finally (setCurrentDirectory cur) $ do
				maybe (return ()) setCurrentDirectory mroot
				void $ runErrorT $ catchError
					(clearImports (askOpts "ghc" as) file >>= mapM_ (liftIO . putStrLn . format as))
					(\e -> liftIO (putStrLn $ "Error: " ++ e))
		clear _ _ = putStrLn "Invalid arguments"
		
		format :: Opts -> (String, String) -> String
		format as (imp, lst)
			| hasOpt "hide-import-list" as = imp
			| maybe False (length lst >) (askOpt "max-import-list" as >>= readMaybe) = imp
			| otherwise = imp ++ " (" ++ lst ++ ")"

main :: IO ()
main = do
	args <- getArgs
	run cmds noCmd onError args
	where
		noCmd = putStrLn "Invalid command"
		onError = mapM_ putStrLn
