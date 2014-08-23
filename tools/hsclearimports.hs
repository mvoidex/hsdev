module Main (
	main
	) where

import Control.Exception (finally)
import Control.Monad (void)
import Control.Monad.Error
import System.Directory
import System.Environment (getArgs)
import Text.Read (readMaybe)

import HsDev.Tools.ClearImports (clearImports)
import HsDev.Symbols (locateSourceDir)

import System.Console.Cmd

opts :: [Opt]
opts = [
	list "ghc" "GHC_OPT" `short` "g" `desc` "options for GHC",
	flag "hide-import-list" `desc` "hide import list",
	req "max-import-list" "N" `desc` "hide long import lists"]

cmds :: [Cmd (IO ())]
cmds = withHelp "hsclearimports" (printWith putStrLn) $ [
	cmd [] ["file"] opts "clear imports in haskell source" clear]
	where
		clear :: Args -> IO ()
		clear (Args [f] as) = do
			file <- canonicalizePath f
			mroot <- locateSourceDir file
			cur <- getCurrentDirectory
			flip finally (setCurrentDirectory cur) $ do
				maybe (return ()) setCurrentDirectory mroot
				void $ runErrorT $ catchError
					(clearImports (listArg "ghc" as) file >>= mapM_ (liftIO . putStrLn . format as))
					(\e -> liftIO (putStrLn $ "Error: " ++ e))
		clear _ = putStrLn "Invalid arguments"

		format :: Opts String -> (String, String) -> String
		format as (imp, lst)
			| flagSet "hide-import-list" as = imp
			| maybe False (length lst >) (narg "max-import-list" as) = imp
			| otherwise = imp ++ " (" ++ lst ++ ")"

main :: IO ()
main = do
	args <- getArgs
	run cmds noCmd onError args
	where
		noCmd = putStrLn "Invalid command"
		onError = putStrLn
