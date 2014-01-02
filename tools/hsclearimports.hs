module Main (
	main
	) where

import Control.Monad
import Control.Monad.Error
import System.Console.GetOpt
import System.Environment

import HsDev.Tools.ClearImports

opts :: [OptDescr [String]]
opts = [
	Option ['g'] ["ghc"] (ReqArg return "ghc_opt") "option to pass to GHC"]

main :: IO ()
main = do
	args <- getArgs
	let
		(ghcOpts, args', _) = getOpt Permute opts args
	case args' of
		[f] -> void $ runErrorT $
			catchError
				(clearImports (concat ghcOpts) f >>= mapM_ (liftIO . putStrLn))
				(\e -> liftIO (putStrLn $ "Error: " ++ e))
		_ -> putStrLn $ usageInfo "hsclearimports file" opts
