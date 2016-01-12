module Main (
	main
	) where

import Control.Exception
import Data.List
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
	as <- prepareArgs
	case parseArgs "hsdev" (info cmdP (progDesc "hsdev tool")) as of
		Left e -> putStrLn e >> exitFailure
		Right scmd -> runServerCommand scmd
	where
		logErr' (SomeException e) = putStrLn $ "exception " ++ show e

-- | Get args with '--stdin' flag replaced and 'help' command changed to flag
prepareArgs :: IO [String]
prepareArgs = getArgs >>= prepare where
	prepare [] = return []
	prepare ("head" : as) = prepare $ as ++ ["-?"]
	prepare as
		| "--stdin" `elem` as = do
			input <- getContents
			return $ delete "--stdin" as ++ ["--data", input]
		| otherwise = return as
