module Main (
	main
	) where

import Control.Arrow
import Control.Monad
import Control.Monad.Error
import Control.Monad.IO.Class
import Data.Char
import Data.Monoid
import System.Args
import System.Environment

import HsDev.Scan as HsDev
import HsDev.Symbols as HsDev
import HsDev.Util as HsDev

main :: IO ()
main = do
	args' <- getArgs
	let
		params = args [] args'
	case argN 0 params of
		Right "help" -> printUsage
		Right _ -> putStrLn "Unknown command" >> printUsage
		Left _ -> run

run :: IO ()
run = do
	cmd <- liftM (args [] . split) getLine
	case argN 0 cmd of
		Left _ -> run
		Right "exit" -> return ()
		Right "scan" -> do
			let
				action = msum [
					do
						cabalPath <- arg "cabal" cmd
						return (HsDev.scanCabal (if null cabalPath then HsDev.Cabal else HsDev.CabalDev cabalPath)),
					do
						file <- arg "file" cmd
						return (HsDev.scanFile file),
					do
						proj <- arg "project" cmd
						return (do
							proj' <- liftIO $ locateProject proj
							maybe (throwError $ "Project " ++ proj ++ " not found") HsDev.scanProject proj')]
			either (const $ putStrLn "Invalid arguments") runAction action
			run
		Right _ -> putStrLn "Unknown command" >> run

runAction :: ErrorT String IO a -> IO ()
runAction act = runErrorT act >>= either (putStrLn . ("Error: " ++)) (const $ putStrLn "Ok")

printUsage :: IO ()
printUsage = mapM_ putStrLn [
	"hsdev",
	"",
	"run hsdev and input commands:",
	"\tscan -cabal [path-to-cabal-dev] -- scan modules installed in cabal or cabal-dev sandbox",
	"\tscan -file file -- scan source file",
	"\tscan -project path-or-cabal-file -- scan project (.cabal and all source files)",
	"\texit -- exit"]

split :: String -> [String]
split "" = []
split (c:cs)
	| isSpace c = split cs
	| c == '"' = let (w, cs') = readQuote cs in w : split cs'
	| otherwise = let (ws, tl) = break isSpace cs in (c:ws) : split tl
	where
		readQuote :: String -> (String, String)
		readQuote "" = ("", "")
		readQuote ('\\':ss)
			| null ss = ("\\", "")
			| otherwise = first (head ss :) $ readQuote (tail ss)
		readQuote ('"':ss) = ("", ss)
		readQuote (s:ss) = first (s:) $ readQuote ss
