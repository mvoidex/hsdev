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
import HsDev.Database as HsDev
import HsDev.Commands as HsDev

main :: IO ()
main = do
	args' <- getArgs
	let
		params = args [] args'
	case argN 0 params of
		Right "help" -> printUsage
		Right _ -> putStrLn "Unknown command" >> printUsage
		Left _ -> run mempty

run :: HsDev.Database -> IO ()
run db = do
	cmd <- liftM (args [] . split) getLine
	case argN 0 cmd of
		Left _ -> run db
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
			r <- either (\_ -> putStrLn "Invalid arguments" >> return mempty) runAction action
			run (mappend db r)
		Right "goto" -> do
			rs <- runAction (HsDev.goToDeclaration db (force $ argN 1 cmd) (try $ arg "qualified" cmd) (force $ argN 2 cmd))
			print rs
			run db
		Right "help" -> printUsage >> run db
		Right _ -> putStrLn "Unknown command" >> run db

runAction :: Monoid a => ErrorT String IO a -> IO a
runAction act = runErrorT act >>= either onError onOk where
	onError msg = do
		putStrLn $ "Error: " ++ msg
		return mempty
	onOk r = do
		putStrLn "Ok"
		return r

printUsage :: IO ()
printUsage = mapM_ putStrLn [
	"hsdev",
	"",
	"run hsdev and input commands:",
	"\tscan -cabal [path-to-cabal-dev] -- scan modules installed in cabal or cabal-dev sandbox",
	"\tscan -file file -- scan source file",
	"\tscan -project path-or-cabal-file -- scan project (.cabal and all source files)",
	"\tgoto file [-qualified prefix] name -- go to declaration from file specified",
	"\thelp -- this command",
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
