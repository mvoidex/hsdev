module Main (
	main
	) where

import Control.Monad
import Control.Monad.Error
import Control.Monad.IO.Class
import Data.Monoid
import Data.List
import qualified Data.Map as M
import System.Args
import System.Environment
import System.Directory (canonicalizePath)
import System.IO

import HsDev.Scan as HsDev
import HsDev.Symbols as HsDev
import HsDev.Util as HsDev
import HsDev.Database as HsDev
import HsDev.Commands as HsDev

main :: IO ()
main = do
	hSetEncoding stdout utf8
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
			r <- runAction $ dispatch cmd (liftIO (putStrLn "Invalid arguments") >> return mempty) [
				("cabal", do
					let
						cabalPath = force $ arg "cabal" cmd
					HsDev.scanCabal (if null cabalPath then HsDev.Cabal else HsDev.CabalDev cabalPath)),
				("file", do
					let
						file = force $ arg "file" cmd
					HsDev.scanFile file),
				("project", do
					let
						proj = force $ arg "project" cmd
					proj' <- liftIO $ locateProject proj
					maybe (throwError $ "Project " ++ proj ++ " not found") HsDev.scanProject proj')]
			run (mappend db r)
		Right "goto" -> do
			rs <- runAction (HsDev.goToDeclaration db (force $ argN 1 cmd) (try $ arg "qualified" cmd) (force $ argN 2 cmd))
			print rs
			run db
		Right "complete" -> do
			file <- canonicalizePath $ force $ argN 1 cmd
			rs <- runAction (HsDev.completions db file (try $ arg "qualified" cmd) (force $ argN 2 cmd))
			mapM_ putStrLn rs
			run db
		Right "dump" -> do
			dispatch cmd (putStrLn "Invalid arguments") [
				("files", do
					forM_ (M.assocs $ M.map symbolName $ HsDev.databaseFiles db) $ \(fname, mname) ->
						putStrLn $ mname ++ " in " ++ fname),
				("file", do
					file <- canonicalizePath $ force $ arg "file" cmd
					maybe (putStrLn "File not found") print $ M.lookup file (HsDev.databaseFiles db))]
			run db
		Right "dump" -> do
			forM_ (M.assocs $ M.map symbolName $ HsDev.databaseFiles db) $ \(fname, mname) ->
				putStrLn $ mname ++ " in " ++ fname
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
	"\tcomplete file [-qualified prefix] input -- autocompletion",
	"\tdump -files -- dump file names, that are loaded in database",
	"\tdump -file filename -- dump contents of file",
	"\thelp -- this command",
	"\texit -- exit"]

dispatch :: Args -> a -> [(String, a)] -> a
dispatch cmds def vars = maybe def snd $ find (\v -> either (const False) (const True) (arg (fst v) cmds)) vars
