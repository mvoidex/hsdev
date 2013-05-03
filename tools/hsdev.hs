module Main (
	main
	) where

import Control.Exception (catch, SomeException)
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Data.Maybe
import Data.Monoid
import Data.List
import qualified Data.Map as M
import System.Args
import System.Environment
import System.Directory (canonicalizePath)
import System.IO

import HsDev.Cache as HsDev
import HsDev.Commands as HsDev
import HsDev.Database as HsDev
import HsDev.Project as HsDev
import HsDev.Scan as HsDev
import HsDev.Symbols as HsDev
import HsDev.Util as HsDev

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
run db = flip catch onError $ do
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
		Right "cache" -> do
			db' <- cache cmd db
			run (mappend db db')
		Right "help" -> printUsage >> run db
		Right _ -> putStrLn "Unknown command" >> run db
	where
		onError :: SomeException -> IO ()
		onError e = do
			putStrLn $ "Exception: " ++ show e
			run db

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
	"\tcache -dump -cabal [path-to-cabal-dev] -- dump cache of cabal packages",
	"\tcache -dump -project project-cabal -- dump cache of project",
	"\tcache -load -cabal [path-to-cabal-dev] -- load cache of cabal packages",
	"\tcache -load -project project-cabal -- load cache of project",
	"\tcache -load -file cache-file -- load any cache",
	"\thelp -- this command",
	"\texit -- exit"]

dispatch :: Args -> a -> [(String, a)] -> a
dispatch cmds def vars = maybe def snd $ find (\v -> either (const False) (const True) (arg (fst v) cmds)) vars

cache :: Args -> Database -> IO Database
cache as db
	| exists (arg "dump" as) = do
		r <- runMaybeT $ msum [cabalSave, projectSave]
		maybe (putStrLn "Invalid arguments") (const $ putStrLn "Ok") r
		return mempty
	| exists (arg "load" as) = do
		r <- runMaybeT $ msum [cabalLoad, projectLoad]
		maybe (putStrLn "Invalid arguments" >> return mempty) return r
	| otherwise = putStrLn "Invalid arguments" >> return mempty
	where
		cabalSave = do
			cabal <- cabalArg
			liftIO $ HsDev.dump (HsDev.cabalCache cabal) (HsDev.cabalModules cabal db)
		projectSave = do
			proj <- projectArg
			liftIO $ HsDev.dump (HsDev.projectCache proj) (HsDev.projectModules proj db)
		cabalLoad = do
			cabal <- cabalArg
			liftIO $ fmap HsDev.toDatabase $ HsDev.load (HsDev.cabalCache cabal)
		projectLoad = do
			proj <- projectArg
			liftIO $ fmap HsDev.toDatabase $ HsDev.load (HsDev.projectCache proj)
		cabalArg = do
			c <- MaybeT $ return $ try $ arg "cabal" as
			if null c
				then return HsDev.Cabal
				else do
					c' <- liftIO $ canonicalizePath c
					return $ HsDev.CabalDev c'
		projectArg = fmap (HsDev.project) $ (MaybeT $ return $ try $ arg "project" as) >>= liftIO . canonicalizePath
