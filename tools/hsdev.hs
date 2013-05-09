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
import HsDev.Symbols.Util as HsDev
import HsDev.Util as HsDev

main :: IO ()
main = do
	hSetEncoding stdout utf8
	args' <- getArgs
	case args' of
		["help"] -> printUsage
		[] -> run mempty
		_ -> putStrLn "Unknown command" >> printUsage

commands :: [Command]
commands = [
	cmd "scan" "scan modules installed in cabal" [
		"-cabal [sandbox]" $= "path to sandbox"],
	cmd "scan" "scan project" [
		"-project p" $= "path to project .cabal file"],
	cmd "scan" "scan file" [
		"-file f" $= "file to scan"],
	cmd "find" "find symbol" [
		"name" $= "",
		"[-project p]" $= "symbol in project",
		"[-file [f]]" $= "symbol in file",
		"[-module m]" $= "module of symbol",
		"[-cabal [sandbox]]" $= "sandbox of module, where symbol defined"],
	cmd "goto" "find symbol declaration" [
		"name" $= "",
		"[-file f]" $= "context source file"],
	cmd "info" "get info for symbol" [
		"name" $= "",
		"[-file f]" $= "context source file"],
	cmd "complete" "autocompletion" [
		"file" $= "context source file",
		"input" $= "string to complete"],
	cmd "dump" "dump file names, that are loaded in database" ["-files" $= ""],
	cmd "dump" "dump module contents" ["-file f" $= "file to dump"],
	cmd "cache" "dump cache of cabal packages" ["-dump" $= "", "-cabal [sandbox]" $= ""],
	cmd "cache" "dump cache of project" ["-dump" $= "", "-project p" $= ""],
	cmd "cache" "load cache of cabal packages" ["-load" $= "", "-cabal [sandbox]" $= ""],
	cmd "cache" "load cache of project" ["-load" $= "", "-project p" $= ""],
	cmd "cache" "load cache from file" ["-load" $= "", "-file cache-file" $= ""],
	cmd "help" "this command" [],
	cmd "exit" "exit" []]

run :: HsDev.Database -> IO ()
run db = flip catch onError $ do
	cmd <- liftM split getLine
	case parseCommand commands cmd of
		Left e -> putStrLn e >> run db
		Right (name, cmdArgs) -> case name of
			"exit" -> return ()
			"help" -> printUsage >> run db
			"scan" -> do
				r <- runAction $ runMaybeT $ msum [
					do
						cabalPath <- MaybeT $ return $ arg "cabal" cmdArgs
						lift $ scanCabal (if null cabalPath then HsDev.Cabal else HsDev.CabalDev cabalPath),
					do
						file <- MaybeT $ return $ arg "file" cmdArgs
						lift $ scanFile file,
					do
						proj <- MaybeT $ return $ arg "project" cmdArgs
						proj' <- liftIO $ locateProject proj
						maybe (throwError $ "Project " ++ proj ++ " not found") (lift . scanProject) proj']
				run $ maybe db (mappend db) r
			"find" -> do
				rs <- runAction (findSymbol db (fromJust $ at 0 cmdArgs))
				file' <- maybe (return Nothing) (\f -> if null f then return Nothing else fmap Just (canonicalizePath f)) $ arg "file" cmdArgs
				let
					filters :: Symbol a -> Bool
					filters = satisfy $ catMaybes [
						fmap (inProject . project) $ arg "project" cmdArgs,
						fmap inFile file',
						if has "file" cmdArgs then Just bySources else Nothing,
						fmap inModule $ arg "module" cmdArgs,
						fmap (inCabal . asCabal) $ arg "cabal" cmdArgs]
					asCabal "" = Cabal
					asCabal p = CabalDev p
				printResults $ filter (filterResult filters) rs
				run db
			"goto" -> do
				rs <- runAction (goToDeclaration db (arg "file" cmdArgs) (fromJust $ at 0 cmdArgs))
				printResults rs
				run db
			"info" -> do
				str <- runAction (symbolInfo db (arg "file" cmdArgs) (fromJust $ at 0 cmdArgs))
				putStrLn str
				run db
			"complete" -> do
				file <- canonicalizePath $ fromJust $ at 0 cmdArgs
				rs <- runAction (completions db file (fromJust $ at 1 cmdArgs))
				mapM_ putStrLn rs
				run db
			"dump" -> do
				runMaybeT $ msum [
					when (has "files" cmdArgs) $ do
						forM_  (M.assocs $ M.map symbolName $ databaseFiles db) $ \(fname, mname) ->
							liftIO (putStrLn (mname ++ " in " ++ fname)),
					do
						file <- MaybeT $ return $ arg "file" cmdArgs
						maybe (liftIO $ putStrLn "File not found") (liftIO . print) $ M.lookup file (databaseFiles db)]
				run db
			"cache" -> do
				db' <- cache cmdArgs db
				run (mappend db db')
			_ -> putStrLn "Unknown command" >> run db
	where
		onError :: SomeException -> IO ()
		onError e = do
			putStrLn $ "Exception: " ++ show e
			run db
		printResults [] = putStrLn "Nothing found"
		printResults rs = mapM_ printResult rs where
			printResult (ResultDeclaration d) = print d
			printResult (ResultModule m)
				| HsDev.bySources m = putStrLn $ "module " ++ symbolName m ++ " from " ++ maybe "" locationFile (symbolLocation m)
				| otherwise = putStrLn $ "module " ++ symbolName m ++ " from " ++ maybe "" show (moduleCabal $ symbol m)

runAction :: Monoid a => ErrorT String IO a -> IO a
runAction act = runErrorT act >>= either onError onOk where
	onError msg = do
		putStrLn $ "Error: " ++ msg
		return mempty
	onOk r = do
		putStrLn "Ok"
		return r

printUsage :: IO ()
printUsage = mapM_ print commands

cache :: Args String -> Database -> IO Database
cache as db
	| has "dump" as = do
		r <- runMaybeT $ msum [cabalSave, projectSave]
		maybe (putStrLn "Invalid arguments") (const $ putStrLn "Ok") r
		return mempty
	| has "load" as = do
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
			c <- MaybeT $ return $ arg "cabal" as
			if null c
				then return HsDev.Cabal
				else do
					c' <- liftIO $ canonicalizePath c
					return $ HsDev.CabalDev c'
		projectArg = fmap (HsDev.project) $ (MaybeT $ return $ arg "project" as) >>= liftIO . canonicalizePath
