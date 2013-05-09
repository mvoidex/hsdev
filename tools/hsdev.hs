module Main (
	main
	) where

import Control.Exception (catch, SomeException)
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as L (unpack)
import Data.Aeson (encode)
import Data.Either
import Data.Maybe
import Data.Monoid
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import System.Args
import System.Environment
import System.Directory (canonicalizePath, getDirectoryContents, doesFileExist)
import System.FilePath
import System.IO

import HsDev.Cache
import HsDev.Commands
import HsDev.Database
import HsDev.Project
import HsDev.Scan
import HsDev.Symbols
import HsDev.Symbols.Util
import HsDev.Symbols.JSON
import HsDev.Util

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
		"[-cabal [sandbox]]" $= "sandbox of module, where symbol defined",
		"[-raw]" $= "don't format output",
		"[-json]" $= "format as json"],
	cmd "list" "list modules" [
		"[-cabal [sandbox]]" $= "modules from sandbox",
		"[-project p]" $= "modules from project"],
	cmd "browse" "browse module" [
		"module" $= "",
		"[-project p]" $= "module in project",
		"[-cabal [sandbox]]" $= "sandbox of module",
		"[-raw]" $= "don't format output",
		"[-detailed]" $= "show detailed info",
		"[-json]" $= "format as json"],
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
	cmd "cache" "dump all" ["-dump" $= ""],
	cmd "cache" "load cache of cabal packages" ["-load" $= "", "-cabal [sandbox]" $= ""],
	cmd "cache" "load cache of project" ["-load" $= "", "-project p" $= ""],
	cmd "cache" "load cache from file" ["-load" $= "", "-file cache-file" $= ""],
	cmd "cache" "load cache from directory" ["-load" $= "", "-path path" $= ""],
	cmd "help" "this command" [],
	cmd "exit" "exit" []]

run :: Database -> IO ()
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
						lift $ scanCabal (if null cabalPath then Cabal else CabalDev cabalPath),
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
				proj' <- getProject db cmdArgs
				file' <- maybe (return Nothing) (\f -> if null f then return Nothing else fmap Just (canonicalizePath f)) $ arg "file" cmdArgs
				let
					filters :: Symbol a -> Bool
					filters = satisfy $ catMaybes [
						fmap inProject proj',
						fmap inFile file',
						if has "file" cmdArgs then Just bySources else Nothing,
						fmap inModule $ arg "module" cmdArgs,
						fmap (inCabal . asCabal) $ arg "cabal" cmdArgs]
					rs' = filter (filterResult filters) rs
					formatJson = forM_ (lefts $ map (searchResult Left Right) rs') $ \r ->
						putStrLn $ L.unpack $ encode $ encodeDeclaration r
				fromMaybe (return ()) $ msum [
					if has "raw" cmdArgs then Just $ printResults rs' else Nothing,
					if has "json" cmdArgs then Just formatJson else Nothing,
					Just (formatResult rs')]
				run db
			"list" -> do
				proj' <- getProject db cmdArgs
				let
					filters = satisfy $ catMaybes [
						fmap inProject proj',
						fmap (inCabal . asCabal) $ arg "cabal" cmdArgs]
					ms = filter filters $ concatMap S.toList $ M.elems $ databaseModules db
				printModules ms
				run db
			"browse" -> do
				rs <- runAction (findSymbol db (fromJust $ at 0 cmdArgs))
				proj' <- getProject db cmdArgs
				let
					filters :: Symbol a -> Bool
					filters = satisfy $ catMaybes [
						fmap inProject proj',
						fmap (inCabal . asCabal) $ arg "cabal" cmdArgs]
					rs' = filter (filterResult filters) $ filter (searchResult (const False) (const True)) rs
					browsedModule = searchResult (error "Impossible") id $ head rs'
				fromMaybe (return ()) $ msum [
					if length rs' > 1 then Just (putStrLn "Ambiguous modules:" >> printResults rs') else Nothing,
					if null rs' then Just (putStrLn "Module not found") else Nothing,
					if has "raw" cmdArgs then Just $ mapM_ print $ M.elems $ moduleDeclarations $ symbol browsedModule else Nothing,
					if has "json" cmdArgs then Just (putStrLn $ L.unpack $ encode $ encodeModule browsedModule) else Nothing,
					Just (mapM_ (putStrLn . (if has "detailed" cmdArgs then detailed else brief)) $ M.elems $ moduleDeclarations $ symbol browsedModule)]
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
				| bySources m = putStrLn $ "module " ++ symbolName m ++ " from " ++ maybe "" locationFile (symbolLocation m)
				| otherwise = putStrLn $ "module " ++ symbolName m ++ " from " ++ maybe "" show (moduleCabal $ symbol m)
		formatResult rs = mapM_ putStrLn $ mapMaybe (searchResult (Just . detailed) (const Nothing)) rs
		printModules ms = mapM_ printModule ms where
			printModule m
				| bySources m = putStrLn $ symbolName m ++ " (" ++ maybe "" locationFile (symbolLocation m) ++ ")"
				| otherwise = putStrLn $ symbolName m ++ maybe "" (\c -> if c == Cabal then "" else show c) (moduleCabal $ symbol m)
		asCabal "" = Cabal
		asCabal p = CabalDev p
		getProject :: Database -> Args String -> IO (Maybe Project)
		getProject db as = do
			projCabal <- maybe (return Nothing) (fmap Just . canonicalizePath) pname
			return $ msum [proj, fmap project projCabal]
			where
				pname = arg "project" as
				proj = find ((== pname) . Just . projectName) $ M.elems $ databaseProjects db

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
		r <- runMaybeT $ msum [cabalSave, projectSave, saveAll]
		maybe (putStrLn "Invalid arguments") (const $ putStrLn "Ok") r
		return mempty
	| has "load" as = do
		r <- runMaybeT $ msum [cabalLoad, projectLoad, loadPath]
		maybe (putStrLn "Invalid arguments" >> return mempty) return r
	| otherwise = putStrLn "Invalid arguments" >> return mempty
	where
		cabalSave = do
			cabal <- cabalArg
			liftIO $ dump (cabalCache cabal) (cabalModules cabal db)
		projectSave = do
			proj <- projectArg
			liftIO $ dump (projectCache proj) (projectModules proj db)
		saveAll = liftIO $ do
			forM_ (M.keys $ databaseCabalModules db) $ \cabal -> dump (cabalCache cabal) (cabalModules cabal db)
			forM_ (M.keys $ databaseProjects db) $ \proj -> dump (projectCache $ project proj) (projectModules (project proj) db)
		cabalLoad = do
			cabal <- cabalArg
			liftIO $ load (cabalCache cabal)
		projectLoad = do
			proj <- projectArg
			liftIO $ load (projectCache proj)
		loadPath = do
			p <- MaybeT $ return $ arg "path" as
			path <- liftIO $ canonicalizePath p
			cts <- liftM (filter ((== ".json") . takeExtension)) $ liftIO $ getDirectoryContents path
			liftM mconcat $ forM cts $ \c -> liftIO $ do
				e <- doesFileExist (path </> c)
				if e then load (path </> c) else return mempty
		cabalArg = do
			c <- MaybeT $ return $ arg "cabal" as
			if null c
				then return Cabal
				else do
					c' <- liftIO $ canonicalizePath c
					return $ CabalDev c'
		projectArg = fmap (project) $ (MaybeT $ return $ arg "project" as) >>= liftIO . canonicalizePath
