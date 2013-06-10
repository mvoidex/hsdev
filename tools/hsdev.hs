{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, OverloadedStrings #-}

module Main (
	main
	) where

import Control.Arrow
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Error
import qualified Data.ByteString.Lazy.Char8 as L (unpack)
import Data.Aeson
import Data.Maybe
import Data.Monoid
import Data.List
import Data.Traversable (traverse)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Network.Socket
import System.Command hiding (brief)
import qualified System.Command as C (brief)
import System.Environment
import System.Exit
import System.Console.GetOpt
import System.Directory (canonicalizePath, getDirectoryContents, doesFileExist, createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath
import System.IO
import Text.Read (readMaybe)

import HsDev.Cache
import HsDev.Commands
import HsDev.Database
import HsDev.Database.Async
import HsDev.Project
import HsDev.Scan
import HsDev.Symbols
import HsDev.Symbols.Util
import HsDev.Symbols.JSON
import HsDev.Tools.GhcMod
import HsDev.Util

mainCommands :: [Command (IO ())]
mainCommands = [
	cmd_ ["help"] ["command"] "help" $ \as -> case as of
		[] -> do
			printMainUsage
			putStrLn ""
			putStrLn "Interactive commands:"
			putStrLn ""
			printUsage
		[cmdname] -> do
			let
				addHeader [] = []
				addHeader (h:hs) = map ('\t':) $ ("hsdev " ++ h) : map ('\t':) hs
			mapM_ (putStrLn . unlines . addHeader . help) $ filter ((== cmdname) . head . commandName) commands
		_ -> do
			printMainUsage
			putStrLn "Invalid arguments",
	cmd_ ["run"] [] "run interactive" $ \_ -> do
			db <- newAsync
			forever $ do
				s <- getLine
				r <- processCmd db s putStrLn
				unless r exitSuccess,
	cmd ["server"] [] "start server" [
		Option ['p'] ["port"] (ReqArg (First . readMaybe) "n") "port number"] $ \p _ -> do
			msgs <- newChan
			forkIO $ getChanContents msgs >>= mapM_ putStrLn
			thId <- myThreadId
			let
				outputStr = writeChan msgs
			forkIO $ do
				s <- socket AF_INET Stream defaultProtocol
				bind s (SockAddrInet (maybe 4567 fromInteger (getFirst p)) iNADDR_ANY)
				listen s maxListenQueue
				db <- newAsync
				forever $ handle ignoreIO $ do
					outputStr "listening for connections"
					s' <- fmap fst $ accept s
					void $ forkIO $ bracket (socketToHandle s' ReadWriteMode) hClose $ \h -> do
						str <- hGetLine h
						outputStr $ "received: " ++ str
						r <- processCmd db str (hPutStrLn h)
						unless r $ throwTo thId ExitSuccess
			forever $ do
				c <- getLine
				case c of
					"exit" -> exitSuccess
					_ -> outputStr "enter 'exit' to quit"]
	where
		ignoreIO :: IOException -> IO ()
		ignoreIO _ = return ()

data ClientOpts = ClientOpts {
	clientPort :: First Int,
	clientJson :: Any }

instance Monoid ClientOpts where
	mempty = ClientOpts mempty mempty
	l `mappend` r = ClientOpts (clientPort l `mappend` clientPort r) (clientJson l `mappend` clientJson r)

main :: IO ()
main = withSocketsDo $ do
	hSetBuffering stdout LineBuffering
	hSetEncoding stdout utf8
	as <- getArgs
	when (null as) $ do
		printMainUsage
		exitSuccess
	run mainCommands (onDef as) onError as
	where
		onError :: [String] -> IO ()
		onError = mapM_ putStrLn

		onDef :: [String] -> IO ()
		onDef as = do
			s <- socket AF_INET Stream defaultProtocol
			addr' <- inet_addr "127.0.0.1"
			connect s (SockAddrInet (maybe 4567 fromIntegral (getFirst $ clientPort p)) addr')
			h <- socketToHandle s ReadWriteMode
			hPutStrLn h $ unsplit $ (if getAny (clientJson p) then ("--json":) else id) as'
			hGetContents h >>= putStrLn
			where
				(ps, as', _) = getOpt RequireOrder [
					Option [] ["port"] (ReqArg (\p -> mempty { clientPort = First (readMaybe p) }) "number") "connection port",
					Option [] ["json"] (NoArg (mempty { clientJson = Any True })) "json output"] as
				p = mconcat ps

data CommandResult =
	ResultDeclarations [Symbol Declaration] |
	ResultModules [Symbol Module] |
	ResultOk (Map String String) |
	ResultError String (Map String String) |
	ResultProcess ((String -> IO ()) -> IO ()) |
	ResultExit

instance ToJSON CommandResult where
	toJSON (ResultDeclarations decls) = object [
		"result" .= ("ok" :: String),
		"declarations" .= toJSON (map encodeDeclaration decls)]
	toJSON (ResultModules ms) = object [
		"result" .= ("ok" :: String),
		"modules" .= toJSON (map encodeModule ms)]
	toJSON (ResultOk ps) = object [
		"result" .= ("ok" :: String),
		"params" .= toJSON ps]
	toJSON (ResultError e as) = object [
		"result" .= ("error" :: String),
		"error" .= e,
		"details" .= as]
	toJSON (ResultProcess _) = object [
		"result" .= ("process" :: String)]
	toJSON ResultExit = object [
		"result" .= ("exit" :: String)]

ok :: CommandResult
ok = ResultOk M.empty

err :: String -> CommandResult
err s = ResultError s M.empty

errArgs :: String -> [(String, String)] -> CommandResult
errArgs s as = ResultError s (M.fromList as)

commands :: [Command (Async Database -> IO CommandResult)]
commands = [
	cmd ["scan", "cabal"] [] "scan modules installed in cabal" [
		Option ['c'] ["cabal"] (ReqArg (opt "cabal") "path") "path to cabal sandbox",
		Option ['w'] ["wait"] (NoArg $ opt "wait" "") "wait for operation to complete",
		Option ['s'] ["status"] (NoArg $ opt "status" "") "show status of operation, works only with --wait"] $ \as _ db -> do
			ms <- runErrorT list
			cabal <- maybe (return Cabal) asCabal $ askOpt "cabal" as
			case ms of
				Left e -> return $ err $ "Failed to invoke ghc-mod 'list': " ++ e
				Right r -> startProcess as $ \onStatus -> do
					forM_ r $ \mname -> do
						result <- runErrorT $ update db $ scanModule cabal mname
						either
							(onStatus . (("error scanning module " ++ mname ++ ": ") ++))
							(const $ onStatus $ "module " ++ mname ++ " scanned")
							result,
	cmd ["scan", "project"] [] "scan project" [
		Option ['p'] ["project"] (ReqArg (opt "cabal") "path") "project's .cabal file",
		Option ['w'] ["wait"] (NoArg $ opt "wait" "") "wait for operation to complete",
		Option ['s'] ["status"] (NoArg $ opt "status" "") "show status of operation, works only with --wait"] $ \as _ db -> do
			proj <- locateProject $ fromMaybe "." $ askOpt "project" as
			case proj of
				Nothing -> return $ err $ "Project " ++ maybe "in current directory" (\p' -> "'" ++ p' ++ "'") (askOpt "project" as) ++ " not found"
				Just proj' -> startProcess as $ \onStatus -> do
					result <- runErrorT $ update db $ scanProject proj'
					either
						(onStatus . (("error scanning project " ++ projectName proj' ++ ": ") ++))
						(const $ onStatus $ "project " ++ projectName proj' ++ " scanned")
						result,
	cmd_ ["scan", "file"] ["source file"] "scan file" $ \fs db -> case fs of
		[file] -> do
			runErrorT $ update db $ scanFile file
			return ok
		_ -> return $ err "Invalid arguments",
	cmd ["scan", "module"] ["module name"] "scan module in cabal" [
		Option ['c'] ["cabal"] (ReqArg (First . Just) "path") "path to cabal sandbox"] $ \p ms db -> case ms of
			[mname] -> do
				runErrorT $ update db $ scanModule (maybe Cabal CabalDev $ getFirst p) mname
				return ok
			_ -> return $ err "Invalid arguments",
	cmd ["find"] ["symbol"] "find symbol" [
		Option ['p'] ["project"] (ReqArg (opt "project") "path") "project to find in",
		Option ['f'] ["file"] (ReqArg (opt "file") "path") "file to find in",
		Option ['m'] ["module"] (ReqArg (opt "module") "name") "module to find in",
		Option ['c'] ["cabal"] (ReqArg (opt "cabal") "path") "path to cabal sandbox"] $ \as ns db -> case ns of
			[nm] -> do
				dbval <- getDb db
				proj <- maybe (return Nothing) (fmap Just . getProject db) $ askOpt "project" as
				file <- traverse canonicalizePath (askOpt "file" as)
				cabal <- traverse asCabal $ askOpt "cabal" as
				let
					filters = satisfy $ catMaybes [
						fmap inProject proj,
						fmap inFile file,
						if isJust file then Just bySources else Nothing,
						fmap inModule (askOpt "module" as),
						fmap inCabal cabal]
				rs <- runErrorT $ findDeclaration dbval nm
				return $ either (err . ("Unable to find declaration: " ++)) (ResultDeclarations . filter filters) rs
			_ -> return $ err "Invalid arguments",
	cmd ["list"] [] "list modules" [
		Option ['p'] ["project"] (ReqArg (opt "project") "path") "project to list modules for",
		Option ['c'] ["cabal"] (ReqArg (opt "cabal") "path") "path to cabal sandbox"] $ \as _ db -> do
			dbval <- getDb db
			proj <- maybe (return Nothing) (fmap Just . getProject db) $ askOpt "project" as
			cabal <- traverse asCabal $ askOpt "cabal" as
			let
				filters = satisfy $ catMaybes [
					fmap inProject proj,
					fmap inCabal cabal]
			return $ ResultOk $ M.singleton "modules" $ unlines $ map symbolName $ filter filters $ concatMap S.toList $ M.elems $ databaseModules dbval,
	cmd ["browse"] ["module name"] "browse module" [
		Option ['p'] ["project"] (ReqArg (opt "project") "path") "project to look module in",
		Option ['c'] ["cabal"] (ReqArg (opt "cabal") "path") "path to cabal sandbox"] $ \as ms db -> case ms of
			[mname] -> do
				dbval <- getDb db
				proj <- maybe (return Nothing) (fmap Just . getProject db) $ askOpt "project" as
				cabal <- traverse asCabal $ askOpt "cabal" as
				let
					filters = satisfy $ catMaybes [
						fmap inProject proj,
						fmap inCabal cabal]
				rs <- runErrorT $ findModule dbval mname
				case rs of
					Left e -> return $ err e
					Right rs' -> case filter filters rs' of
						[] -> return $ errArgs "Module not found" [("module", mname)]
						[m] -> return $ ResultModules [m]
						ms' -> return $ errArgs "Ambiguous modules" [("modules", unlines (map showModule ms'))],
	cmd ["goto"] ["symbol"] "find symbol declaration" [
		Option ['f'] ["file"] (ReqArg (opt "file") "path") "context source file"] $ \as ns db -> case ns of
			[nm] -> do
				dbval <- getDb db
				liftM (either err ResultDeclarations) $ runErrorT $ goToDeclaration dbval (askOpt "file" as) nm
			_ -> return $ err "Invalid arguments",
	cmd ["info"] ["symbol"] "get info for symbol" [
		Option ['f'] ["file"] (ReqArg (opt "file") "path") "context source file"] $ \as ns db -> case ns of
			[nm] -> do
				dbval <- getDb db
				liftM (either err (ResultDeclarations . return)) $ runErrorT $ symbolInfo dbval (askOpt "file" as) nm
			_ -> return $ err "Invalid arguments",
	cmd_ ["lookup"] ["source file", "symbol"] "find symbol" $ \ns db -> case ns of
		[file, nm] -> do
			dbval <- getDb db
			r <- runErrorT $ liftM (either ResultDeclarations (ResultDeclarations . return)) (lookupSymbol dbval file nm)
			return $ either err id r
		_ -> return $ err "Invalid arguments",
	cmd_ ["import"] ["source file", "symbol"] "import module to bring symbols in scope" $ \ns db -> case ns of
		[file, nm] -> do
			dbval <- getDb db
			liftM (either err (ResultOk . M.singleton "imports" . unlines)) $ runErrorT (importSymbol dbval file nm)
		_ -> return $ err "Invalid arguments",
	cmd_ ["complete", "file"] ["source file", "input"] "autocompletion" $ \as db -> case as of
		[file, input] -> do
			dbval <- getDb db
			file' <- canonicalizePath file
			maybe
				(return $ errArgs "File is not scanned" [("file", file)])
				(\m -> liftM (either err ResultDeclarations) (runErrorT (completions dbval m input)))
				(lookupFile file' dbval)
		_ -> return $ err "Invalid arguments",
	cmd ["complete", "module"] ["module name", "input"] "autocompletion" [
		Option ['c'] ["cabal"] (ReqArg (First . Just) "path") "path to cabal sandbox"] $ \p as db -> case as of
			[mname, input] -> do
				dbval <- getDb db
				cabal <- getCabal $ getFirst p
				maybe
					(return $ errArgs "Unknown module" [("module", mname)])
					(\m -> liftM (either err ResultDeclarations) (runErrorT (completions dbval m input)))
					(lookupModule cabal mname dbval)
			_ -> return $ err "Invalid arguments",
	cmd_ ["dump", "files"] [] "dump file names loaded in database" $ \_ db -> do
		dbval <- getDb db
		return $ ResultOk $ M.fromList $ map (snd &&& fst) $ M.assocs $ M.map symbolName $ databaseFiles dbval,
	cmd_ ["dump", "contents"] ["file name"] "dump file contents" $ \as db -> case as of
		[file] -> do
			dbval <- getDb db
			return $ maybe (errArgs "File not found" [("file", file)]) (ResultModules . return) $ M.lookup file (databaseFiles dbval)
		_ -> return $ err "Invalid arguments",
	cmd ["cache", "dump", "cabal"] [] "dump cache of cabal packages" [
		Option ['c'] ["cabal"] (ReqArg (First . Just) "path") "path to cabal sandbox"] $ \p _ db -> do
			dbval <- getDb db
			cabal <- getCabal $ getFirst p
			dump (cabalCache cabal) (cabalModules cabal dbval)
			return ok,
	cmd_ ["cache", "dump", "project"] ["project name"] "dump cache of project" $ \as db -> case as of
		[proj] -> do
			dbval <- getDb db
			proj' <- getProject db proj
			dump (projectCache proj') (projectModules proj' dbval)
			return ok,
	cmd_ ["cache", "dump", "all"] ["path"] "dump all" $ \as db -> do
		if length as > 1
			then return (err "Invalid arguments")
			else do
				let
					path = fromMaybe "." $ listToMaybe as
				dbval <- getDb db
				createDirectoryIfMissing True (path </> "cabal")
				createDirectoryIfMissing True (path </> "projects")
				forM_ (M.keys $ databaseCabalModules dbval) $ \c -> dump (path </> "cabal" </> cabalCache c) (cabalModules c dbval)
				forM_ (M.keys $ databaseProjects dbval) $ \p -> dump (path </> "projects" </> projectCache (project p)) (projectModules (project p) dbval)
				return ok,
	cmd ["cache", "load", "cabal"] [] "load cache for cabal packages" [
		Option ['c'] ["cabal"] (ReqArg (First . Just) "path") "path to cabal sandbox"] $ \p _ db -> do
			cabal <- getCabal $ getFirst p
			cacheLoad db $ load (cabalCache cabal)
			return ok,
	cmd_ ["cache", "load", "project"] ["project name"] "load cache for project" $ \as db -> case as of
		[proj] -> do
			proj' <- getProj proj
			cacheLoad db . load . projectCache $ proj'
			return ok
		_ -> return $ err "Invalid arguments",
	cmd_ ["cache", "load", "all"] ["paths..."] "load cache from directory" $ \as db -> do
		cts <- liftM concat $ mapM (liftM (filter ((== ".json") . takeExtension)) . getDirectoryContents') $ concatMap (\p -> [p, p </> "cabal", p </> "projects"]) (if null as then ["."] else as)
		forM_ cts $ \c -> do
			e <- doesFileExist c
			when e $ cacheLoad db (load c)
		return ok,
	cmd_ ["help"] ["command"] "this command" $ \as db -> case as of
		[] -> printUsage >> return ok
		[cmdname] -> do
			let
				addHeader [] = []
				addHeader (h:hs) = map ('\t':) $ ("hsdev " ++ h) : map ('\t':) hs
			mapM_ (putStrLn . unlines . addHeader . help) $ filter ((== cmdname) . head . commandName) commands
			return ok
		_ -> return $ err "Invalid arguments",
	cmd_ ["exit"] [] "exit" $ \_ _ -> return ResultExit]
	where
		getDirectoryContents' :: FilePath -> IO [FilePath]
		getDirectoryContents' p = do
			b <- doesDirectoryExist p
			if b then liftM (map (p </>) . filter (`notElem` [".", ".."])) (getDirectoryContents p) else return []

		cacheLoad db act = do
			db' <- act
			case db' of
				Left e -> putStrLn e
				Right database -> modifyAsync db (Append database)

		showModule = symbolName

		getCabal :: Maybe String -> IO Cabal
		getCabal = liftM (maybe Cabal CabalDev) . traverse canonicalizePath

		getProj :: String -> IO Project
		getProj = fmap project . canonicalizePath

		asCabal "" = return Cabal
		asCabal p = fmap CabalDev $ canonicalizePath p

		getProject :: Async Database -> String -> IO Project
		getProject db projName = do
			db' <- getDb db
			let
				foundProject = find ((== projName) . projectName) $ M.elems $ databaseProjects db'
			projCabal <- canonicalizePath projName
			return $ fromMaybe (error "Impossible happened") $ msum [foundProject, Just $ project projCabal]

		getDb :: (MonadIO m) => Async Database -> m Database
		getDb = liftIO . readAsync

		startProcess :: Map String String -> ((String -> IO ()) -> IO ()) -> IO CommandResult
		startProcess as f
			| isJust (askOpt "wait" as) = return $ ResultProcess (f . onMsg)
			| otherwise = forkIO (f $ const $ return ()) >> return ok
			where
				onMsg showMsg = if isJust (askOpt "status" as) then showMsg else (const $ return ())

printMainUsage :: IO ()
printMainUsage = do
	mapM_ (putStrLn . ('\t':) . ("hsdev " ++) . C.brief) mainCommands
	putStrLn "\thsdev [--port=number] [--json] interactive command... -- send command to server"

printUsage :: IO ()
printUsage = mapM_ (putStrLn . ('\t':) . ("hsdev " ++) . C.brief) commands

processCmd :: Async Database -> String -> (String -> IO ()) -> IO Bool
processCmd db cmdArgs printResult = run commands (asCmd unknownCommand) (asCmd . commandError) cmdArgs' db >>= processResult where
	(isJsonArgs, cmdArgs', _) = getOpt RequireOrder [Option [] ["json"] (NoArg (Any True)) "json output"] (split cmdArgs)
	isJson = getAny $ mconcat isJsonArgs
	asCmd :: CommandResult -> (Async Database -> IO CommandResult)
	asCmd r _ = return r

	unknownCommand :: CommandResult
	unknownCommand = err "Unknown command"
	commandError :: [String] -> CommandResult
	commandError errs = errArgs "Command syntax error" [("what", unlines errs)]

	processResult :: CommandResult -> IO Bool
	processResult (ResultProcess act) = act (printResult . showStatus) >> processResult ok where
		showStatus s
			| isJson = L.unpack $ encode $ object ["status" .= s]
			| otherwise = "status: " ++ s
	processResult ResultExit = printResult (if isJson then L.unpack (encode ResultExit) else "bye") >> return False
	processResult v
		| isJson = (printResult $ L.unpack $ encode v) >> return True
		| otherwise = printResult str >> return True where
			str = case v of
				ResultDeclarations decls -> unlines $ map (formatResult fmt) decls
				ResultModules ms -> unlines $ map (formatModules fmt) ms
				ResultOk ps -> unlines $ "ok" : map (('\t':) . uncurry showP) (M.toList ps)
				ResultError e ds -> unlines $ ("error: " ++ e) : map (('\t':) . uncurry showP) (M.toList ds)
				where
					fmt :: String
					fmt = "detailed" -- fromMaybe "raw" $ get "format" as
					formatResult fmt' = case fmt' of
						"raw" -> show
						"name" -> symbolName
						"brief" -> brief
						"detailed" -> detailed
						_ -> show
					formatModules fmt' = case fmt' of
						"raw" -> show
						"name" -> symbolName
						"brief" -> moduleBrief
						"detailed" -> moduleDetailed
						_ -> show
					moduleBrief m
						| bySources m = "module " ++ symbolName m ++ maybe "" (\l -> " (" ++ locationFile l ++ ")") (symbolLocation m)
						| otherwise = "module " ++ symbolName m ++ maybe "" (\c -> " (" ++ show c ++ ")") (moduleCabal (symbol m))
					moduleDetailed m = unlines $ moduleBrief m : map (formatResult ("brief" :: String)) (M.elems $ moduleDeclarations $ symbol m)
	showP :: String -> String -> String
	showP n v = n ++ ": " ++ v
