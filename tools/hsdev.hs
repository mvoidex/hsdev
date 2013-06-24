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
import qualified Data.ByteString.Lazy.Char8 as L (unpack, pack)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Either (partitionEithers)
import Data.Maybe
import Data.Monoid
import Data.List
import Data.Traversable (traverse)
import qualified Data.Traversable as T (forM)
import Data.Map (Map)
import Data.String (fromString)
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
import System.Process
import Text.Read (readMaybe)

import HsDev.Cache
import HsDev.Commands
import HsDev.Database
import HsDev.Database.Async
import HsDev.Project
import HsDev.Project.JSON
import HsDev.Scan
import HsDev.Symbols
import HsDev.Symbols.Util
import HsDev.Symbols.JSON
import HsDev.Tools.GhcMod
import HsDev.Util
import HsDev.Inspect

mainCommands :: [Command (IO ())]
mainCommands = [
	cmd_ ["help"] ["command"] "help" $ \as -> case as of
		[] -> do
			printMainUsage
			putStrLn ""
			putStrLn "Interactive commands:"
			putStrLn ""
			printUsage
		cmdname -> do
			let
				addHeader [] = []
				addHeader (h:hs) = map ('\t':) $ ("hsdev " ++ h) : map ('\t':) hs
			case filter ((cmdname `isPrefixOf`) . commandName) commands of
				[] -> putStrLn $ "Unknown command: " ++ unwords cmdname
				helpCmds -> mapM_ (putStrLn . unlines . addHeader . help) helpCmds,
	cmd_ ["run"] [] "run interactive" $ \_ -> do
			db <- newAsync
			forever $ do
				s <- getLine
				r <- processCmd db s putStrLn
				unless r exitSuccess,
	cmd ["server", "start"] [] "start remote server" clientOpts $ \copts _ -> do
		let
			args = ["server", "run"] ++ maybe [] (\p' -> ["--port", show p']) (getFirst $ clientPort copts)
		void $ runInteractiveProcess "powershell" ["-Command", "& {start-process hsdev " ++ intercalate ", " args ++ " -WindowStyle Hidden}"] Nothing Nothing
		printOk copts $ "Server started at port " ++ show (fromMaybe 4567 (getFirst $ clientPort copts)),
	cmd ["server", "run"] [] "start server" clientOpts $ \copts _ -> do
		msgs <- newChan
		forkIO $ getChanContents msgs >>= mapM_ (printOk copts)
		let
			outputStr = writeChan msgs

		db <- newAsync

		waitListen <- newEmptyMVar
		clientChan <- newChan

		forkIO $ do
			accepter <- myThreadId
			s <- socket AF_INET Stream defaultProtocol
			bind s (SockAddrInet (maybe 4567 fromIntegral (getFirst $ clientPort copts)) iNADDR_ANY)
			listen s maxListenQueue
			forever $ handle (ignoreIO outputStr) $ do
				s' <- fmap fst $ accept s
				outputStr $ show s' ++ " connected"
				void $ forkIO $ bracket (socketToHandle s' ReadWriteMode) hClose $ \h -> do
					me <- myThreadId
					done <- newEmptyMVar
					let
						timeoutWait = do
							notDone <- isEmptyMVar done
							when notDone $ do
								void $ forkIO $ do
									threadDelay 1000000
									tryPutMVar done ()
									killThread me
								void $ takeMVar done
					writeChan clientChan (Just timeoutWait)
					req <- hGetLine h
					outputStr $ show s' ++ ": " ++ req
					r <- processCmd db req (hPutStrLn h)
					unless r $ do
						void $ tryPutMVar waitListen ()
						killThread accepter
					putMVar done ()


		takeMVar waitListen
		outputStr "waiting for clients"
		writeChan clientChan Nothing
		getChanContents clientChan >>= sequence_ . catMaybes . takeWhile isJust
		outputStr "server shutdown",
	cmd ["server", "stop"] [] "stop remote server" clientOpts $ \copts _ -> do
		pname <- getExecutablePath
		r <- readProcess pname (["--json"] ++ maybe [] (\p' -> ["--port", show p']) (getFirst $ clientPort copts) ++ ["exit"]) ""
		let
			stopped = case decode (L.pack r) of
				Just (Object obj) -> fromMaybe False $ flip parseMaybe obj $ \_ -> do
					res <- obj .: "result"
					return (res == ("exit" :: String))
				_ -> False
		printOk copts $ if stopped then "Server stopped" else "Server returned: " ++ r]
	where
		ignoreIO :: (String -> IO ()) -> IOException -> IO ()
		ignoreIO out = out . show

		printOk :: ClientOpts -> String -> IO ()
		printOk copts str
			| getAny (clientJson copts) = putStrLn $ L.unpack $ encode $ object [
				"result" .= ("ok" :: String),
				"message" .= str]
			| otherwise = putStrLn str

data ClientOpts = ClientOpts {
	clientPort :: First Int,
	clientJson :: Any }

clientOpts :: [OptDescr ClientOpts]
clientOpts = [
	Option [] ["port"] (ReqArg (\p -> mempty { clientPort = First (readMaybe p) }) "number") "connection port",
	Option [] ["json"] (NoArg (mempty { clientJson = Any True })) "json output"]

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
				(ps, as', _) = getOpt RequireOrder clientOpts as
				p = mconcat ps

data CommandResult =
	ResultDeclarations [Symbol Declaration] |
	ResultModules [Symbol Module] |
	ResultProjects [Project] |
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
	toJSON (ResultProjects ps) = object [
		"result" .= ("ok" :: String),
		"projects" .= toJSON (map encodeProject ps)]
	toJSON (ResultOk ps) = object $ ("result" .= ("ok" :: String)) : map (uncurry (.=) . first fromString) (M.toList ps) where
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
	cmd ["scan", "cabal"] [] "scan modules installed in cabal" (sandbox : [wait, status]) $
		\as _ db -> do
			dbval <- getDb db
			ms <- runErrorT list
			cabal <- maybe (return Cabal) asCabal $ askOpt "cabal" as
			case ms of
				Left e -> return $ err $ "Failed to invoke ghc-mod 'list': " ++ e
				Right r -> startProcess as $ \onStatus -> do
					forM_ r $ \mname -> when (isNothing $ lookupModule cabal mname dbval) $ void $ runErrorT $ do
						update db $ scanModule cabal mname
						liftIO $ onStatus $ "module " ++ mname ++ " scanned"
						`catchError`
						(liftIO . onStatus . (("error scanning module " ++ mname ++ ": ") ++)),
	cmd ["scan", "project"] [] "scan project" (projectArg "project's .cabal file" : [wait, status]) $
		\as _ db -> do
			dbval <- getDb db
			proj <- locateProject $ fromMaybe "." $ askOpt "project" as
			case proj of
				Nothing -> return $ err $ "Project " ++ maybe "in current directory" (\p' -> "'" ++ p' ++ "'") (askOpt "project" as) ++ " not found"
				Just proj' -> startProcess as $ \onStatus -> void $ runErrorT $ flip catchError (liftIO . onStatus . (("scanning project " ++ projectName proj' ++ " fails with: ") ++)) $ do
					proj'' <- loadProject proj'
					update db $ return $ fromProject proj''
					srcs <- projectSources proj''
					forM_ srcs $ \src -> flip catchError (liftIO . onStatus . (("error scanning file " ++ src ++ ": ") ++)) $ do
						maybe (update db $ fmap fromModule $ inspectFile src) (update db . rescanModule) $ lookupFile src dbval
						liftIO (onStatus $ "file " ++ src ++ " scanned"),
	cmd ["scan", "path"] ["path to scan"] "scan directory" (
		Option ['p'] ["projects"] (NoArg (opt "projects" "")) "scan only for projects" : [wait, status]) $ \as ps db -> case ps of
		[dir] -> do
			dbval <- getDb db
			dir' <- canonicalizePath dir
			exists <- doesDirectoryExist dir'
			case exists of
				False -> return $ err $ "Invalid directory: " ++ dir
				True -> case isJust (askOpt "projects" as) of
					False -> startProcess as $ \onStatus -> do
						onStatus $ "scanning " ++ dir ++ " for haskell sources"
						srcs <- liftM (filter haskellSource) $ traverseDirectory dir
						forM_ srcs $ \src -> void $ runErrorT $ flip catchError (liftIO . onStatus . (("error scanning file " ++ src ++ ": ") ++)) $ do
							maybe (update db $ fmap fromModule $ inspectFile src) (update db . rescanModule) $ lookupFile src dbval
							liftIO (onStatus $ "file " ++ src ++ " scanned")
					True -> startProcess as $ \onStatus -> do
						onStatus $ "scanning " ++ dir ++ " for cabal projects"
						cabals <- liftM (filter cabalFile) $ traverseDirectory dir
						forM_ cabals $ \cabal -> void $ runErrorT $ flip catchError (liftIO . onStatus . (("scanning project " ++ cabal ++ " fails with: ") ++)) $ do
							proj <- loadProject $ project cabal
							update db $ return $ fromProject proj
							srcs <- projectSources proj
							forM_ srcs $ \src -> flip catchError (liftIO . onStatus . (("error scanning file " ++ src ++ ": ") ++)) $ do
								maybe (update db $ fmap fromModule $ inspectFile src) (update db . rescanModule) $ lookupFile src dbval
								liftIO (onStatus $ "file " ++ src ++ " scanned")
							liftIO (onStatus $ "project " ++ projectName proj ++ " scanned")
		_ -> return $ err "Invalid arguments",
	cmd_ ["scan", "file"] ["source file"] "scan file" $ \fs db -> case fs of
		[file] -> do
			dbval <- getDb db
			file' <- canonicalizePath file
			res <- runErrorT $ maybe (update db $ fmap fromModule $ inspectFile file') (update db . rescanModule) $ lookupFile file' dbval
			return $ either (\e -> errArgs e []) (const ok) res
		_ -> return $ err "Invalid arguments",
	cmd ["scan", "module"] ["module name"] "scan module in cabal" [sandbox] $ \as ms db -> case ms of
		[mname] -> do
			dbval <- getDb db
			if (isNothing $ lookupModule (maybe Cabal CabalDev $ askOpt "cabal" as) mname dbval)
				then do
					res <- runErrorT $ update db $ scanModule (maybe Cabal CabalDev $ askOpt "cabal" as) mname
					return $ either (\e -> errArgs e []) (const ok) res
				else return ok
		_ -> return $ err "Invalid arguments",
	cmd ["rescan"] [] "rescan sources" ([
		projectArg "project to rescan",
		fileArg "file to rescan",
		Option ['d'] ["dir"] (ReqArg (opt "dir") "directory") "directory to rescan"] ++ [wait, status]) $
			\as _ db -> do
				dbval <- getDb db
				projectMods <- T.forM (askOpt "project" as) $ \p -> do
					p' <- getProject db p
					let
						res = projectModules p' dbval
					return $ if M.null res then Left ("Unknown project: " ++ p) else Right res
				fileMods <- T.forM (askOpt "file" as) $ \f ->
					return $ maybe (Left $ "Unknown file: " ++ f) (Right . M.singleton f) $ lookupFile f dbval
				dirMods <- T.forM (askOpt "dir" as) $ \d -> do
					return $ Right $ M.filterWithKey (\f _ -> isParent d f) $ databaseFiles dbval

				let
					(errors, filteredMods) = partitionEithers $ catMaybes [projectMods, fileMods, dirMods]
					rescanMods = if null filteredMods then databaseFiles dbval else M.unions filteredMods
					moduleId m = maybe (symbolName m) locationFile $ symbolLocation m

				if not $ null errors
					then return $ err $ intercalate ", " errors
					else startProcess as $ \onStatus -> do
						forM_ (M.elems rescanMods) $ \m -> runErrorT $ flip catchError (throwError . (("error rescanning module " ++ moduleId m ++ ": ") ++)) $ do
							update db $ rescanModule m
							liftIO $ onStatus $ "module " ++ moduleId m ++ " rescanned",
	cmd ["clean"] [] "clean info about modules" [
		sandbox,
		projectArg "module project",
		fileArg "module source file",
		moduleArg "module name",
		Option ['a'] ["all"] (NoArg (opt "all" "")) "clear all"] $ \as _ db -> do
			dbval <- getDb db
			cabal <- traverse asCabal $ askOpt "cabal" as
			proj <- traverse (getProject db) $ askOpt "project" as
			file <- traverse canonicalizePath $ askOpt "file" as
			let
				cleanAll = isJust (askOpt "all" as)
				filters = catMaybes [
					fmap inProject proj,
					fmap inFile file,
					fmap inModule (askOpt "module" as),
					fmap inCabal cabal]
				toClean = filter (satisfy filters) (flattenModules dbval)
				mkey m
					| bySources m = "sources"
					| otherwise = maybe "<null>" show $ moduleCabal (symbol m)
				action
					| null filters && cleanAll = do
						modifyAsync db Clear
						return ok
					| null filters && not cleanAll = return $ errArgs "Specify filter or explicitely set flag --all" []
					| cleanAll = return $ errArgs "--all flag can't be set with filters" []
					| otherwise = do
						mapM_ (modifyAsync db . Remove . fromModule) toClean
						return $ ResultOk $ M.map unlines $ M.unionsWith (++) $ map (uncurry M.singleton . (mkey &&& (return . symbolName))) toClean
			action,
	cmd ["find"] ["symbol"] "find symbol" [
		projectArg "project to find in",
		fileArg "file to find in",
		moduleArg "module to find in",
		sandbox] $ \as ns db -> case ns of
			[nm] -> do
				dbval <- getDb db
				proj <- traverse (getProject db) $ askOpt "project" as
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
	cmd ["list"] [] "list modules" [projectArg "project to list modules for", sandbox] $ \as _ db -> do
		dbval <- getDb db
		proj <- traverse (getProject db) $ askOpt "project" as
		cabal <- traverse asCabal $ askOpt "cabal" as
		let
			filters = satisfy $ catMaybes [
				fmap inProject proj,
				fmap inCabal cabal]
		return $ ResultOk $ M.singleton "modules" $ unlines $ map symbolName $ filter filters $ concatMap S.toList $ M.elems $ databaseModules dbval,
	cmd ["browse"] ["module name"] "browse module" [
		projectArg "project to look module in",
		sandbox] $ \as ms db -> case ms of
			[mname] -> do
				dbval <- getDb db
				proj <- traverse (getProject db) $ askOpt "project" as
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
	cmd_ ["project"] ["project name"] "show project list or detailed project info" $ \ns db -> case ns of
		[] -> do
			dbval <- getDb db
			return $ ResultOk $ M.singleton "projects" $ unlines $ map projectName $ M.elems $ databaseProjects dbval
		[pname] -> do
			dbval <- getDb db
			case filter ((== pname) . projectName) $ M.elems $ databaseProjects dbval of
				[] -> return $ err $ "Project not found: " ++ pname
				ps -> return $ ResultProjects ps
		_ -> return $ err "Invalid arguments",
	cmd ["goto"] ["symbol"] "find symbol declaration" [contextFile] $ \as ns db -> case ns of
		[nm] -> do
			dbval <- getDb db
			liftM (either err ResultDeclarations) $ runErrorT $ goToDeclaration dbval (askOpt "file" as) nm
		_ -> return $ err "Invalid arguments",
	cmd ["info"] ["symbol"] "get info for symbol" [contextFile] $ \as ns db -> case ns of
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
	cmd ["complete", "module"] ["module name", "input"] "autocompletion" [sandbox] $ \p as db -> case as of
		[mname, input] -> do
			dbval <- getDb db
			cabal <- getCabal $ askOpt "cabal" p
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
	cmd ["cache", "dump", "cabal"] [] "dump cache of cabal modules" [
		sandbox, cacheDir] $ \as _ db -> do
			dbval <- getDb db
			cabal <- getCabal $ askOpt "cabal" as
			dump (pathArg as </> cabalCache cabal) (cabalModules cabal dbval)
			return ok,
	cmd ["cache", "dump", "project"] ["projects..."] "dump cache of project" [cacheDir] $ \as ns db -> do
		dbval <- getDb db
		ps <- if null ns
			then return (M.elems $ databaseProjects dbval)
			else mapM (getProject db) ns
		forM_ ps $ \p -> dump (pathArg as </> projectCache p) (projectModules p dbval)
		return ok,
	cmd ["cache", "dump", "standalone"] [] "dump cache of standalone files" [cacheDir] $ \as _ db -> do
		dbval <- getDb db
		dump (pathArg as </> standaloneCache) (standaloneModules dbval)
		return ok,
	cmd ["cache", "dump"] [] "dump all" [cacheDir] $ \as _ db -> do
		dbval <- getDb db
		createDirectoryIfMissing True (pathArg as </> "cabal")
		createDirectoryIfMissing True (pathArg as </> "projects")
		forM_ (M.keys $ databaseCabalModules dbval) $ \c -> dump (pathArg as </> "cabal" </> cabalCache c) (cabalModules c dbval)
		forM_ (M.keys $ databaseProjects dbval) $ \p -> dump (pathArg as </> "projects" </> projectCache (project p)) (projectModules (project p) dbval)
		dump (pathArg as </> standaloneCache) (standaloneModules dbval)
		return ok,
	cmd ["cache", "load", "cabal"] [] "load cache for cabal packages" [
		sandbox, cacheDir, wait] $ \as _ db -> do
			cabal <- getCabal $ askOpt "cabal" as
			cacheLoad db $ load (pathArg as </> cabalCache cabal)
			waitDb as db
			return ok,
	cmd ["cache", "load", "project"] ["projects..."] "load cache for projects" [cacheDir, wait] $ \as ns db -> do
		ps <- mapM getProj ns
		forM_ ps $ \p -> cacheLoad db (load (pathArg as </> projectCache p))
		waitDb as db
		return ok,
	cmd ["cache", "load", "standalone"] [] "load cache for standalone files" [cacheDir, wait] $ \as _ db -> do
		cacheLoad db (load $ pathArg as </> standaloneCache)
		waitDb as db
		return ok,
	cmd ["cache", "load"] [] "load cache from directories" [cacheDir, wait] $ \as _ db -> do
		cts <- liftM concat $ mapM (liftM (filter ((== ".json") . takeExtension)) . getDirectoryContents') [pathArg as, pathArg as </> "cabal", pathArg as </> "projects"]
		forM_ cts $ \c -> do
			e <- doesFileExist c
			when e $ cacheLoad db (load c)
		waitDb as db
		return ok,
	cmd_ ["help"] ["command"] "this command" $ \as _ -> case as of
		[] -> printUsage >> return ok
		cmdname -> do
			let
				addHeader [] = []
				addHeader (h:hs) = map ('\t':) $ ("hsdev " ++ h) : map ('\t':) hs
			case filter ((cmdname `isPrefixOf`) . commandName) commands of
				[] -> return $ err $  "Unknown command: " ++ unwords cmdname
				helpCmds -> mapM_ (putStrLn . unlines . addHeader . help) helpCmds >> return ok,
	cmd_ ["exit"] [] "exit" $ \_ _ -> return ResultExit]
	where
		getDirectoryContents' :: FilePath -> IO [FilePath]
		getDirectoryContents' p = do
			b <- doesDirectoryExist p
			if b then liftM (map (p </>) . filter (`notElem` [".", ".."])) (getDirectoryContents p) else return []

		waitDb as db = when (isJust (askOpt "wait" as)) $ do
			putStrLn "wait for db"
			waitDbVar <- newEmptyMVar
			modifyAsync db (Action $ \d -> putStrLn "db done!" >> putMVar waitDbVar () >> return d)
			takeMVar waitDbVar

		cacheLoad db act = do
			db' <- act
			case db' of
				Left e -> putStrLn e
				Right database -> update db (return database)

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

		wait = Option ['w'] ["wait"] (NoArg $ opt "wait" "") "wait for operation to complete"
		status = Option ['s'] ["status"] (NoArg $ opt "status" "") "show status of operation, works only with --wait"
		cacheDir = Option ['p'] ["path"] (ReqArg (opt "path") "path") "cache path"
		sandbox = Option ['c'] ["cabal"] (ReqArg (opt "cabal") "path") "path to cabal sandbox"
		projectArg = Option ['p'] ["project"] (ReqArg (opt "project") "path")
		fileArg = Option ['f'] ["file"] (ReqArg (opt "file") "path")
		moduleArg = Option ['m'] ["module"] (ReqArg (opt "module") "name")
		contextFile = fileArg "context source file"

		pathArg = fromMaybe "." . askOpt "path"

		isParent :: FilePath -> FilePath -> Bool
		isParent dir file = norm dir `isPrefixOf` norm file where
			norm = splitDirectories . normalise

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
	processResult (ResultProcess act) = do
		act (printResult . showStatus)
		processResult ok
		`catch`
		processFailed
		where
			processFailed :: SomeException -> IO Bool
			processFailed e = do
				processResult $ errArgs "process throws exception" [("exception", show e)]
				return True

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
				ResultProjects ps -> unlines $ map (formatProject fmt) ps
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
					formatProject fmt' = case fmt' of
						"raw" -> show
						"name" -> projectName
						"brief" -> projectCabal
						"detailed" -> show
					moduleBrief m
						| bySources m = "module " ++ symbolName m ++ maybe "" (\l -> " (" ++ locationFile l ++ ")") (symbolLocation m)
						| otherwise = "module " ++ symbolName m ++ maybe "" (\c -> " (" ++ show c ++ ")") (moduleCabal (symbol m))
					moduleDetailed m = unlines $ moduleBrief m : map (formatResult ("brief" :: String)) (M.elems $ moduleDeclarations $ symbol m)
	showP :: String -> String -> String
	showP n v = n ++ ": " ++ v
