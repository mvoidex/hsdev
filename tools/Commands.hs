{-# LANGUAGE OverloadedStrings, CPP, TupleSections #-}

module Commands (
	mainCommands, commands
	) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans.Maybe
import Control.Exception
import Control.Concurrent
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.Types
import Data.Char
import Data.Either
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Traversable (traverse)
import Network.Socket
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process
import System.Console.GetOpt
import System.Timeout
import System.FilePath
import Text.Read (readMaybe)

import qualified HsDev.Database.Async as DB
import HsDev.Commands
import HsDev.Database
import HsDev.Project
import HsDev.Symbols
import HsDev.Symbols.Util
import HsDev.Util
import HsDev.Scan
import qualified HsDev.Tools.Cabal as Cabal
import qualified HsDev.Tools.GhcMod as GhcMod (typeOf)
import qualified HsDev.Tools.Hayoo as Hayoo
import qualified HsDev.Cache.Structured as SC
import HsDev.Cache

import qualified Control.Concurrent.FiniteChan as F
import System.Command

#if mingw32_HOST_OS
import System.Win32.FileMapping.Memory (withMapFile, readMapFile)
import System.Win32.FileMapping.NamePool
#else
import System.Posix.Process
import System.Posix.IO
#endif

import qualified Update
import Types

#if mingw32_HOST_OS

translate :: String -> String
translate str = '"' : snd (foldr escape (True,"\"") str) where
	escape '"'  (b, str) = (True,  '\\' : '"'  : str)
	escape '\\' (True, str) = (True,  '\\' : '\\' : str)
	escape '\\' (False, str) = (False, '\\' : str)
	escape c (b, str) = (False, c : str)

powershell :: String -> String
powershell str
	| all isAlphaNum str = str
	| otherwise = "'" ++ translate str ++ "'"

#endif

-- | Main commands
mainCommands :: [Command (IO ())]
mainCommands = addHelp "hsdev" id $ srvCmds ++ map wrapCmd commands where
	wrapCmd :: Command CommandAction -> Command (IO ())
	wrapCmd = fmap sendCmd . addClientOpts . fmap withOptsCommand
	srvCmds = [
		cmd ["server", "start"] [] "start remote server" serverOpts start',
		cmd ["server", "run"] [] "start server" serverOpts run',
		cmd ["server", "stop"] [] "stop remote server" clientOpts stop',
		cmd ["connect"] [] "connect to send commands directly" clientOpts connect']

	start' sopts _ = do
#if mingw32_HOST_OS
		let
			args = ["server", "run"] ++ serverOptsToArgs sopts
		myExe <- getExecutablePath
		r <- readProcess "powershell" [
			"-Command",
			unwords [
				"&", "{", "start-process",
				powershell myExe,
				intercalate ", " (map powershell args),
				"-WindowStyle Hidden",
				"}"]] ""
		if all isSpace r
			then putStrLn $ "Server started at port " ++ show (fromJust $ getFirst $ serverPort sopts)
			else putStrLn $ "Failed to start server: " ++ r
#else
		let
			forkError :: SomeException -> IO ()
			forkError e  = putStrLn $ "Failed to start server: " ++ show e

			proxy :: IO ()
			proxy = do
				createSession
				forkProcess serverAction
				exitImmediately ExitSuccess

			serverAction :: IO ()
			serverAction = do
				mapM_ closeFd [stdInput, stdOutput, stdError]
				nullFd <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
				mapM_ (dupTo nullFd) [stdInput, stdOutput, stdError]
				closeFd nullFd
				run' sopts []

		handle forkError $ do
			forkProcess proxy
			putStrLn $ "Server started at port " ++ show (fromJust $ getFirst $ serverPort sopts)
#endif
	run' sopts _
		| getAny (serverAsClient sopts) = runServer sopts $ \copts -> do
			commandLog copts $ "Server started as client connecting at port " ++ show (fromJust $ getFirst $ serverPort sopts)
			me <- myThreadId
			s <- socket AF_INET Stream defaultProtocol
			addr' <- inet_addr "127.0.0.1"
			connect s $ SockAddrInet (fromIntegral $ fromJust $ getFirst $ serverPort sopts) addr'
			bracket (socketToHandle s ReadWriteMode) hClose $ \h ->
				processClient (show s) (hGetLine' h) (L.hPutStrLn h) sopts (copts {
					commandExit = killThread me })
		| otherwise = runServer sopts $ \copts -> do
			commandLog copts $ "Server started at port " ++ show (fromJust $ getFirst $ serverPort sopts)

			waitListen <- newEmptyMVar
			clientChan <- F.newChan

			forkIO $ do
				accepter <- myThreadId

				let
					serverStop :: IO ()
					serverStop = void $ forkIO $ do
						void $ tryPutMVar waitListen ()
						killThread accepter

				s <- socket AF_INET Stream defaultProtocol
				bind s $ SockAddrInet (fromIntegral $ fromJust $ getFirst $ serverPort sopts) iNADDR_ANY
				listen s maxListenQueue
				forever $ logIO "accept client exception: " (commandLog copts) $ do
					s' <- fst <$> accept s
					void $ forkIO $ logIO (show s' ++ " exception: ") (commandLog copts) $
						bracket (socketToHandle s' ReadWriteMode) hClose $ \h -> do
							bracket newEmptyMVar (`putMVar` ()) $ \done -> do
								me <- myThreadId
								let
									timeoutWait = do
										notDone <- isEmptyMVar done
										when notDone $ do
											void $ forkIO $ do
												threadDelay 1000000
												tryPutMVar done ()
												killThread me
											takeMVar done
									waitForever = forever $ hGetLine' h
								F.putChan clientChan timeoutWait
								processClient (show s') (hGetLine' h) (L.hPutStrLn h) sopts (copts {
									commandHold = waitForever,
									commandExit = serverStop })

			takeMVar waitListen
			DB.readAsync (commandDatabase copts) >>= writeCache sopts (commandLog copts)
			F.stopChan clientChan >>= sequence_
			commandLog copts "server stopped"

	stop' copts _ = run (map wrapCmd' commands) onDef onError ["exit"] where
		onDef = putStrLn "Command 'exit' not found"
		onError es = putStrLn $ "Failed to stop server: " ++ intercalate ", " es
		wrapCmd' = fmap (sendCmd . (copts,) . withOptsCommand)

	connect' copts _ = do
		curDir <- getCurrentDirectory
		s <- socket AF_INET Stream defaultProtocol
		addr' <- inet_addr "127.0.0.1"
		connect s (SockAddrInet (fromIntegral $ fromJust $ getFirst $ clientPort copts) addr')
		bracket (socketToHandle s ReadWriteMode) hClose $ \h -> forever $ ignoreIO $ do
			cmd <- hGetLine' stdin
			case eitherDecode cmd of
				Left e -> L.putStrLn $ encodeValue $ object ["error" .= ("invalid command" :: String)]
				Right cmd' -> do
					L.hPutStrLn h $ encode $ cmd' `addCallOpts` ["current-directory" %-- curDir]
					waitResp h
		where
			pretty = getAny $ clientPretty copts
			encodeValue :: ToJSON a => a -> L.ByteString
			encodeValue
				| pretty = encodePretty
				| otherwise = encode

			waitResp h = do
				resp <- hGetLine' h
				parseResp h resp

			parseResp h str = void $ runErrorT $ flip catchError (liftIO . putStrLn) $ do
				v <- ErrorT (return $ eitherDecode str) `orFail` ("Can't decode response", ["response" .= fromUtf8 str])
				case v of
					ResponseStatus s -> liftIO $ do
						L.putStrLn $ encodeValue s
						liftIO $ waitResp h
#if mingw32_HOST_OS
					ResponseMapFile viewFile -> do
						str <- fmap L.fromStrict (readMapFile viewFile) `orFail`
							("Can't read map view of file", ["file" .= viewFile])
						lift $ parseResp h str
#else
					ResponseMapFile viewFile -> throwError $ fromUtf8 $ encodeValue $
						object ["error" .= ("Not supported" :: String)]
#endif
					Response r -> liftIO $ L.putStrLn $ encodeValue r
				where
					orFail :: (Monad m, Functor m) => ErrorT String m a -> (String, [Pair]) -> ErrorT String m a
					orFail act (msg, fs) = act <|> (throwError $ fromUtf8 $ encodeValue $ object (
						("error" .= msg) : fs))

	-- Send command to server
	sendCmd :: (ClientOpts, CommandCall) -> IO ()
	sendCmd (p, cmdCall) = do
		svar <- newEmptyMVar
		race [takeMVar svar, waitResponse >> putMVar svar ()]
		where
			pretty = getAny $ clientPretty p
			encodeValue :: ToJSON a => a -> L.ByteString
			encodeValue
				| pretty = encodePretty
				| otherwise = encode

			waitResponse = do
				curDir <- getCurrentDirectory
				stdinData <- if getAny (clientData p)
					then do
						cdata <- liftM (eitherDecode :: L.ByteString -> Either String Value) L.getContents
						case cdata of
							Left cdataErr -> do
								putStrLn $ "Invalid data: " ++ cdataErr
								exitFailure
							Right dataValue -> return $ Just dataValue
					else return Nothing

				s <- socket AF_INET Stream defaultProtocol
				addr' <- inet_addr "127.0.0.1"
				connect s (SockAddrInet (fromIntegral $ fromJust $ getFirst $ clientPort p) addr')
				h <- socketToHandle s ReadWriteMode
				L.hPutStrLn h $ encode $ cmdCall `addCallOpts` [
					"current-directory" %-- curDir,
					case stdinData of
						Nothing -> mempty
						Just d -> "data" %-- (fromUtf8 $ encode d)]
				peekResponse h

			peekResponse h = do
				resp <- hGetLine' h
				parseResponse h resp

			parseResponse h str = void $ runErrorT $ flip catchError (liftIO . putStrLn) $ do
				v <- ErrorT (return $ eitherDecode str) `orFail` ("Can't decode response", ["response" .= fromUtf8 str])
				case v of
					ResponseStatus s -> liftIO $ do
						L.putStrLn $ encodeValue s
						peekResponse h
#if mingw32_HOST_OS
					ResponseMapFile viewFile -> do
						str <- fmap L.fromStrict (readMapFile viewFile) `orFail`
							("Can't read map view of file", ["file" .= viewFile])
						lift $ parseResponse h str
#else
					ResponseMapFile viewFile -> throwError $ fromUtf8 $ encodeValue $
						object ["error" .= ("Not supported" :: String)]
#endif
					Response r -> liftIO $ L.putStrLn $ encodeValue r
				where
					orFail :: (Monad m, Functor m) => ErrorT String m a -> (String, [Pair]) -> ErrorT String m a
					orFail act (msg, fs) = act <|> (throwError $ fromUtf8 $ encodeValue $ object (
						("error" .= msg) : fs))

	-- Add parsing 'ClieptOpts'
	addClientOpts :: Command CommandCall -> Command (ClientOpts, CommandCall)
	addClientOpts c = c { commandRun = run' } where
		run' args = fmap (fmap (p,)) $ commandRun c args' where
			(ps, args', _) = getOpt RequireOrder clientOpts args
			p = mconcat ps `mappend` defaultConfig

-- | Inits log chan and returns functions (print message, wait channel)
initLog :: ServerOpts -> IO (String -> IO (), IO ())
initLog sopts = do
	msgs <- F.newChan
	outputDone <- newEmptyMVar
	forkIO $ finally
		(F.readChan msgs >>= mapM_ (logMsg sopts))
		(putMVar outputDone ())
	return (F.putChan msgs, F.closeChan msgs >> takeMVar outputDone)

-- | Run server
runServer :: ServerOpts -> (CommandOptions -> IO ()) -> IO ()
runServer sopts act = bracket (initLog sopts) snd $ \(outputStr, waitOutput) -> do
	db <- DB.newAsync
	when (getAny $ serverLoadCache sopts) $ withCache sopts () $ \cdir -> do
		outputStr $ "Loading cache from " ++ cdir
		dbCache <- liftA merge <$> SC.load cdir
		case dbCache of
			Left err -> outputStr $ "Failed to load cache: " ++ err
			Right dbCache' -> DB.update db (return dbCache')
#if mingw32_HOST_OS
	mmapPool <- Just <$> createPool "hsdev"
#endif
	act $ CommandOptions
		db
		(writeCache sopts outputStr)
		(readCache sopts outputStr)
		"."
		outputStr
		waitOutput
#if mingw32_HOST_OS
		mmapPool
#endif
		(return ())
		(return ())
		(return ())

withCache :: ServerOpts -> a -> (FilePath -> IO a) -> IO a
withCache sopts v onCache = case getFirst (serverCache sopts) of
	Nothing -> return v
	Just cdir -> onCache cdir

writeCache :: ServerOpts -> (String -> IO ()) -> Database -> IO ()
writeCache sopts logMsg d = withCache sopts () $ \cdir -> do
	logMsg $ "writing cache to " ++ cdir
	logIO "cache writing exception: " logMsg $ do
		SC.dump cdir $ structurize d
	logMsg $ "cache saved to " ++ cdir

readCache :: ServerOpts -> (String -> IO ()) -> (FilePath -> ErrorT String IO Structured) -> IO (Maybe Database)
readCache sopts logMsg act = withCache sopts Nothing $ join . liftM (either cacheErr cacheOk) . runErrorT . act where
	cacheErr e = logMsg ("Error reading cache: " ++ e) >> return Nothing
	cacheOk s = do
		forM_ (M.keys (structuredCabals s)) $ \c -> logMsg ("cache read: cabal " ++ show c)
		forM_ (M.keys (structuredProjects s)) $ \p -> logMsg ("cache read: project " ++ p)
		case allModules (structuredFiles s) of
			[] -> return ()
			ms -> logMsg $ "cache read: " ++ show (length ms) ++ " files"
		return $ Just $ merge s

#if mingw32_HOST_OS
sendResponseMmap :: Pool -> (ByteString -> IO ()) -> Response -> IO ()
sendResponseMmap mmapPool send r@(ResponseMapFile _) = send $ encode r
sendResponseMmap mmapPool send r
	| L.length msg <= 1024 = send msg
	| otherwise = do
		sync <- newEmptyMVar
		forkIO $ void $ withName mmapPool $ \mmapName -> do
			runErrorT $ flip catchError
				(\e -> liftIO $ do
					sendResponseMmap mmapPool send $ Response $ object ["error" .= e]
					putMVar sync ())
				(withMapFile mmapName (L.toStrict msg) $ liftIO $ do
					sendResponseMmap mmapPool send $ ResponseMapFile mmapName
					putMVar sync ()
					-- give 10 seconds for client to read data
					threadDelay 10000000)
		takeMVar sync
	where
		msg = encode r
#endif

sendResponse :: (ByteString -> IO ()) -> Response -> IO ()
sendResponse = (. encode)

processClient :: String -> IO ByteString -> (ByteString -> IO ()) -> ServerOpts -> CommandOptions -> IO ()
processClient name receive send sopts copts = do
	commandLog copts $ name ++ " connected"
	linkVar <- newMVar $ return ()
	flip finally (disconnected linkVar) $ forever $ do
		req <- receive
		commandLog copts $ name ++ " >> " ++ fromUtf8 req
		case extractMeta <$> eitherDecode req of
			Left err -> answer True $ Response $ object [
				"error" .= ("Invalid request" :: String),
				"request" .= fromUtf8 req,
				"what" .= err]
			Right (cdir, noFile, reqArgs) -> processCmdArgs
				(copts { commandLink = void (swapMVar linkVar $ commandExit copts), commandRoot = cdir })
				(fromJust $ getFirst $ serverTimeout sopts)
				(callArgs reqArgs)
				(answer noFile)
	where
		answer :: Bool -> Response -> IO ()
		answer noFile' r = do
			commandLog copts $ name ++ " << " ++ fromUtf8 (encode r)
#if mingw32_HOST_OS
			case noFile' of
				True -> sendResponse send r
				False -> maybe (sendResponse send) (`sendResponseMmap` send) (commandMmapPool copts) r
#else
			sendResponse send r
#endif

		extractMeta :: CommandCall -> (FilePath, Bool, CommandCall)
		extractMeta c = (fpath, noFile, c `removeCallOpts` ["current-directory", "no-file"]) where
			fpath = fromMaybe (commandRoot copts) $ arg "current-directory" $ commandCallOpts c
			noFile = flag "no-file" $ commandCallOpts c

		disconnected :: MVar (IO ()) -> IO ()
		disconnected var = do
			commandLog copts $ name ++ " disconnected"
			join $ takeMVar var

commands :: [Command CommandAction]
commands = map wrapErrors $ map (fmap (fmap timeout')) cmds ++ map (fmap (fmap noTimeout)) linkCmd where
	timeout' :: (CommandOptions -> IO CommandResult) -> (Int -> CommandOptions -> IO CommandResult)
	timeout' f tm copts = fmap (fromMaybe $ err "timeout") $ timeout (tm * 1000) $ f copts
	noTimeout :: (CommandOptions -> IO CommandResult) -> (Int -> CommandOptions -> IO CommandResult)
	noTimeout f _ copts = f copts

	handleErrors :: (Int -> CommandOptions -> IO CommandResult) -> (Int -> CommandOptions -> IO CommandResult)
	handleErrors act tm copts = handle onCmdErr (act tm copts) where
		onCmdErr :: SomeException -> IO CommandResult
		onCmdErr = return . err . show

	wrapErrors :: Command CommandAction -> Command CommandAction
	wrapErrors = fmap (fmap handleErrors)

	cmds = [
		-- Ping command
		cmd_' ["ping"] [] "ping server" ping',
		-- Database commands
		cmd' ["add"] [] "add info to database" [dataArg] add',
		cmd' ["scan", "cabal"] [] "scan modules installed in cabal" (sandboxes ++ [
			ghcOpts, wait, status]) scanCabal',
		cmd' ["scan", "module"] ["module name"] "scan module in cabal" (sandboxes ++ [ghcOpts]) scanModule',
		cmd' ["scan"] [] "scan sources" [
			projectArg "project path or .cabal",
			fileArg "source file",
			pathArg "directory to scan for files and projects",
			ghcOpts, wait, status] scan',
		cmd' ["rescan"] [] "rescan sources" [
			projectArg "project path or .cabal",
			fileArg "source file",
			pathArg "path to rescan",
			ghcOpts, wait, status] rescan',
		cmd' ["remove"] [] "remove modules info" (sandboxes ++ [
			projectArg "module project",
			fileArg "module source file",
			moduleArg,
			packageArg, noLastArg, packageVersionArg,
			allFlag]) remove',
		-- | Context free commands
		cmd' ["list", "modules"] [] "list modules" (sandboxes ++ [
			projectArg "projects to list modules from",
			noLastArg,
			packageArg,
			sourced, standaloned]) listModules',
		cmd_' ["list", "packages"] [] "list packages" listPackages',
		cmd_' ["list", "projects"] [] "list projects" listProjects',
		cmd' ["symbol"] ["name"] "get symbol info" (matches ++ sandboxes ++ [
			projectArg "related project",
			fileArg "source file",
			moduleArg, localsArg,
			packageArg, noLastArg, packageVersionArg,
			sourced, standaloned]) symbol',
		cmd' ["module"] [] "get module info" (sandboxes ++ [
			moduleArg, localsArg,
			packageArg, noLastArg, packageVersionArg,
			projectArg "module project",
			fileArg "module source file",
			sourced]) modul',
		cmd' ["project"] [] "get project info" [
			projectArg "project path or name"] project',
		-- Context commands
		cmd' ["lookup"] ["symbol"] "lookup for symbol" ctx lookup',
		cmd' ["whois"] ["symbol"] "get info for symbol" ctx whois',
		cmd' ["scope", "modules"] [] "get modules accessible from module or within a project" ctx scopeModules',
		cmd' ["scope"] [] "get declarations accessible from module or within a project" (ctx ++ matches ++ [globalArg]) scope',
		cmd' ["complete"] ["input"] "show completions for input" ctx complete',
		-- Tool commands
		cmd' ["hayoo"] ["query"] "find declarations online via Hayoo" [] hayoo',
		cmd' ["cabal", "list"] ["packages..."] "list cabal packages" [] cabalList',
		cmd' ["ghc-mod", "type"] ["line", "column"] "infer type with 'ghc-mod type'" ctx ghcmodType',
		-- Dump/load commands
		cmd' ["dump", "cabal"] [] "dump cabal modules" (sandboxes ++ [cacheDir, cacheFile]) dumpCabal',
		cmd' ["dump", "projects"] [] "dump projects" [projectArg "project", cacheDir, cacheFile] dumpProjects',
		cmd' ["dump", "files"] [] "dump standalone files" [cacheDir, cacheFile] dumpFiles',
		cmd' ["dump"] [] "dump whole database" [cacheDir, cacheFile] dump',
		cmd' ["load"] [] "load data" [cacheDir, cacheFile, dataArg, wait] load',
		-- Exit
		cmd_' ["exit"] [] "exit" exit']
	linkCmd = [cmd' ["link"] [] "link to server" [holdArg] link']

	-- Command arguments and flags
	allFlag = option_ ['a'] "all" no "remove all"
	cacheDir = pathArg "cache path"
	cacheFile = fileArg "cache file"
	ctx = [fileArg "source file", sandbox]
	dataArg = option_ [] "data" (req "contents") "data to pass to command"
	fileArg = option_ ['f'] "file" (req "file")
	findArg = option_ [] "find" (req "find") "infix match"
	ghcOpts = option_ ['g'] "ghc" (req "ghc options") "options to pass to GHC"
	globalArg = option_ [] "global" no "scope of project"
	holdArg = option_ ['h'] "hold" no "don't return any response"
	localsArg = option_ ['l'] "locals" no "look in local declarations"
	noLastArg = option_ [] "no-last" no "don't select last package version"
	matches = [prefixArg, findArg]
	moduleArg = option_ ['m'] "module" (req "module name") "module name"
	packageArg = option_ [] "package" (req "package") "module package"
	pathArg = option_ ['p'] "path" (req "path")
	prefixArg = option_ [] "prefix" (req "prefix") "prefix match"
	projectArg = option [] "project" ["proj"] (req "project")
	packageVersionArg = option_ ['v'] "version" (req "version") "package version"
	sandbox = option_ [] "sandbox" (req "path") "path to cabal sandbox"
	sandboxes = [
		option_ [] "cabal" no "cabal",
		sandbox]
	sourced = option_ [] "src" no "source files"
	standaloned = option_ [] "stand" no "standalone files"
	status = option_ ['s'] "status" no "show status of operation, works only with --wait"
	wait = option_ ['w'] "wait" no "wait for operation to complete"

	-- ping server
	ping' _ copts = return $ ResultOk $ ResultMap $ M.singleton "message" (ResultString "pong")
	-- add data
	add' as _ copts = do
		dbval <- getDb copts
		res <- runErrorT $ do
			jsonData <- maybe (throwError $ err "Specify --data") return $ arg "data" as
			decodedData <- either
				(\err -> throwError (errArgs "Unable to decode data" [
					("why", ResultString err),
					("data", ResultString jsonData)]))
				return $
				eitherDecode $ toUtf8 jsonData
			let
				updateData (ResultDeclaration d) = throwError $ errArgs "Can't insert declaration" [("declaration", ResultDeclaration d)]
				updateData (ResultModuleDeclaration md) = do
					let
						ModuleId mname mloc = declarationModuleId md
						defMod = Module mname Nothing mloc [] mempty mempty
						defInspMod = Inspected InspectionNone mloc (Right defMod)
						dbmod = maybe
							defInspMod
							(\i -> i { inspectionResult = inspectionResult i <|> (Right defMod) }) $
							M.lookup mloc (databaseModules dbval)
						updatedMod = dbmod {
							inspectionResult = fmap (addDeclaration $ moduleDeclaration md) (inspectionResult dbmod) }
					DB.update (dbVar copts) $ return $ fromModule updatedMod
				updateData (ResultModuleId (ModuleId mname mloc)) = when (M.notMember mloc $ databaseModules dbval) $
					DB.update (dbVar copts) $ return $ fromModule $ Inspected InspectionNone mloc (Right $ Module mname Nothing mloc [] mempty mempty)
				updateData (ResultModule m) = DB.update (dbVar copts) $ return $ fromModule $ Inspected InspectionNone (moduleLocation m) (Right m)
				updateData (ResultInspectedModule m) = DB.update (dbVar copts) $ return $ fromModule m
				updateData (ResultProject p) = DB.update (dbVar copts) $ return $ fromProject p
				updateData (ResultList l) = mapM_ updateData l
				updateData (ResultMap m) = mapM_ updateData $ M.elems m
				updateData (ResultString s) = throwError $ err "Can't insert string"
				updateData ResultNone = return ()
			updateData decodedData
		return $ either id (const (ResultOk ResultNone)) res
	-- scan
	scan' as _ copts = updateProcess copts as $
		mapM_ (\(n, f) -> forM_ (list n as) (findPath copts >=> f (list "ghc" as))) [
			("project", Update.scanProject),
			("file", Update.scanFile),
			("path", Update.scanDirectory)]
	-- scan cabal
	scanCabal' as _ copts = error_ $ do
		cabals <- getSandboxes copts as
		lift $ updateProcess copts as $ mapM_ (Update.scanCabal $ list "ghc" as) cabals
	-- scan cabal module
	scanModule' as [] copts = return $ err "Module name not specified"
	scanModule' as ms copts = error_ $ do
		cabal <- getCabal copts as
		lift $ updateProcess copts as $
			forM_ ms (Update.scanModule (list "ghc" as) . CabalModule cabal Nothing)
	-- rescan
	rescan' as _ copts = do
		dbval <- getDb copts
		let
			fileMap = M.fromList $ mapMaybe toPair $
				selectModules (byFile . moduleId) dbval

		(errors, filteredMods) <- liftM partitionEithers $ mapM runErrorT $ concat [
			[do
				p' <- findProject copts p
				return $ M.fromList $ mapMaybe toPair $
					selectModules (inProject p' . moduleId) dbval |
				p <- list "project" as],
			[do
				f' <- findPath copts f
				maybe
					(throwError $ "Unknown file: " ++ f')
					(return . M.singleton f')
					(lookupFile f' dbval) |
				f <- list "file" as],
			[do
				d' <- findPath copts d
				return $ M.filterWithKey (\f _ -> isParent d' f) fileMap |
				d <- list "path" as]]
		let
			rescanMods = map (getInspected dbval) $
				M.elems $ if null filteredMods then fileMap else M.unions filteredMods

		if not (null errors)
			then return $ err $ intercalate ", " errors
			else updateProcess copts as $ Update.runTask (toJSON $ ("rescanning modules" :: String)) $ do
				needRescan <- Update.liftErrorT $ filterM (changedModule dbval (list "ghc" as) . inspectedId) rescanMods
				Update.scanModules (list "ghc" as) (map (inspectedId &&& inspectionOpts . inspection) needRescan)
	-- remove
	remove' as _ copts = errorT $ do
		dbval <- getDb copts
		cabal <- getCabal_ copts as
		proj <- traverse (findProject copts) $ arg "project" as
		file <- traverse (findPath copts) $ arg "file" as
		let
			cleanAll = flag "all" as
			filters = catMaybes [
				fmap inProject proj,
				fmap inFile file,
				fmap inModule (arg "module" as),
				fmap inPackage (arg "package" as),
				fmap inVersion (arg "version" as),
				fmap inCabal cabal]
			toClean = newest as $ filter (allOf filters . moduleId) (allModules dbval)
			action
				| null filters && cleanAll = liftIO $ do
					DB.modifyAsync (dbVar copts) DB.Clear
					return ResultNone
				| null filters && not cleanAll = throwError "Specify filter or explicitely set flag --all"
				| cleanAll = throwError "--all flag can't be set with filters"
				| otherwise = liftIO $ do
					DB.modifyAsync (dbVar copts) $ DB.Remove $ mconcat $ map (fromModule . getInspected dbval) toClean
					return $ ResultList $ map (ResultModuleId . moduleId) toClean
		action
	-- list modules
	listModules' as _ copts = errorT $ do
		dbval <- getDb copts
		projs <- traverse (findProject copts) $ list "project" as
		cabals <- getSandboxes copts as
		let
			packages = list "package" as
			hasFilters = not $ null projs && null packages && null cabals
			filters = allOf $ catMaybes [
				if hasFilters
					then Just $ anyOf [
						\m -> any (`inProject` m) projs,
						\m -> any (`inPackage` m) packages,
						\m -> any (`inCabal` m) cabals]
					else Nothing,
				if flag "src" as then Just byFile else Nothing,
				if flag "stand" as then Just standalone else Nothing]
		return $ ResultList $ map (ResultModuleId . moduleId) $ newest as $ selectModules (filters . moduleId) dbval
	-- list packages
	listPackages' _ copts = do
		dbval <- getDb copts
		return $ ResultOk $ ResultList $
			map ResultPackage $ nub $ sort $
			mapMaybe (moduleCabalPackage . moduleLocation) $
			allModules dbval
	-- list projects
	listProjects' _ copts = do
		dbval <- getDb copts
		return $ ResultOk $ ResultList $ map ResultProject $ M.elems $ databaseProjects dbval
	-- get symbol info
	symbol' as ns copts = errorT $ do
		dbval <- liftM (localsDatabase as) $ getDb copts
		proj <- traverse (findProject copts) $ arg "project" as
		file <- traverse (findPath copts) $ arg "file" as
		cabal <- getCabal_ copts as
		let
			filters = checkModule $ allOf $ catMaybes [
				fmap inProject proj,
				fmap inFile file,
				fmap inModule (arg "module" as),
				fmap inPackage (arg "package" as),
				fmap inVersion (arg "version" as),
				fmap inCabal cabal,
				if flag "src" as then Just byFile else Nothing,
				if flag "stand" as then Just standalone else Nothing]
			toResult = ResultList . map ResultModuleDeclaration . newest as . filterMatch as . filter filters
		case ns of
			[] -> return $ toResult $ allDeclarations dbval
			[nm] -> liftM toResult (findDeclaration dbval nm) `catchError` (\e ->
				throwError ("Can't find symbol: " ++ e))
			_ -> throwError "Too much arguments"
	-- get module info
	modul' as _ copts = errorT' $ do
		dbval <- liftM (localsDatabase as) $ getDb copts
		proj <- mapErrorT (fmap $ strMsg +++ id) $ traverse (findProject copts) $ arg "project" as
		cabal <- mapErrorT (fmap $ strMsg +++ id) $ getCabal_ copts as
		file' <- mapErrorT (fmap $ strMsg +++ id) $ traverse (findPath copts) $ arg "file" as
		let
			filters = allOf $ catMaybes [
				fmap inProject proj,
				fmap inCabal cabal,
				fmap inFile file',
				fmap inModule (arg "module" as),
				fmap inPackage (arg "package" as),
				fmap inVersion (arg "version" as),
				if flag "src" as then Just byFile else Nothing]
		rs <- mapErrorT (fmap $ strMsg +++ id) $
			(newest as . filter (filters . moduleId)) <$> maybe
				(return $ allModules dbval)
				(findModule dbval)
				(arg "module" as)
		case rs of
			[] -> throwError $ err "Module not found"
			[m] -> return $ ResultModule m
			ms' -> throwError $ errArgs "Ambiguous modules" [("modules", ResultList $ map (ResultModuleId . moduleId) ms')]
	-- get project info
	project' as _ copts = errorT $ do
		proj <- maybe (throwError "Specify project name or .cabal file") (findProject copts) $ arg "project" as
		return $ ResultProject proj
	-- lookup info about symbol
	lookup' as [nm] copts = errorT $ do
		dbval <- getDb copts
		(srcFile, cabal) <- getCtx copts as
		liftM (ResultList . map ResultModuleDeclaration) $ lookupSymbol dbval cabal srcFile nm
	lookup' as _ copts = return $ err "Invalid arguments"
	-- get detailed info about symbol in source file
	whois' as [nm] copts = errorT $ do
		dbval <- getDb copts
		(srcFile, cabal) <- getCtx copts as
		liftM (ResultList . map ResultModuleDeclaration) $ whois dbval cabal srcFile nm
	whois' as _ copts = return $ err "Invalid arguments"
	-- get modules accessible from module
	scopeModules' as [] copts = errorT $ do
		dbval <- getDb copts
		(srcFile, cabal) <- getCtx copts as
		liftM (ResultList . map (ResultModuleId . moduleId)) $ scopeModules dbval cabal srcFile
	scopeModules' as _ copts = return $ err "Invalid arguments"
	-- get declarations accessible from module
	scope' as [] copts = errorT $ do
		dbval <- getDb copts
		(srcFile, cabal) <- getCtx copts as
		liftM (ResultList . map ResultModuleDeclaration . filterMatch as) $ scope dbval cabal srcFile (flag "global" as)
	scope' as _ copts = return $ err "Invalid arguments"
	-- completion
	complete' as [] copts = complete' as [""] copts
	complete' as [input] copts = errorT $ do
		dbval <- getDb copts
		(srcFile, cabal) <- getCtx copts as
		liftM (ResultList . map ResultModuleDeclaration) $ completions dbval cabal srcFile input
	complete' as _ copts = return $ err "Invalid arguments"
	-- hayoo
	hayoo' as [] copts = return $ err "Query not specified"
	hayoo' as [query] copts = errorT $
		liftM
			(ResultList . map (ResultModuleDeclaration . Hayoo.hayooAsDeclaration) . Hayoo.hayooFunctions) $
			Hayoo.hayoo query
	hayoo' as _ copts = return $ err "Too much arguments"
	-- cabal list
	cabalList' as qs copts = errorT $ do
		ps <- Cabal.cabalList qs
		return $ ResultList $ map (ResultJSON . toJSON) ps
	-- ghc-mod type
	ghcmodType' as [line] copts = ghcmodType' as [line, "1"] copts
	ghcmodType' as [line, column] copts = errorT $ do
		line' <- maybe (throwError "line must be a number") return $ readMaybe line
		column' <- maybe (throwError "column must be a number") return $ readMaybe column
		dbval <- getDb copts
		(srcFile, cabal) <- getCtx copts as
		(srcFile', m, mproj) <- fileCtx dbval srcFile
		tr <- GhcMod.typeOf (list "ghc" as) cabal srcFile' mproj (moduleName m) line' column'
		return $ ResultList $ map ResultTyped tr
	ghcmodType' as [] copts = return $ err "Specify line"
	ghcmodType' as _ copts = return $ err "Too much arguments"
	-- dump cabal modules
	dumpCabal' as _ copts = errorT $ do
		dbval <- getDb copts
		cabals <- getSandboxes copts as
		let
			dats = map (id &&& flip cabalDB dbval) cabals
		liftM (fromMaybe (ResultList $ map (ResultDatabase . snd) dats)) $
			runMaybeT $ msum [
				maybeOpt "path" as $ (lift . findPath copts) >=> \p ->
					fork (forM_ dats $ \(cabal, dat) -> (dump (p </> cabalCache cabal) dat)),
				maybeOpt "file" as $ (lift . findPath copts) >=> \f ->
					fork (dump f $ mconcat $ map snd dats)]
	-- dump projects
	dumpProjects' as [] copts = errorT $ do
		dbval <- getDb copts
		ps' <- traverse (findProject copts) $ list "project" as
		let
			ps = if null ps' then M.elems (databaseProjects dbval) else ps'
			dats = map (id &&& flip projectDB dbval) ps
		liftM (fromMaybe (ResultList $ map (ResultDatabase . snd) dats)) $
			runMaybeT $ msum [
				maybeOpt "path" as $ (lift . findPath copts) >=> \p ->
					fork (forM_ dats $ \(proj, dat) -> (dump (p </> projectCache proj) dat)),
				maybeOpt "file" as $ (lift . findPath copts) >=> \f ->
					fork (dump f (mconcat $ map snd dats))]
	dumpProjects' as _ copts = return $ err "Invalid arguments"
	-- dump files
	dumpFiles' as [] copts = errorT $ do
		dbval <- getDb copts
		let
			dat = standaloneDB dbval
		liftM (fromMaybe $ ResultDatabase dat) $ runMaybeT $ msum [
			maybeOpt "path" as $ (lift . findPath copts) >=> \p ->
				fork (dump (p </> standaloneCache) dat),
			maybeOpt "file" as $ (lift . findPath copts) >=> \f ->
				fork (dump f dat)]
	dumpFiles' as _ copts = return $ err "Invalid arguments"
	-- dump database
	dump' as _ copts = errorT $ do
		dbval <- getDb copts
		liftM (fromMaybe $ ResultDatabase dbval) $ runMaybeT $ msum [
			do
				p <- MaybeT $ traverse (findPath copts) $ arg "path" as
				fork $ SC.dump p $ structurize dbval
				return ResultNone,
			do
				f <- MaybeT $ traverse (findPath copts) $ arg "file" as
				fork $ dump f dbval
				return ResultNone]
	-- load database
	load' as _ copts = do
		res <- liftM (fromMaybe (err "Specify one of: --path, --file or --data")) $ runMaybeT $ msum [
			do
				p <- MaybeT $ return $ arg "path" as
				forkOrWait as $ cacheLoad copts (liftA merge <$> SC.load p)
				return ok,
			do
				f <- MaybeT $ return $ arg "file" as
				e <- liftIO $ doesFileExist f
				forkOrWait as $ when e $ cacheLoad copts (load f)
				return ok,
			do
				dat <- MaybeT $ return $ arg "data" as
				forkOrWait as $ cacheLoad copts (return $ eitherDecode (toUtf8 dat))
				return ok]
		waitDb copts as
		return res
	-- link to server
	link' as _ copts = do
		commandLink copts
		when (flag "hold" as) $ commandHold copts
		return ok
	-- exit
	exit' _ copts = do
		commandExit copts
		return ok

	-- Helper functions
	cmd' :: [String] -> [String] -> String -> [OptDescr (Opts String)] -> (Opts String -> [String] -> a) -> Command (WithOpts a)
	cmd' name posArgs descr as act = cmd name posArgs descr as act' where
		act' os args = WithOpts (act os args) $ CommandCall name args os

	cmd_' :: [String] -> [String] -> String -> ([String] -> a) -> Command (WithOpts a)
	cmd_' name posArgs descr act = cmd_ name posArgs descr act' where
		act' args = WithOpts (act args) $ CommandCall name args defaultConfig

	findSandbox :: MonadIO m => CommandOptions -> Maybe FilePath -> ErrorT String m Cabal
	findSandbox copts = maybe
		(return Cabal)
		(findPath copts >=> mapErrorT liftIO . locateSandbox)

	findPath :: MonadIO m => CommandOptions -> FilePath -> ErrorT String m FilePath
	findPath copts f = liftIO $ canonicalizePath (normalise f') where
		f'
			| isRelative f = commandRoot copts </> f
			| otherwise = f

	getCtx :: (MonadIO m, Functor m) => CommandOptions -> Opts String -> ErrorT String m (FilePath, Cabal)
	getCtx copts as = liftM2 (,)
		(forceJust "No file specified" $ traverse (findPath copts) $ arg "file" as)
		(getCabal copts as)

	getCabal :: MonadIO m => CommandOptions -> Opts String -> ErrorT String m Cabal
	getCabal copts as
		| flag "cabal" as = findSandbox copts Nothing
		| otherwise  = findSandbox copts $ arg "sandbox" as

	getCabal_ :: (MonadIO m, Functor m) => CommandOptions -> Opts String -> ErrorT String m (Maybe Cabal)
	getCabal_ copts as
		| flag "cabal" as = Just <$> findSandbox copts Nothing
		| otherwise = case arg "sandbox" as of
			Just f -> Just <$> findSandbox copts (Just f)
			Nothing -> return Nothing

	getSandboxes :: (MonadIO m, Functor m) => CommandOptions -> Opts String -> ErrorT String m [Cabal]
	getSandboxes copts as = traverse (findSandbox copts) paths where
		paths
			| flag "cabal" as = Nothing : sboxes
			| otherwise = sboxes
		sboxes = map Just $ list "sandbox" as

	findProject :: MonadIO m => CommandOptions -> String -> ErrorT String m Project
	findProject copts proj = do
		db' <- getDb copts
		proj' <- liftM addCabal $ findPath copts proj
		let
			result =
				M.lookup proj' (databaseProjects db') <|>
				find ((== proj) . projectName) (M.elems $ databaseProjects db')
		maybe (throwError $ "Projects " ++ proj ++ " not found") return result
		where
			addCabal p
				| takeExtension p == ".cabal" = p
				| otherwise = p </> (takeBaseName p <.> "cabal")

	toPair :: Module -> Maybe (FilePath, Module)
	toPair m = case moduleLocation m of
		FileModule f _ -> Just (f, m)
		_ -> Nothing

	modCabal :: Module -> Maybe Cabal
	modCabal m = case moduleLocation m of
		CabalModule c _ _ -> Just c
		_ -> Nothing

	waitDb copts as = when (flag "wait" as) $ do
		commandLog copts "wait for db"
		DB.wait (dbVar copts)
		commandLog copts "db done"

	forkOrWait as act
		| flag "wait" as = liftIO act
		| otherwise = liftIO $ void $ forkIO act

	cacheLoad copts act = do
		db' <- act
		case db' of
			Left e -> commandLog copts e
			Right database -> DB.update (dbVar copts) (return database)

	localsDatabase :: Opts String -> Database -> Database
	localsDatabase as
		| flag "locals" as = databaseLocals
		| otherwise = id

	newest :: Symbol a => Opts String -> [a] -> [a]
	newest as
		| flag "no-last" as = id
		| otherwise = newestPackage

	forceJust :: MonadIO m => String -> ErrorT String m (Maybe a) -> ErrorT String m a
	forceJust msg act = act >>= maybe (throwError msg) return

	getDb :: (MonadIO m) => CommandOptions -> m Database
	getDb = liftIO . DB.readAsync . commandDatabase

	dbVar :: CommandOptions -> DB.Async Database
	dbVar = commandDatabase

	startProcess :: Opts String -> ((Update.Status -> IO ()) -> IO ()) -> IO CommandResult
	startProcess as f
		| flag "wait" as = return $ ResultProcess (f . onMsg)
		| otherwise = forkIO (f $ const $ return ()) >> return ok
		where
			onMsg showMsg
				| flag "status" as = showMsg
				| otherwise = const $ return ()
	error_ :: ErrorT String IO CommandResult -> IO CommandResult
	error_ = liftM (either err id) . runErrorT

	errorT :: ErrorT String IO ResultValue -> IO CommandResult
	errorT = liftM (either err ResultOk) . runErrorT

	errorT' :: ErrorT CommandResult IO ResultValue -> IO CommandResult
	errorT' = liftM (either id ResultOk) . runErrorT

	updateProcess :: CommandOptions -> Opts String -> ErrorT String (Update.UpdateDB IO) () -> IO CommandResult
	updateProcess opts as act = startProcess as $ \onStatus ->
		Update.updateDB
			(Update.Settings
				(commandDatabase opts)
				(commandReadCache opts)
				onStatus
				(list "ghc" as))
			act

	fork :: MonadIO m => IO () -> m ()
	fork = voidm . liftIO . forkIO

	voidm :: Monad m => m a -> m ()
	voidm act = act >> return ()

	maybeOpt :: Monad m => String -> Opts String -> (String -> MaybeT m a) -> MaybeT m ResultValue
	maybeOpt n as act = do
		p <- MaybeT $ return $ arg n as
		act p
		return ResultNone

	filterMatch :: Opts String -> [ModuleDeclaration] -> [ModuleDeclaration]
	filterMatch as = findMatch as . prefMatch as

	findMatch :: Opts String -> [ModuleDeclaration] -> [ModuleDeclaration]
	findMatch as = case arg "find" as of
		Nothing -> id
		Just str -> filter (match' str)
		where
			match' str m = str `isInfixOf` declarationName (moduleDeclaration m)

	prefMatch :: Opts String -> [ModuleDeclaration] -> [ModuleDeclaration]
	prefMatch as = case fmap splitIdentifier (arg "prefix" as) of
		Nothing -> id
		Just (qname, pref) -> filter (match' qname pref)
		where
			match' qname pref m =
				pref `isPrefixOf` declarationName (moduleDeclaration m) &&
				maybe True (== moduleIdName (declarationModuleId m)) qname

processCmd :: CommandOptions -> Int -> String -> (Response -> IO ()) -> IO ()
processCmd copts tm cmdLine sendResponse = processCmdArgs copts tm (splitArgs cmdLine) sendResponse

-- | Process command, returns 'False' if exit requested
processCmdArgs :: CommandOptions -> Int -> [String] -> (Response -> IO ()) -> IO ()
processCmdArgs copts tm cmdArgs sendResponse = run (map (fmap withOptsAct) commands) (asCmd unknownCommand) (asCmd . commandError) cmdArgs tm copts >>= sendResponses where
	asCmd :: CommandResult -> (Int -> CommandOptions -> IO CommandResult)
	asCmd r _ _ = return r

	unknownCommand :: CommandResult
	unknownCommand = err "Unknown command"
	commandError :: [String] -> CommandResult
	commandError errs = errArgs "Command syntax error" [("what", ResultList $ map ResultString errs)]

	sendResponses :: CommandResult -> IO ()
	sendResponses (ResultOk v) = sendResponse $ Response $ toJSON v
	sendResponses (ResultError e args) = sendResponse $ Response $ object [
		"error" .= e,
		"details" .= args]
	sendResponses (ResultProcess act) = do
		act (sendResponse . ResponseStatus)
		sendResponses ok
		`catch`
		processFailed
		where
			processFailed :: SomeException -> IO ()
			processFailed e = sendResponses $ errArgs "process throws exception" [
				("exception", ResultString $ show e)]

hGetLine' :: Handle -> IO ByteString
hGetLine' = fmap L.fromStrict . B.hGetLine

race :: [IO ()] -> IO ()
race acts = do
	v <- newEmptyMVar
	forM_ acts $ \a -> forkIO ((a `finally` putMVar v ()) `catch` ignoreError)
	takeMVar v
	where
		ignoreError :: SomeException -> IO ()
		ignoreError _ = return ()

logIO :: String -> (String -> IO ()) -> IO () -> IO ()
logIO pre out act = handle onIO act where
	onIO :: IOException -> IO ()
	onIO e = out $ pre ++ show e

ignoreIO :: IO () -> IO ()
ignoreIO = handle (const (return ()) :: IOException -> IO ())

logMsg :: ServerOpts -> String -> IO ()
logMsg sopts s = ignoreIO $ do
	putStrLn s
	case getFirst (serverLog sopts) of
		Nothing -> return ()
		Just f -> withFile f AppendMode (`hPutStrLn` s)
