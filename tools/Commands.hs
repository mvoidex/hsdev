{-# LANGUAGE OverloadedStrings, CPP #-}

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
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
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

import qualified HsDev.Database.Async as DB
import HsDev.Commands
import HsDev.Database
import HsDev.Project
import HsDev.Symbols
import HsDev.Symbols.Util
import HsDev.Util
import HsDev.Scan
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
	| all (`notElem` "\"'") str = "'" ++ str ++ "'"
	| otherwise = "'" ++ translate str ++ "'"

#endif

-- | Main commands
mainCommands :: [Command (IO ())]
mainCommands = addHelp "hsdev" id $ srvCmds ++ map wrapCmd commands where
	wrapCmd :: Command CommandAction -> Command (IO ())
	wrapCmd = fmap sendCmd . addClientOpts . fmap withOptsArgs
	srvCmds = [
		cmd_ ["run"] [] "run interactive" runi',
		cmd ["server", "start"] [] "start remote server" serverOpts start',
		cmd ["server", "run"] [] "start server" serverOpts run',
		cmd ["server", "stop"] [] "stop remote server" clientOpts stop']

	runi' _ = do
		dir <- getCurrentDirectory
		db <- DB.newAsync
		forever $ do
			s <- getLine
			processCmd (CommandOptions db (const $ return ()) (const $ return Nothing) dir putStrLn getLine (error "Not supported") exitSuccess) 1000 s (L.putStrLn . encode)
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
	run' sopts _ = do
		msgs <- F.newChan
		outputDone <- newEmptyMVar
		forkIO $ finally
			(F.readChan msgs >>= mapM_ (logMsg sopts))
			(putMVar outputDone ())

		let
			outputStr = F.putChan msgs
			waitOutput = F.closeChan msgs >> takeMVar outputDone
			withCache :: a -> (FilePath -> IO a) -> IO a
			withCache v onCache = case getFirst (serverCache sopts) of
				Nothing -> return v
				Just cdir -> onCache cdir
			writeCache :: Database -> IO ()
			writeCache d = withCache () $ \cdir -> do
				outputStr "writing cache"
				SC.dump cdir (structurize d)
			readCache :: (FilePath -> ErrorT String IO Structured) -> IO (Maybe Database)
			readCache act = withCache Nothing $ join . liftM (either cacheErr cacheOk) . runErrorT . act where
				cacheErr e = outputStr ("Unable read cache: " ++ e) >> return Nothing
				cacheOk s = do
					forM_ (M.keys (structuredCabals s)) $ \c -> outputStr ("cache read: cabal " ++ show c)
					forM_ (M.keys (structuredProjects s)) $ \p -> outputStr ("cache read: project " ++ p)
					case allModules (structuredFiles s) of
						[] -> return ()
						ms -> outputStr $ "cache read: " ++ show (length ms) ++ " files"
					return $ Just $ merge s

		outputStr $ "Server started at port " ++ show (fromJust $ getFirst $ serverPort sopts)

		logIO "server exception: " outputStr $ flip finally waitOutput $ do
			db <- DB.newAsync

			when (getAny $ serverLoadCache sopts) $ withCache () $ \cdir -> do
				outputStr $ "Loading cache from " ++ cdir
				dbCache <- liftA merge <$> SC.load cdir
				case dbCache of
					Left err -> outputStr $ "Failed to load cache: " ++ err
					Right dbCache' -> DB.update db (return dbCache')

			waitListen <- newEmptyMVar
			clientChan <- F.newChan

			linkChan <- F.newChan
			let
				linkToSrv :: IO ()
				linkToSrv = do
					v <- newEmptyMVar
					F.putChan linkChan (putMVar v ())
					takeMVar v

#if mingw32_HOST_OS
			mmapPool <- createPool "hsdev"
			let
				-- | Send response as is or via memory mapped file
				sendResponse :: Handle -> Response -> IO ()
				sendResponse h r@(ResponseMapFile _) = L.hPutStrLn h $ encode r
				sendResponse h r
					| L.length msg <= 1024 = L.hPutStrLn h msg
					| otherwise = do
						sync <- newEmptyMVar
						forkIO $ void $ withName mmapPool $ \mmapName -> do
							runErrorT $ flip catchError
								(\e -> liftIO $ do
									sendResponse h $ Response $ object ["error" .= e]
									putMVar sync ())
								(withMapFile mmapName (L.toStrict msg) $ liftIO $ do
									sendResponse h $ ResponseMapFile mmapName
									putMVar sync ()
									-- Dirty: give 10 seconds for client to read it
									threadDelay 10000000)
						takeMVar sync
					where
						msg = encode r
#else
			let
				sendResponse h = L.hPutStrLn h . encode
#endif

			forkIO $ do
				accepter <- myThreadId

				let
					serverStop = void $ forkIO $ do
						void $ tryPutMVar waitListen ()
						killThread accepter

				s <- socket AF_INET Stream defaultProtocol
				bind s (SockAddrInet (fromIntegral $ fromJust $ getFirst $ serverPort sopts) iNADDR_ANY)
				listen s maxListenQueue
				forever $ logIO "accept client exception: " outputStr $ do
					s' <- fmap fst $ accept s
					outputStr $ show s' ++ " connected"
					void $ forkIO $ logIO (show s' ++ " exception: ") outputStr $
						bracket (socketToHandle s' ReadWriteMode) hClose $ \h -> do
							bracket newEmptyMVar (`putMVar` ()) $ \done -> do
								me <- myThreadId
								let
									timeoutWait = do
										notDone <- isEmptyMVar done
										when notDone $ do
											void $ forkIO $ do
												threadDelay 10000000
												tryPutMVar done ()
												killThread me
											void $ takeMVar done
								F.putChan clientChan timeoutWait
								req <- hGetLine' h
								outputStr $ show s' ++ ": " ++ fromUtf8 req
								case fmap extractCurrentDir $ eitherDecode req of
									Left reqErr -> sendResponse h $ Response $ object [
										"error" .= ("Invalid request" :: String),
										"request" .= fromUtf8 req,
										"what" .= reqErr]
									Right (clientDir, reqArgs) -> processCmdArgs
										(CommandOptions
											db
											writeCache
											readCache
											clientDir
											outputStr
											(fromUtf8 <$> hGetLine' h)
											linkToSrv
											serverStop)
										(fromJust $ getFirst $ serverTimeout sopts) reqArgs (sendResponse h)
								-- Send 'end' message and wait client
								L.hPutStrLn h L.empty
								outputStr $ "waiting " ++ show s'
								ignoreIO $ void $ hGetLine' h
								outputStr $ show s' ++ " disconnected"

			takeMVar waitListen
			withCache () $ \cdir -> do
				outputStr $ "saving cache to " ++ cdir
				logIO "cache saving exception: " outputStr $ do
					dbval <- DB.readAsync db
					SC.dump cdir $ structurize dbval
				outputStr "cache saved"
			outputStr "closing links"
			F.stopChan linkChan >>= sequence_
			outputStr "waiting for clients"
			F.stopChan clientChan >>= sequence_
			outputStr "server shutdown"
	stop' copts _ = run (map wrapCmd' commands) onDef onError ["exit"] where
		onDef = putStrLn "Command 'exit' not found"
		onError es = putStrLn $ "Failed to stop server: " ++ intercalate ", " es
		wrapCmd' = fmap (sendCmd . fmap ((,) copts) . withOptsArgs)

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

	-- Send command to server
	sendCmd :: IO (ClientOpts, [String]) -> IO ()
	sendCmd get' = do
		svar <- newEmptyMVar
		race [takeMVar svar, waitResponse >> putMVar svar ()]
		where
			waitResponse = do
				curDir <- getCurrentDirectory
				(p, as) <- get'
				stdinData <- if getAny (clientData p)
					then do
						cdata <- liftM eitherDecode L.getContents
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
				L.hPutStrLn h $ encode $ ["--current-directory=" ++ curDir] ++ setData stdinData as
				responses <- liftM (takeWhile (not . L.null) . L.lines) $ L.hGetContents h
				forM_ responses $
					parseResponse >=>
					(L.putStrLn . encodeValue (getAny $ clientPretty p))

			setData :: Maybe ResultValue -> [String] -> [String]
			setData Nothing = id
			setData (Just d) = (++ ["--data=" ++ (fromUtf8 $ encode d)])

			parseResponse r = fmap (either err' id) $ runErrorT $ do
				v <- errT (eitherDecode r) `orFail` (\e -> "Can't decode response")
				case v of
					Response rv -> return rv
					ResponseStatus sv -> return sv
#if mingw32_HOST_OS
					ResponseMapFile viewFile -> do
						str <- fmap L.fromStrict (readMapFile viewFile) `orFail`
							(\e -> "Can't read map view of file")
						lift $ parseResponse str
#else
					ResponseMapFile viewFile -> return $ err' ("Not supported" :: String)
#endif
				where
					errT act = ErrorT $ return act
					orFail act msg = act `catchError` (throwError . msg)
					err' msg = object ["error" .= msg]

			encodeValue True = encodePretty
			encodeValue False = encode

	-- Add parsing 'ClieptOpts'
	addClientOpts :: Command (IO [String]) -> Command (IO (ClientOpts, [String]))
	addClientOpts c = c { commandRun = run' } where
		run' args = fmap (fmap (fmap $ (,) p)) $ commandRun c args' where
			(ps, args', _) = getOpt RequireOrder clientOpts args
			p = mconcat ps `mappend` defaultConfig

	extractCurrentDir :: [String] -> (FilePath, [String])
	extractCurrentDir as = (head $ cur ++ ["."], as') where
		(cur, as', _) = getOpt RequireOrder curDirOpts as
		curDirOpts = [Option [] ["current-directory"] (ReqArg id "path") "current directory"]

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
		-- Database commands
		cmd' ["add"] [] "add info to database" [dataArg] add',
		cmd' ["scan", "cabal"] [] "scan modules installed in cabal" [
			sandbox, ghcOpts, wait, status] scanCabal',
		cmd' ["scan", "module"] ["module name"] "scan module in cabal" [sandbox, ghcOpts] scanModule',
		cmd' ["scan"] [] "scan sources" [
			projectArg "project path or .cabal",
			fileArg "source file",
			pathArg "directory to scan for files and projects",
			ghcOpts, wait, status] scan',
		cmd' ["rescan"] [] "rescan sources" [
			projectArg "project path or .cabal",
			projectNameArg "project name",
			fileArg "source file",
			pathArg "path to rescan",
			ghcOpts, wait, status] rescan',
		cmd' ["remove"] [] "remove modules info" [
			sandbox,
			projectArg "module project",
			projectNameArg "module project by name",
			fileArg "module source file",
			moduleArg,
			packageArg,
			allFlag] remove',
		-- | Context free commands
		cmd' ["list", "modules"] [] "list modules" [
			projectArg "project to list modules from",
			projectNameArg "project name to list modules from",
			sandbox, sourced, standaloned] listModules',
		cmd_' ["list", "projects"] [] "list projects" listProjects',
		cmd' ["symbol"] ["name"] "get symbol info" (matches ++ [
			projectArg "related project",
			projectNameArg "related project name",
			fileArg "source file",
			moduleArg,
			packageArg,
			sandbox, sourced, standaloned]) symbol',
		cmd' ["module"] [] "get module info" [
			moduleArg,
			packageArg,
			projectArg "module project",
			projectNameArg "module project name",
			fileArg "module source file",
			sandbox, sourced] modul',
		cmd_' ["project"] ["project name"] "show project info" project',
		-- Context commands
		cmd' ["lookup"] ["symbol"] "lookup for symbol" ctx lookup',
		cmd' ["whois"] ["symbol"] "get info for symbol" ctx whois',
		cmd' ["scope", "modules"] [] "get modules accesible from module or within a project" ctx scopeModules',
		cmd' ["scope"] [] "get declarations accessible from module or within a project" (ctx ++ matches ++ [globalArg]) scope',
		cmd' ["complete"] ["input"] "show completions for input" ctx complete',
		-- Dump/load commands
		cmd' ["dump", "cabal"] [] "dump cabal modules" [sandbox, cacheDir, cacheFile] dumpCabal',
		cmd' ["dump", "projects"] [] "dump projects" [projectArg "project .cabal", projectNameArg "project name", cacheDir, cacheFile] dumpProjects',
		cmd' ["dump", "files"] [] "dump standalone files" [cacheDir, cacheFile] dumpFiles',
		cmd' ["dump"] [] "dump whole database" [cacheDir, cacheFile] dump',
		cmd' ["load"] [] "load data" [cacheDir, cacheFile, dataArg, wait] load',
		-- Exit
		cmd_' ["exit"] [] "exit" exit']
	linkCmd = [cmd' ["link"] [] "link to server" [] link']

	-- Command arguments and flags
	allFlag = option_ ['a'] "all" flag "remove all"
	cacheDir = pathArg "cache path"
	cacheFile = fileArg "cache file"
	ctx = [fileArg "source file", sandbox]
	dataArg = option_ [] "data" (req "contents") "data to pass to command"
	fileArg = option_ ['f'] "file" (req "file")
	findArg = option_ [] "find" (req "find") "infix match"
	ghcOpts = option_ ['g'] "ghc" (req "ghc options") "options to pass to GHC"
	globalArg = option_ [] "global" flag "scope of project"
	matches = [prefixArg, findArg]
	moduleArg = option_ ['m'] "module" (req "module name") "module name"
	packageArg = option_ [] "package" (req "package") "module package"
	pathArg = option_ ['p'] "path" (req "path")
	prefixArg = option_ [] "prefix" (req "prefix") "prefix match"
	projectArg = option [] "project" ["proj"] (req "project")
	projectNameArg = option [] "project-name" ["proj-name"] (req "name")
	sandbox = option_ [] "sandbox" (noreq "path") "path to cabal sandbox"
	sourced = option_ [] "src" flag "source files"
	standaloned = option_ [] "stand" flag "standalone files"
	status = option_ ['s'] "status" flag "show status of operation, works only with --wait"
	wait = option_ ['w'] "wait" flag "wait for operation to complete"

	-- add data
	add' as _ copts = do
		dbval <- getDb copts
		res <- runErrorT $ do
			jsonData <- maybe (throwError $ err "Specify --data") return $ askOpt "data" as
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
		mapM_ (\(n, f) -> forM_ (askOpts n as) (canonicalizePath' copts >=> f (getGhcOpts as))) [
			("project", Update.scanProject),
			("file", Update.scanFile),
			("path", Update.scanDirectory)]
	-- scan cabal
	scanCabal' as _ copts = error_ $ do
		cabal <- getCabal copts as
		lift $ updateProcess copts as $ Update.scanCabal (getGhcOpts as) cabal
	-- scan cabal module
	scanModule' as [] copts = return $ err "Module name not specified"
	scanModule' as ms copts = error_ $ do
		cabal <- getCabal copts as
		lift $ updateProcess copts as $
			forM_ ms (Update.scanModule (getGhcOpts as) . CabalModule cabal Nothing)
	-- rescan
	rescan' as _ copts = do
		dbval <- getDb copts
		let
			fileMap = M.fromList $ mapMaybe toPair $
				selectModules (byFile . moduleId) dbval

		(errors, filteredMods) <- liftM partitionEithers $ mapM runErrorT $ concat [
			do
				p <- askOpts "project" as
				return $ do
					p' <- getProject copts p
					return $ M.fromList $ mapMaybe toPair $
						selectModules (inProject p' . moduleId) dbval,
			do
				f <- askOpts "file" as
				return $ maybe
					(throwError $ "Unknown file: " ++ f)
					(return . M.singleton f)
					(lookupFile f dbval),
			do
				d <- askOpts "path" as
				return $ return $ M.filterWithKey (\f _ -> isParent d f) fileMap]
		let
			rescanMods = map (getInspected dbval) $
				M.elems $ if null filteredMods then fileMap else M.unions filteredMods

		if not (null errors)
			then return $ err $ intercalate ", " errors
			else updateProcess copts as $ Update.runTask (toJSON $ ("rescanning modules" :: String)) $ do
				needRescan <- Update.liftErrorT $ filterM (changedModule dbval (getGhcOpts as) . inspectedId) rescanMods
				Update.scanModules (getGhcOpts as) (map (inspectedId &&& inspectionOpts . inspection) needRescan)
	-- remove
	remove' as _ copts = errorT $ do
		dbval <- liftIO $ getDb copts
		cabal <- askCabal copts as
		proj <- askProject copts as
		file <- traverse (canonicalizePath' copts) $ askOpt "file" as
		let
			cleanAll = hasOpt "all" as
			filters = catMaybes [
				fmap inProject proj,
				fmap inFile file,
				fmap inModule (askOpt "module" as),
				fmap inPackage (askOpt "package" as),
				fmap inCabal cabal]
			toClean = filter (allOf filters . moduleId) (allModules dbval)
			action
				| null filters && cleanAll = liftIO $ do
					DB.modifyAsync (dbVar copts) DB.Clear
					return ResultNone
				| null filters && not cleanAll = throwError "Specify filter or explicitely set flag --all"
				| cleanAll = throwError "--all flag can't be set with filters"
				| otherwise = liftIO $ do
					mapM_ (DB.modifyAsync (dbVar copts) . DB.Remove . fromModule) $ map (getInspected dbval) $ toClean
					return $ ResultList $ map (ResultModuleId . moduleId) toClean
		action
	-- list modules
	listModules' as _ copts = errorT $ do
		dbval <- liftIO $ getDb copts
		proj <- askProject copts as
		cabal <- askCabal copts as
		let
			filters = allOf $ catMaybes [
				fmap inProject proj,
				fmap inCabal cabal,
				if hasOpt "src" as then Just byFile else Nothing,
				if hasOpt "stand" as then Just standalone else Nothing]
		return $ ResultList $ map (ResultModuleId . moduleId) $ selectModules (filters . moduleId) dbval
	-- list projects
	listProjects' _ copts = do
		dbval <- getDb copts
		return $ ResultOk $ ResultList $ map ResultProject $ M.elems $ databaseProjects dbval
	-- get symbol info
	symbol' as ns copts = errorT $ do
		dbval <- liftIO $ getDb copts
		proj <- askProject copts as
		file <- traverse (canonicalizePath' copts) $ askOpt "file" as
		cabal <- askCabal copts as
		let
			filters = checkModule $ allOf $ catMaybes [
				fmap inProject proj,
				fmap inFile file,
				fmap inModule (askOpt "module" as),
				fmap inPackage (askOpt "package" as),
				fmap inCabal cabal,
				if hasOpt "src" as then Just byFile else Nothing,
				if hasOpt "stand" as then Just standalone else Nothing]
			toResult = ResultList . map ResultModuleDeclaration . filterMatch as . filter filters
		case ns of
			[] -> return $ toResult $ allDeclarations dbval
			[nm] -> liftM toResult (findDeclaration dbval nm) `catchError` (\e ->
				throwError ("Can't find symbol: " ++ e))
			_ -> throwError "Too much arguments"
	-- get module info
	modul' as _ copts = errorT' $ do
		dbval <- liftIO $ getDb copts
		proj <- mapErrorT (fmap $ strMsg +++ id) $ askProject copts as
		cabal <- mapErrorT (fmap $ strMsg +++ id) $ askCabal copts as
		file' <- traverse (canonicalizePath' copts) $ askOpt "file" as
		let
			filters = allOf $ catMaybes [
				fmap inProject proj,
				fmap inCabal cabal,
				fmap inFile file',
				fmap inModule (askOpt "module" as),
				fmap inPackage (askOpt "package" as),
				if hasOpt "src" as then Just byFile else Nothing]
		rs <- mapErrorT (fmap $ strMsg +++ id) $
			filter (filters . moduleId) <$> maybe
				(return $ allModules dbval)
				(findModule dbval)
				(askOpt "module" as)
		case rs of
			[] -> throwError $ err "Module not found"
			[m] -> return $ ResultModule m
			ms' -> throwError $ errArgs "Ambiguous modules" [("modules", ResultList $ map (ResultModuleId . moduleId) ms')]
	-- get project info
	project' [] copts = return $ err "Project name not specified"
	project' pnames copts = do
		dbval <- getDb copts
		case filter ((`elem` pnames) . projectName) $ M.elems $ databaseProjects dbval of
			[] -> return $ errArgs "Projects not found" [
				("projects", ResultList $ map ResultString pnames)]
			ps -> return $ ResultOk $ ResultList $ map ResultProject ps
	-- lookup info about symbol
	lookup' as [nm] copts = errorT $ do
		dbval <- liftIO $ getDb copts
		(srcFile, cabal) <- askCtx copts as
		liftM (ResultList . map ResultModuleDeclaration) $ lookupSymbol dbval cabal srcFile nm
	lookup' as _ copts = return $ err "Invalid arguments"
	-- get detailed info about symbol in source file
	whois' as [nm] copts = errorT $ do
		dbval <- liftIO $ getDb copts
		(srcFile, cabal) <- askCtx copts as
		liftM (ResultList . map ResultModuleDeclaration) $ whois dbval cabal srcFile nm
	whois' as _ copts = return $ err "Invalid arguments"
	-- get modules accessible from module
	scopeModules' as [] copts = errorT $ do
		dbval <- liftIO $ getDb copts
		(srcFile, cabal) <- askCtx copts as
		liftM (ResultList . map (ResultModuleId . moduleId)) $ scopeModules dbval cabal srcFile
	scopeModules' as _ copts = return $ err "Invalid arguments"
	-- get declarations accessible from module
	scope' as [] copts = errorT $ do
		dbval <- liftIO $ getDb copts
		(srcFile, cabal) <- askCtx copts as
		liftM (ResultList . map ResultModuleDeclaration . filterMatch as) $ scope dbval cabal srcFile (hasOpt "global" as)
	scope' as _ copts = return $ err "Invalid arguments"
	-- completion
	complete' as [] copts = complete' as [""] copts
	complete' as [input] copts = errorT $ do
		dbval <- getDb copts
		(srcFile, cabal) <- askCtx copts as
		liftM (ResultList . map ResultModuleDeclaration) $ completions dbval cabal srcFile input
	complete' as _ copts = return $ err "Invalid arguments"
	-- dump cabal modules
	dumpCabal' as _ copts = errorT $ do
		dbval <- liftIO $ getDb copts
		cabal <- getCabal copts as
		let
			dat = cabalDB cabal dbval
		liftM (fromMaybe (ResultDatabase dat)) $ runMaybeT $ msum [
			maybeOpt "path" as $ canonicalizePath' copts >=> \p ->
				fork (dump (p </> cabalCache cabal) dat),
			maybeOpt "file" as $ canonicalizePath' copts >=> \f ->
				fork (dump f dat)]
	-- dump projects
	dumpProjects' as [] copts = errorT $ do
		dbval <- liftIO $ getDb copts
		ps' <- traverse (getProject copts) $ askOpts "project" as
		let
			ps = if null ps' then M.elems (databaseProjects dbval) else ps'
			dats = map (id &&& flip projectDB dbval) ps
		liftM (fromMaybe (ResultList $ map (ResultDatabase . snd) dats)) $
			runMaybeT $ msum [
				maybeOpt "path" as $ canonicalizePath' copts >=> \p ->
					fork (forM_ dats $ \(proj, dat) -> (dump (p </> projectCache proj) dat)),
				maybeOpt "file" as $ canonicalizePath' copts >=> \f ->
					fork (dump f (mconcat $ map snd dats))]
	dumpProjects' as _ copts = return $ err "Invalid arguments"
	-- dump files
	dumpFiles' as [] copts = do
		dbval <- getDb copts
		let
			dat = standaloneDB dbval
		liftM (ResultOk . fromMaybe (ResultDatabase dat)) $ runMaybeT $ msum [
			maybeOpt "path" as $ canonicalizePath' copts >=> \p ->
				fork (dump (p </> standaloneCache) dat),
			maybeOpt "file" as $ canonicalizePath' copts >=> \f ->
				fork (dump f dat)]
	dumpFiles' as _ copts = return $ err "Invalid arguments"
	-- dump database
	dump' as _ copts = do
		dbval <- getDb copts
		liftM (fromMaybe (ResultOk $ ResultDatabase dbval)) $ runMaybeT $ msum [
			do
				p <- MaybeT $ traverse (canonicalizePath' copts) $ askOpt "path" as
				fork $ SC.dump p $ structurize dbval
				return ok,
			do
				f <- MaybeT $ traverse (canonicalizePath' copts) $ askOpt "file" as
				fork $ dump f dbval
				return ok]
	-- load database
	load' as _ copts = do
		res <- liftM (fromMaybe (err "Specify one of: --path, --file or --data")) $ runMaybeT $ msum [
			do
				p <- MaybeT $ return $ askOpt "path" as
				forkOrWait as $ cacheLoad copts (liftA merge <$> SC.load p)
				return ok,
			do
				f <- MaybeT $ return $ askOpt "file" as
				e <- liftIO $ doesFileExist f
				forkOrWait as $ when e $ cacheLoad copts (load f)
				return ok,
			do
				dat <- MaybeT $ return $ askOpt "data" as
				forkOrWait as $ cacheLoad copts (return $ eitherDecode (toUtf8 dat))
				return ok]
		waitDb copts as
		return res
	-- link to server
	link' as _ copts = do
		race [void (commandWaitInput copts) `finally` commandExit copts, commandLink copts]
		return ok
	-- exit
	exit' _ copts = do
		commandExit copts
		return ok

	-- Helper functions
	cmd' :: [String] -> [String] -> String -> [OptDescr Opts] -> (Opts -> [String] -> a) -> Command (WithOpts a)
	cmd' name posArgs descr as act = cmd name posArgs descr as act' where
		act' os args = WithOpts (act os args) $
			return $ name ++ args ++ optsToArgs os

	cmd_' :: [String] -> [String] -> String -> ([String] -> a) -> Command (WithOpts a)
	cmd_' name posArgs descr act = cmd_ name posArgs descr act' where
		act' args = WithOpts (act args) $
			return $ name ++ args ++ optsToArgs defaultConfig

	getGhcOpts = askOpts "ghc"

	toPair :: Module -> Maybe (FilePath, Module)
	toPair m = case moduleLocation m of
		FileModule f _ -> Just (f, m)
		_ -> Nothing

	modCabal :: Module -> Maybe Cabal
	modCabal m = case moduleLocation m of
		CabalModule c _ _ -> Just c
		_ -> Nothing

	waitDb copts as = when (hasOpt "wait" as) $ do
		commandLog copts "wait for db"
		DB.wait (dbVar copts)
		commandLog copts "db done"

	forkOrWait as act
		| hasOpt "wait" as = liftIO act
		| otherwise = liftIO $ void $ forkIO act

	cacheLoad copts act = do
		db' <- act
		case db' of
			Left e -> commandLog copts e
			Right database -> DB.update (dbVar copts) (return database)

	asCabal :: CommandOptions -> Maybe FilePath -> ErrorT String IO Cabal
	asCabal copts = maybe
		(return Cabal)
		(canonicalizePath' copts >=> locateSandbox)

	askCabal :: CommandOptions -> Opts -> ErrorT String IO (Maybe Cabal)
	askCabal copts as = traverse (asCabal copts) $ askOptDef "sandbox" as

	getCabal :: CommandOptions -> Opts -> ErrorT String IO Cabal
	getCabal copts as = asCabal copts $ askOpt "sandbox" as

	getProject :: CommandOptions -> String -> ErrorT String IO Project
	getProject copts proj = do
		db' <- getDb copts
		proj' <- liftM addCabal $ canonicalizePath' copts proj
		let
			result =
				M.lookup proj' (databaseProjects db') <|>
				find ((== proj) . projectName) (M.elems $ databaseProjects db')
		maybe (throwError $ "Project " ++ proj ++ " not found") return result
		where
			addCabal p
				| takeExtension p == ".cabal" = p
				| otherwise = p </> (takeBaseName p <.> "cabal")

	askProject :: CommandOptions -> Opts -> ErrorT String IO (Maybe Project)
	askProject copts = traverse (getProject copts) . askOpt "project"

	askFile :: CommandOptions -> Opts -> ErrorT String IO (Maybe FilePath)
	askFile copts = traverse (canonicalizePath' copts) . askOpt "file"

	forceJust :: String -> ErrorT String IO (Maybe a) => ErrorT String IO a
	forceJust msg act = act >>= maybe (throwError msg) return

	askCtx :: CommandOptions -> Opts -> ErrorT String IO (FilePath, Cabal)
	askCtx copts as = liftM2 (,)
		(forceJust "No file specified" $ askFile copts as)
		(getCabal copts as)

	getDb :: (MonadIO m) => CommandOptions -> m Database
	getDb = liftIO . DB.readAsync . commandDatabase

	dbVar :: CommandOptions -> DB.Async Database
	dbVar = commandDatabase

	canonicalizePath' :: MonadIO m => CommandOptions -> FilePath -> m FilePath
	canonicalizePath' copts f = liftIO $ canonicalizePath (normalise f') where
		f'
			| isRelative f = commandRoot copts </> f
			| otherwise = f

	startProcess :: Opts -> ((Value -> IO ()) -> IO ()) -> IO CommandResult
	startProcess as f
		| hasOpt "wait" as = return $ ResultProcess (f . onMsg)
		| otherwise = forkIO (f $ const $ return ()) >> return ok
		where
			onMsg showMsg
				| hasOpt "status" as = showMsg
				| otherwise = const $ return ()

	error_ :: ErrorT String IO CommandResult -> IO CommandResult
	error_ = liftM (either err id) . runErrorT

	errorT :: ErrorT String IO ResultValue -> IO CommandResult
	errorT = liftM (either err ResultOk) . runErrorT

	errorT' :: ErrorT CommandResult IO ResultValue -> IO CommandResult
	errorT' = liftM (either id ResultOk) . runErrorT

	updateProcess :: CommandOptions -> Opts -> ErrorT String (Update.UpdateDB IO) () -> IO CommandResult
	updateProcess opts as act = startProcess as $ \onStatus -> Update.updateDB (Update.Settings (commandDatabase opts) (commandReadCache opts) onStatus (getGhcOpts as)) act

	fork :: MonadIO m => IO () -> m ()
	fork = voidm . liftIO . forkIO

	voidm :: Monad m => m a -> m ()
	voidm act = act >> return ()

	maybeOpt :: Monad m => String -> Opts -> (String -> MaybeT m a) -> MaybeT m ResultValue
	maybeOpt n as act = do
		p <- MaybeT $ return $ askOpt n as
		act p
		return ResultNone

	filterMatch :: Opts -> [ModuleDeclaration] -> [ModuleDeclaration]
	filterMatch as = findMatch as . prefMatch as

	findMatch :: Opts -> [ModuleDeclaration] -> [ModuleDeclaration]
	findMatch as = case askOpt "find" as of
		Nothing -> id
		Just str -> filter (match' str)
		where
			match' str m = str `isInfixOf` declarationName (moduleDeclaration m)

	prefMatch :: Opts -> [ModuleDeclaration] -> [ModuleDeclaration]
	prefMatch as = case fmap splitIdentifier (askOpt "prefix" as) of
		Nothing -> id
		Just (qname, pref) -> filter (match' qname pref)
		where
			match' qname pref m =
				pref `isPrefixOf` declarationName (moduleDeclaration m) &&
				maybe True (== moduleIdName (declarationModuleId m)) qname

processCmd :: CommandOptions -> Int -> String -> (Response -> IO ()) -> IO ()
processCmd copts tm cmdLine sendResponse = processCmdArgs copts tm (split cmdLine) sendResponse

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

fromUtf8 :: ByteString -> String
fromUtf8 = T.unpack . T.decodeUtf8

toUtf8 :: String -> ByteString
toUtf8 = T.encodeUtf8 . T.pack

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
