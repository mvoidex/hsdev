{-# LANGUAGE OverloadedStrings, CPP, PatternGuards #-}

module HsDev.Server.Commands (
	commands,
	serverOpts, serverDefCfg,
	clientOpts, clientDefCfg,

	clientCmd, sendCmd,

	initLog, runServer,
	processRequest, processClient,

	withCache, writeCache, readCache
	) where

import Control.Applicative
import Control.Arrow (second)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Error
import Data.Aeson hiding (Result, Error)
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Either (isLeft)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Traversable (traverse)
import Network.Socket hiding (connect)
import qualified Network.Socket as Net
import System.Directory
import System.Exit
import System.IO
import Text.Read (readMaybe)

import Control.Apply.Util
import Control.Concurrent.Util
import qualified Control.Concurrent.FiniteChan as F
import Data.Lisp
import System.Console.Cmd hiding (run)
import Text.Format ((~~), (%))

import qualified HsDev.Cache.Structured as SC
import qualified HsDev.Client.Commands as Client
import HsDev.Database
import qualified HsDev.Database.Async as DB
import HsDev.Tools.Ghc.Worker
import HsDev.Tools.GhcMod (ghcModMultiWorker)
import HsDev.Server.Message as M
import HsDev.Server.Types
import HsDev.Util

#if mingw32_HOST_OS
import Data.Aeson.Types hiding (Result, Error)
import Data.Char
import Data.List
import System.Environment
import System.Process
import System.Win32.FileMapping.Memory (withMapFile, readMapFile)
import System.Win32.FileMapping.NamePool
import System.Win32.PowerShell (escape, quote, quoteDouble)
#else
import System.Posix.Process
import System.Posix.IO
#endif

-- | Server commands
commands :: [Cmd (IO ())]
commands = [
	cmd' "start" server' "start remote server" start',
	cmd' "run" server' "run server" run',
	cmd' "stop" client' "stop remote server" stop',
	cmd' "connect" client' "connect to send commands directly" connect']
	where
		cmd' :: String -> ([Opt], Opts String) -> String -> (Args -> IO ()) -> Cmd (IO ())
		cmd' nm (opts', defOpts') desc' act' =
			cmd nm [] opts' desc' act' `with` [defaultOpts defOpts']

		server' = (serverOpts, serverDefCfg)
		client' = (clientOpts, clientDefCfg)

		-- | Start remote server
		start' :: Args -> IO ()
		start' (Args _ sopts) = do
#if mingw32_HOST_OS
			let
				args = ["run"] ++ toArgs (Args [] sopts)
			myExe <- getExecutablePath
			curDir <- getCurrentDirectory
			let
				-- one escape for start-process and other for callable process
				-- seems, that start-process just concats arguments into one string
				-- start-process foo 'bar baz' ⇒ foo bar baz -- not expected
				-- start-process foo '"bar baz"' ⇒ foo "bar baz" -- ok
				biescape = escape quote . escape quoteDouble
				script = "try { start-process $ $ -WindowStyle Hidden -WorkingDirectory $ } catch { $$_.Exception, $$_.InvocationInfo.Line }" ~~ (
					escape quote myExe %
					(intercalate ", " (map biescape args)) %
					escape quote curDir)
			r <- readProcess "powershell" [
				"-Command",
				script] ""
			if all isSpace r
				then putStrLn $ "Server started at port " ++ (fromJust $ arg "port" sopts)
				else mapM_ putStrLn [
					"Failed to start server",
					"\tCommand: " ++ script,
					"\tResult: " ++ r]
#else
			let
				forkError :: SomeException -> IO ()
				forkError e  = putStrLn $ "Failed to start server: " ++ show e

				proxy :: IO ()
				proxy = do
					_ <- createSession
					_ <- forkProcess serverAction
					exitImmediately ExitSuccess

				serverAction :: IO ()
				serverAction = do
					mapM_ closeFd [stdInput, stdOutput, stdError]
					nullFd <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
					mapM_ (dupTo nullFd) [stdInput, stdOutput, stdError]
					closeFd nullFd
					run' (Args [] sopts)

			handle forkError $ do
				_ <- forkProcess proxy
				putStrLn $ "Server started at port " ++ fromJust (arg "port" sopts)
#endif

		-- | Run server
		run' :: Args -> IO ()
		run' (Args _ sopts)
			| flagSet "as-client" sopts = runServer sopts $ \copts -> do
				commandLog copts $ "Server started as client connecting at port " ++ fromJust (arg "port" sopts)
				me <- myThreadId
				s <- socket AF_INET Stream defaultProtocol
				addr' <- inet_addr "127.0.0.1"
				Net.connect s $ SockAddrInet (fromIntegral $ fromJust $ iarg "port" sopts) addr'
				bracket (socketToHandle s ReadWriteMode) hClose $ \h ->
					processClientHandle s h (copts {
						commandExit = killThread me })
			| otherwise = runServer sopts $ \copts -> do
				commandLog copts $ "Server started at port " ++ fromJust (arg "port" sopts)

				waitListen <- newEmptyMVar
				clientChan <- F.newChan

				void $ forkIO $ do
					accepter <- myThreadId

					let
						serverStop :: IO ()
						serverStop = void $ forkIO $ do
							void $ tryPutMVar waitListen ()
							killThread accepter

					s <- socket AF_INET Stream defaultProtocol
					bind s $ SockAddrInet (fromIntegral $ fromJust $ iarg "port" sopts) iNADDR_ANY
					listen s maxListenQueue
					forever $ logIO "accept client exception: " (commandLog copts) $ do
						s' <- fst <$> accept s
						void $ forkIO $ logIO (show s' ++ " exception: ") (commandLog copts) $
							bracket (socketToHandle s' ReadWriteMode) hClose $ \h ->
							bracket newEmptyMVar (`putMVar` ()) $ \done -> do
								me <- myThreadId
								let
									timeoutWait = do
										notDone <- isEmptyMVar done
										when notDone $ do
											void $ forkIO $ do
												threadDelay 1000000
												void $ tryPutMVar done ()
												killThread me
											takeMVar done
									waitForever = forever $ hGetLineBS h
								F.putChan clientChan timeoutWait
								processClientHandle s' h (copts {
									commandHold = waitForever,
									commandExit = serverStop })

				takeMVar waitListen
				DB.readAsync (commandDatabase copts) >>= writeCache sopts (commandLog copts)
				F.stopChan clientChan >>= sequence_
				commandLog copts "server stopped"

		-- | Stop remote server
		stop' :: Args -> IO ()
		stop' (Args _ copts) = runArgs (map clientCmd Client.commands) onDef onError (Args ["exit"] copts) where
			onDef = putStrLn "Command 'exit' not found"
			onError es = putStrLn $ "Failed to stop server: " ++ es

		-- | Connect to remote server
		connect' :: Args -> IO ()
		connect' (Args _ copts) = do
			curDir <- getCurrentDirectory
			s <- socket AF_INET Stream defaultProtocol
			addr' <- inet_addr "127.0.0.1"
			Net.connect s (SockAddrInet (fromIntegral $ fromJust $ iarg "port" copts) addr')
			bracket (socketToHandle s ReadWriteMode) hClose $ \h -> forM_ [(1 :: Integer)..] $ \i -> ignoreIO $ do
				input' <- hGetLineBS stdin
				case decodeLispOrJSON input' of
					Left _ -> L.putStrLn $ encodeValue False $ object ["error" .= ("invalid command" :: String)]
					Right (isLisp, req') -> do
						L.hPutStrLn h $ encodeLispOrJSON isLisp $ Message (Just $ show i) $
							req' `M.withOpts` ["current-directory" %-- curDir]
						waitResp h
			where
				pretty = flagSet "pretty" copts

				encodeValue :: ToJSON a => Bool -> a -> L.ByteString
				encodeValue True = encodeLisp
				encodeValue False
					| pretty = encodePretty
					| otherwise = encode

				waitResp h = do
					resp <- hGetLineBS h
					parseResp h resp

				parseResp h str = case decodeLispOrJSON str of
					Left e -> putStrLn $ "Can't decode response: " ++ e
					Right (isLisp, Message i r) -> do
						r' <- unMmap r
						putStrLn $ fromMaybe "_" i ++ ":" ++ fromUtf8 (encodeValue isLisp r')
						case r of
							Left _ -> waitResp h
							_ -> return ()

-- | Server options
serverOpts :: [Opt]
serverOpts = [
	req "port" "number" `desc` "listen port",
	req "timeout" "msec" `desc` "query timeout",
	req "log" "file" `short` ['l'] `desc` "log file",
	req "cache" "path" `desc` "cache directory",
	flag "load" `desc` "force load all data from cache on startup"]

-- | Client options
clientOpts :: [Opt]
clientOpts = [
	req "port" "number" `desc` "connection port",
	flag "pretty" `desc` "pretty json output",
	req "stdin" "data" `desc` "pass data to stdin",
	req "timeout" "msec" `desc` "overwrite timeout duration",
	flag "silent" `desc` "supress notifications"]

-- | Server default options
serverDefCfg :: Opts String
serverDefCfg = mconcat [
	"port" %-- (4567 :: Int),
	"timeout" %-- (1000 :: Int)]

-- | Client default options
clientDefCfg :: Opts String
clientDefCfg = mconcat ["port" %-- (4567 :: Int)]

-- | Command to send to client
clientCmd :: Cmd CommandAction -> Cmd (IO ())
clientCmd c = cmd (cmdName c) (cmdArgs c) (cmdOpts c ++ clientOpts) (cmdDesc c) (sendCmd (cmdName c))
	`with` [defaultOpts clientDefCfg]

-- | Send command to server
sendCmd :: String -> Args -> IO ()
sendCmd name (Args args opts) = do
	var <- newEmptyMVar
	thId <- forkIO $ ignoreIO sendReceive >> putMVar var ()
	handle (\(SomeException _) -> killThread thId) $ takeMVar var
	where
		(copts, opts') = splitOpts clientOpts opts
		reqCall = Request name args opts'
		pretty = flagSet "pretty" copts
		encodeValue :: ToJSON a => a -> L.ByteString
		encodeValue
			| pretty = encodePretty
			| otherwise = encode
		sendReceive = do
			curDir <- getCurrentDirectory
			stdinData <- if flagSet "data" copts
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
			Net.connect s (SockAddrInet (fromIntegral $ fromJust $ iarg "port" copts) addr')
			bracket (socketToHandle s ReadWriteMode) hClose $ \h -> do
				L.hPutStrLn h $ encode $ Message Nothing $ reqCall `M.withOpts` [
					"current-directory" %-- curDir,
					"data" %-? (fromUtf8 . encode <$> stdinData),
					"timeout" %-? (iarg "timeout" copts :: Maybe Integer),
					if flagSet "silent" copts then hoist "silent" else mempty]
				hFlush h
				peekResponse h

		peekResponse h = do
			resp <- hGetLineBS h
			parseResponse h resp

		parseResponse h str = case decodeLispOrJSON str of
			Left e -> putStrLn $ "Can't decode response: " ++ e
			Right (_, Message _ r) -> do
				r' <- unMmap r
				L.putStrLn $ case r' of
					Left n -> encodeValue n
					Right (Result v) -> encodeValue v
					Right e -> encodeValue e
				when (isLeft r') $ peekResponse h

-- | Inits log chan and returns functions (print message, wait channel)
initLog :: Opts String -> IO (String -> IO (), ([String] -> IO ()) -> IO (), IO ())
initLog sopts = do
	msgs <- F.newChan
	outputDone <- newEmptyMVar
	void $ forkIO $ finally
		(F.readChan msgs >>= mapM_ (logMsg sopts))
		(putMVar outputDone ())
	let
		listenLog f = logException "listen log" (F.putChan msgs) $ do
			msgs' <- F.dupChan msgs
			F.readChan msgs' >>= f
	return (F.putChan msgs, listenLog, F.closeChan msgs >> takeMVar outputDone)

-- | Run server
runServer :: Opts String -> (CommandOptions -> IO ()) -> IO ()
runServer sopts act = bracket (initLog sopts) (\(_, _, x) -> x) $ \(outputStr, listenLog, waitOutput) -> do
	db <- DB.newAsync
	when (flagSet "load" sopts) $ withCache sopts () $ \cdir -> do
		outputStr $ "Loading cache from " ++ cdir
		dbCache <- liftA merge <$> SC.load cdir
		case dbCache of
			Left err -> outputStr $ "Failed to load cache: " ++ err
			Right dbCache' -> DB.update db (return dbCache')
#if mingw32_HOST_OS
	mmapPool <- Just <$> createPool "hsdev"
#endif
	ghcw <- ghcWorker
	ghcmodw <- ghcModMultiWorker
	act $ CommandOptions
		db
		(writeCache sopts outputStr)
		(readCache sopts outputStr)
		"."
		outputStr
		listenLog
		waitOutput
#if mingw32_HOST_OS
		mmapPool
#endif
		ghcw
		ghcmodw
		(const $ return ())
		(return ())
		(return ())
		(return ())

decodeLispOrJSON :: FromJSON a => ByteString -> Either String (Bool, a)
decodeLispOrJSON str =
	((,) <$> pure False <*> eitherDecode str) <|>
	((,) <$> pure True <*> decodeLisp str)

encodeLispOrJSON :: ToJSON a => Bool -> a -> ByteString
encodeLispOrJSON True = encodeLisp
encodeLispOrJSON False = encode

-- | Process request, notifications can be sent during processing
processRequest :: CommandOptions -> (Notification -> IO ()) -> Request -> IO Result
processRequest copts onNotify req' =
	runArgs
		Client.commands
		unknownCommand
		requestError
		(requestToArgs req')
		(copts { commandNotify = onNotify })
	where
		unknownCommand :: CommandAction
		unknownCommand _ = return $ Error "Unknown command" M.empty

		requestError :: String -> CommandAction
		requestError errs _ = return $ Error "Command syntax error" $ M.fromList [
			("what", toJSON $ lines errs)]

-- | Process client, listen for requests and process them
processClient :: String -> IO ByteString -> (ByteString -> IO ()) -> CommandOptions -> IO ()
processClient name receive send' copts = do
	commandLog copts $ name ++ " connected"
	respChan <- newChan
	void $ forkIO $ do
		responses <- getChanContents respChan
		mapM_ (send' . uncurry encodeLispOrJSON) responses
	linkVar <- newMVar $ return ()
	let
		answer :: Bool -> Message Response -> IO ()
		answer isLisp m@(Message i r) = do
			when (not $ isNotification r) $
				commandLog copts $ name ++ " << " ++ fromMaybe "_" i ++ ":" ++ fromUtf8 (encode r)
			writeChan respChan (isLisp, m)
	flip finally (disconnected linkVar) $ forever $ do
		req' <- receive
		case second (fmap extractMeta) <$> decodeLispOrJSON req' of
			Left _ -> do
				commandLog copts $ name ++ " >> #: " ++ fromUtf8 req'
				answer False $ Message Nothing $ responseError "Invalid request" [
					"request" .= fromUtf8 req']
			Right (isLisp, m) -> do
				resp' <- flip traverse m $ \(cdir, noFile, silent, tm, reqArgs) -> do
					let
						onNotify n
							| silent = return ()
							| otherwise = traverse (const $ mmap' noFile (Left n)) m >>= answer isLisp
					commandLog copts $ name ++ " >> " ++ fromMaybe "_" (messageId m) ++ ":" ++ fromUtf8 (encode reqArgs)
					resp <- fmap Right $ handleTimeout tm $ handleError $
						processRequest
							(copts {
								commandRoot = cdir,
								commandLink = void (swapMVar linkVar $ commandExit copts) })
							onNotify
							reqArgs
					mmap' noFile resp
				answer isLisp resp'
	where
		extractMeta :: Request -> (FilePath, Bool, Bool, Maybe Int, Request)
		extractMeta c = (cdir, noFile, silent, tm, c { requestOpts = opts' }) where
			cdir = fromMaybe (commandRoot copts) $ arg "current-directory" metaOpts
			noFile = flagSet "no-file" metaOpts
			silent = flagSet "silent" metaOpts
			tm = join $ fmap readMaybe $ arg "timeout" metaOpts
			(metaOpts, opts') = splitOpts [
				req "current-directory" "path",
				flag "no-file",
				flag "silent",
				req "timeout" "ms"]
				(requestOpts c)

		handleTimeout :: Maybe Int -> IO Result -> IO Result
		handleTimeout Nothing = id
		handleTimeout (Just tm) = fmap (fromMaybe $ Error "Timeout" M.empty) . timeout tm

		handleError :: IO Result -> IO Result
		handleError = handle onErr where
			onErr :: SomeException -> IO Result
			onErr e = return $ Error "Exception" $ M.fromList [("what", toJSON $ show e)]

		mmap' :: Bool -> Response -> IO Response
#if mingw32_HOST_OS
		mmap' False
			| Just pool <- commandMmapPool copts = mmap pool
#endif
		mmap' _ = return

		disconnected :: MVar (IO ()) -> IO ()
		disconnected var = do
			commandLog copts $ name ++ " disconnected"
			join $ takeMVar var

-- | Process client by Handle
processClientHandle :: Show a => a -> Handle -> CommandOptions -> IO ()
processClientHandle n h = processClient (show n) (hGetLineBS h) (\s -> L.hPutStrLn h s >> hFlush h)

-- | Perform action on cache
withCache :: Opts String -> a -> (FilePath -> IO a) -> IO a
withCache sopts v onCache = case arg "cache" sopts of
	Nothing -> return v
	Just cdir -> onCache cdir

writeCache :: Opts String -> (String -> IO ()) -> Database -> IO ()
writeCache sopts logMsg' d = withCache sopts () $ \cdir -> do
	logMsg' $ "writing cache to " ++ cdir
	logIO "cache writing exception: " logMsg' $ do
		let
			sd = structurize d
		SC.dump cdir sd
		forM_ (M.keys (structuredCabals sd)) $ \c -> logMsg' ("cache write: cabal " ++ show c)
		forM_ (M.keys (structuredProjects sd)) $ \p -> logMsg' ("cache write: project " ++ p)
		case allModules (structuredFiles sd) of
			[] -> return ()
			ms -> logMsg' $ "cache write: " ++ show (length ms) ++ " files"
	logMsg' $ "cache saved to " ++ cdir

readCache :: Opts String -> (String -> IO ()) -> (FilePath -> ErrorT String IO Structured) -> IO (Maybe Database)
readCache sopts logMsg' act = withCache sopts Nothing $ join . liftM (either cacheErr cacheOk) . runErrorT . act where
	cacheErr e = logMsg' ("Error reading cache: " ++ e) >> return Nothing
	cacheOk s = do
		forM_ (M.keys (structuredCabals s)) $ \c -> logMsg' ("cache read: cabal " ++ show c)
		forM_ (M.keys (structuredProjects s)) $ \p -> logMsg' ("cache read: project " ++ p)
		case allModules (structuredFiles s) of
			[] -> return ()
			ms -> logMsg' $ "cache read: " ++ show (length ms) ++ " files"
		return $ Just $ merge s

#if mingw32_HOST_OS
data MmapFile = MmapFile String

instance ToJSON MmapFile where
	toJSON (MmapFile f) = object ["file" .= f]

instance FromJSON MmapFile where
	parseJSON = withObject "file" $ \v -> MmapFile <$> v .:: "file"

-- | Push message to mmap and return response which points to this mmap
mmap :: Pool -> Response -> IO Response
mmap mmapPool r
	| L.length msg <= 1024 = return r
	| otherwise = withSync (responseError "timeout" []) $ \sync -> timeout 10000000 $
		withName mmapPool $ \mmapName -> do
			runErrorT $ flip catchError
				(\e -> liftIO $ sync $ responseError e [])
				(withMapFile mmapName (L.toStrict msg) $ liftIO $ do
					sync $ result $ MmapFile mmapName
					-- give 10 seconds for client to read data
					threadDelay 10000000)
	where
		msg = encode r
#endif

-- | If response points to mmap, get its contents and parse
unMmap :: Response -> IO Response
#if mingw32_HOST_OS
unMmap (Right (Result v))
	| Just (MmapFile f) <- parseMaybe parseJSON v = do
		cts <- runErrorT (fmap L.fromStrict (readMapFile f))
		case cts of
			Left _ -> return $ responseError "Unable to read map view of file" ["file" .= f]
			Right r' -> case eitherDecode r' of
				Left e' -> return $ responseError "Invalid response" ["response" .= fromUtf8 r', "parser error" .= e']
				Right r'' -> return r''
#endif
unMmap r = return r

-- | Log message
logMsg :: Opts String -> String -> IO ()
logMsg sopts s = ignoreIO $ do
	putStrLn s
	case arg "log" sopts of
		Nothing -> return ()
		Just f -> withFile f AppendMode (`hPutStrLn` s)
