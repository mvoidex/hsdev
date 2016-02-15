{-# LANGUAGE OverloadedStrings, CPP, PatternGuards, LambdaCase, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Server.Commands (
	ServerCommand(..), ServerOpts(..), ClientOpts(..),
	Request(..),
	sendCommand, runServerCommand,
	findPath,
	processRequest, processClient, processClientSocket,
	module HsDev.Server.Types
	) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Data.Aeson hiding (Result, Error)
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T (pack)
import Data.Traversable (for)
import Network.Socket hiding (connect)
import qualified Network.Socket as Net hiding (send)
import qualified Network.Socket.ByteString as Net (send)
import qualified Network.Socket.ByteString.Lazy as Net (getContents)
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import qualified System.Log.Simple.Base as Log

import Control.Concurrent.Util
import qualified Control.Concurrent.FiniteChan as F
import Text.Format ((~~))
import System.Directory.Paths

import qualified HsDev.Client.Commands as Client
import qualified HsDev.Database.Async as DB
import HsDev.Server.Base
import HsDev.Server.Types
import HsDev.Util
import HsDev.Version

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

sendCommand :: ClientOpts -> Bool -> Command -> (Notification -> IO a) -> IO Result
sendCommand copts noFile c onNotification = do
	asyncAct <- async sendReceive
	res <- waitCatch asyncAct
	case res of
		Left e -> return $ Error (show e) $ M.fromList []
		Right r -> return r
	where
		encodeValue :: ToJSON a => a -> L.ByteString
		encodeValue
			| clientPretty copts = encodePretty
			| otherwise = encode
		sendReceive = do
			curDir <- getCurrentDirectory
			input <- if clientStdin copts
				then liftM Just L.getContents
				else return $ fmap toUtf8 $ Nothing -- arg "data" copts
			let
				parseData :: L.ByteString -> IO Value
				parseData cts = case eitherDecode cts of
					Left err -> putStrLn ("Invalid data: " ++ err) >> exitFailure
					Right v -> return v
			dat <- traverse parseData input -- FIXME: Not used!

			s <- socket AF_INET Stream defaultProtocol
			addr' <- inet_addr "127.0.0.1"
			Net.connect s (SockAddrInet (fromIntegral $ clientPort copts) addr')
			bracket (socketToHandle s ReadWriteMode) hClose $ \h -> do
				L.hPutStrLn h $ encode $ Message Nothing $ Request c curDir noFile (clientTimeout copts) (clientSilent copts)
				hFlush h
				peekResponse h

		peekResponse h = do
			resp <- hGetLineBS h
			parseResponse h resp

		parseResponse h str = case eitherDecode str of
			Left e -> return $ Error e $ M.fromList [("response", toJSON $ fromUtf8 str)]
			Right (Message _ r) -> do
				Response r' <- unMmap r
				case r' of
					Left n -> onNotification n >> peekResponse h
					Right res -> return res

runServerCommand :: ServerCommand -> IO ()
runServerCommand Version = putStrLn $cabalVersion
runServerCommand (Start sopts) = do
#if mingw32_HOST_OS
	let
		args = "run" : serverOptsArgs sopts
	myExe <- getExecutablePath
	curDir <- getCurrentDirectory
	let
		-- one escape for start-process and other for callable process
		-- seems, that start-process just concats arguments into one string
		-- start-process foo 'bar baz' ⇒ foo bar baz -- not expected
		-- start-process foo '"bar baz"' ⇒ foo "bar baz" -- ok
		biescape = escape quote . escape quoteDouble
		script = "try {{ start-process {} {} -WindowStyle Hidden -WorkingDirectory {} }} catch {{ $_.Exception, $_.InvocationInfo.Line }}"
			~~ escape quote myExe
			~~ intercalate ", " (map biescape args)
			~~ escape quote curDir
	r <- readProcess "powershell" [
		"-Command",
		script] ""
	if all isSpace r
		then putStrLn $ "Server started at port " ++ show (serverPort sopts)
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
			runServerCommand (Run sopts)

	handle forkError $ do
		_ <- forkProcess proxy
		putStrLn $ "Server started at port " ++ show (serverPort sopts)
#endif
runServerCommand (Run sopts) = runServer sopts $ \copts -> do
	commandLog copts Log.Info $ "Server started at port " ++ show (serverPort sopts)
	clientChan <- F.newChan
	listenerSem <- newQSem 0
	_ <- async $ Log.scopeLog (commandLogger copts) "listener" $ flip finally (signalQSem listenerSem) $ do
		let
			serverStop :: IO ()
			serverStop = void $ do
				commandLog copts Log.Trace "stopping server"
				-- NOTE: killing listener doesn't work on Windows, because accept blocks async exceptions
				signalQSem listenerSem

		bracket (socket AF_INET Stream defaultProtocol) close $ \s -> do
			setSocketOption s ReuseAddr 1
			bind s $ SockAddrInet (fromIntegral $ serverPort sopts) iNADDR_ANY
			listen s maxListenQueue
			forever $ logAsync (commandLog copts Log.Fatal) $ logIO "exception: " (commandLog copts Log.Error) $ do
				commandLog copts Log.Trace "accepting connection"
				s' <- fst <$> accept s
				commandLog copts Log.Trace $ "accepted " ++ show s'
				void $ forkIO $ Log.scopeLog (commandLogger copts) (T.pack $ show s') $
					logAsync (commandLog copts Log.Fatal) $ logIO ("exception: ") (commandLog copts Log.Error) $
						flip finally (close s') $
							bracket newEmptyMVar (`putMVar` ()) $ \done -> do
								me <- myThreadId
								let
									timeoutWait = do
										notDone <- isEmptyMVar done
										when notDone $ do
											commandLog copts Log.Trace $ "waiting for " ++ show s' ++ " to complete"
											waitAsync <- async $ do
												threadDelay 1000000
												killThread me
											void $ waitCatch waitAsync
								F.putChan clientChan timeoutWait
								processClientSocket s' (copts {
									commandExit = serverStop })

	commandLog copts Log.Trace "waiting for accept thread"
	waitQSem listenerSem
	commandLog copts Log.Trace "accept thread stopped"
	DB.readAsync (commandDatabase copts) >>= writeCache sopts (commandLog copts)
	commandLog copts Log.Trace "waiting for clients"
	F.stopChan clientChan >>= sequence_
	commandLog copts Log.Info "server stopped"
runServerCommand (Stop copts) = runServerCommand (Remote copts False Exit)
runServerCommand (Connect copts) = do
	curDir <- getCurrentDirectory
	s <- socket AF_INET Stream defaultProtocol
	addr' <- inet_addr "127.0.0.1"
	Net.connect s (SockAddrInet (fromIntegral $ clientPort copts) addr')
	bracket (socketToHandle s ReadWriteMode) hClose $ \h -> forM_ [(1 :: Integer)..] $ \i -> ignoreIO $ do
		input' <- hGetLineBS stdin
		case eitherDecode input' of
			Left _ -> L.putStrLn $ encodeValue $ object ["error" .= ("invalid command" :: String)]
			Right req' -> do
				L.hPutStrLn h $ encode $ Message (Just $ show i) $ Request req' curDir True (clientTimeout copts) False
				waitResp h
	where
		pretty = clientPretty copts

		encodeValue :: ToJSON a => a -> L.ByteString
		encodeValue
			| pretty = encodePretty
			| otherwise = encode

		waitResp h = do
			resp <- hGetLineBS h
			parseResp h resp

		parseResp h str = case eitherDecode str of
			Left e -> putStrLn $ "Can't decode response: " ++ e
			Right (Message i r) -> do
				Response r' <- unMmap r
				putStrLn $ fromMaybe "_" i ++ ":" ++ fromUtf8 (encodeValue r')
				case unResponse r of
					Left _ -> waitResp h
					_ -> return ()
runServerCommand (Remote copts noFile c) = sendCommand copts noFile c printValue >>= printResult where
	printValue :: ToJSON a => a -> IO ()
	printValue = L.putStrLn . encodeValue
	printResult :: Result -> IO ()
	printResult (Result r) = printValue r
	printResult e = printValue e
	encodeValue :: ToJSON a => a -> L.ByteString
	encodeValue = if clientPretty copts then encodePretty else encode

findPath :: MonadIO m => CommandOptions -> FilePath -> m FilePath
findPath copts f = liftIO $ canonicalizePath (normalise f') where
	f'
		| isRelative f = commandRoot copts </> f
		| otherwise = f

-- | Process request, notifications can be sent during processing
processRequest :: CommandOptions -> (Notification -> IO ()) -> Command -> IO Result
processRequest copts onNotify c = paths (findPath copts) c >>= Client.runCommand (copts { commandNotify = onNotify })

-- | Process client, listen for requests and process them
processClient :: String -> IO ByteString -> (ByteString -> IO ()) -> CommandOptions -> IO ()
processClient name receive send' copts = do
	commandLog copts Log.Info $ name ++ " connected"
	respChan <- newChan
	void $ forkIO $ getChanContents respChan >>= mapM_ (send' . encode)
	linkVar <- newMVar $ return ()
	let
		answer :: Message Response -> IO ()
		answer m@(Message _ r) = do
			unless (isNotification r) $
				commandLog copts Log.Trace $ " << " ++ ellipsis (fromUtf8 (encode r))
			writeChan respChan m
			where
				ellipsis :: String -> String
				ellipsis s
					| length s < 100 = s
					| otherwise = take 100 s ++ "..."
	-- flip finally (disconnected linkVar) $ forever $ Log.scopeLog (commandLogger copts) (T.pack name) $ do
	flip finally (disconnected linkVar) $ forever $ do
		req' <- receive
		commandLog copts Log.Trace $ " => " ++ fromUtf8 req'
		case eitherDecode req' of
			Left _ -> do
				commandLog copts Log.Warning $ "Invalid request: " ++ fromUtf8 req'
				answer $ Message Nothing $ responseError "Invalid request" [
					"request" .= fromUtf8 req']
			Right m -> Log.scopeLog (commandLogger copts) (T.pack $ fromMaybe "_" (messageId m)) $ do
				resp' <- for m $ \(Request c cdir noFile tm silent) -> do
					let
						onNotify n
							| silent = return ()
							| otherwise = traverse (const $ mmap' noFile (Response $ Left n)) m >>= answer
					commandLog copts Log.Trace $ name ++ " >> " ++ fromUtf8 (encode c)
					resp <- fmap result $ handleTimeout tm $ handleError $
						processRequest
							(copts {
								commandRoot = cdir,
								commandLink = void (swapMVar linkVar $ commandExit copts) })
							onNotify
							c
					mmap' noFile resp
				answer resp'
	where
		handleTimeout :: Int -> IO Result -> IO Result
		handleTimeout 0 = id
		handleTimeout tm = fmap (fromMaybe $ Error "Timeout" M.empty) . timeout tm

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

		-- Call on disconnected, either no action or exit command
		disconnected :: MVar (IO ()) -> IO ()
		disconnected var = do
			commandLog copts Log.Info $ name ++ " disconnected"
			join $ takeMVar var

-- | Process client by socket
processClientSocket :: Socket -> CommandOptions -> IO ()
processClientSocket s copts = do
	recvChan <- F.newChan
	void $ forkIO $ finally
		(Net.getContents s >>= mapM_ (F.putChan recvChan) . L.lines)
		(F.closeChan recvChan)
	processClient (show s) (getChan_ recvChan) (sendLine s) (copts {
		commandHold = forever (getChan_ recvChan) })
	where
		getChan_ :: F.Chan a -> IO a
		getChan_ = F.getChan >=> maybe noData return
		noData :: IO a
		noData = throwIO $ userError "Receive chan closed"
		-- NOTE: Network version of `sendAll` goes to infinite loop on client socket close
		-- when server's send is blocked, see https://github.com/haskell/network/issues/155
		-- After that issue fixed we may revert to `processClientHandle`
		sendLine :: Socket -> ByteString -> IO ()
		sendLine sock bs = sendAll sock $ L.toStrict $ L.snoc bs '\n'
		sendAll :: Socket -> BS.ByteString -> IO ()
		sendAll sock bs
			| BS.null bs = return ()
			| otherwise = do
				sent <- Net.send sock bs
				when (sent > 0) $ sendAll sock (BS.drop sent bs)

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
	| otherwise = do
		rvar <- newEmptyMVar
		_ <- forkIO $ flip finally (tryPutMVar rvar r) $ void $ withName mmapPool $ \mmapName -> runExceptT $ flip catchError
			(\e -> liftIO $ void $ tryPutMVar rvar r)
			(withMapFile mmapName (L.toStrict msg) $ liftIO $ do
				_ <- tryPutMVar rvar $ result $ MmapFile mmapName
				-- give 10 seconds for client to read data
				threadDelay 10000000)
		takeMVar rvar
	where
		msg = encode r
#endif

-- | If response points to mmap, get its contents and parse
unMmap :: Response -> IO Response
#if mingw32_HOST_OS
unMmap (Response (Right (Result v)))
	| Just (MmapFile f) <- parseMaybe parseJSON v = do
		cts <- runExceptT (fmap L.fromStrict (readMapFile f))
		case cts of
			Left _ -> return $ responseError "Unable to read map view of file" ["file" .= f]
			Right r' -> case eitherDecode r' of
				Left e' -> return $ responseError "Invalid response" ["response" .= fromUtf8 r', "parser error" .= e']
				Right r'' -> return r''
#endif
unMmap r = return r
