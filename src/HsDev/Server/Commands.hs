{-# LANGUAGE OverloadedStrings, CPP, PatternGuards, LambdaCase, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Server.Commands (
	ServerCommand(..), ServerOpts(..), ClientOpts(..),
	Request(..),
	Msg, isLisp, msg, jsonMsg, lispMsg, encodeMessage, decodeMessage,
	sendCommand, runServerCommand,
	findPath,
	processRequest, processClient, processClientSocket,
	module HsDev.Server.Types
	) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Lens (set, traverseOf, view, over, Lens', Lens, _1, _2, _Left)
import Control.Monad
import Control.Monad.Catch (bracket, finally)
import Data.Aeson hiding (Result, Error)
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe
import Data.String (fromString)
import qualified Data.Text as T (pack)
import Network.Socket hiding (connect)
import qualified Network.Socket as Net hiding (send)
import qualified Network.Socket.ByteString as Net (send)
import qualified Network.Socket.ByteString.Lazy as Net (getContents)
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import qualified System.Log.Simple as Log

import Control.Concurrent.Util
import qualified Control.Concurrent.FiniteChan as F
import Data.Lisp
import Text.Format ((~~), (~%))
import System.Directory.Paths

import qualified HsDev.Client.Commands as Client
import qualified HsDev.Database.Async as DB
import HsDev.Server.Base
import HsDev.Server.Types
import HsDev.Tools.Base (runTool_)
import HsDev.Error
import HsDev.Util
import HsDev.Version

#if mingw32_HOST_OS
import Data.Aeson.Types hiding (Result, Error)
import Data.Char
import Data.List
import System.Environment
import System.Win32.FileMapping.Memory (withMapFile, readMapFile)
import System.Win32.FileMapping.NamePool
import System.Win32.PowerShell (escape, quote, quoteDouble)
#else
import Control.Exception (SomeException, handle)
import System.Posix.Process
import System.Posix.Files (removeLink)
import System.Posix.IO
#endif

sendCommand :: ClientOpts -> Bool -> Command -> (Notification -> IO a) -> IO Result
sendCommand copts noFile c onNotification = do
	asyncAct <- async sendReceive
	res <- waitCatch asyncAct
	case res of
		Left e -> return $ Error $ OtherError (show e)
		Right r -> return r
	where
		sendReceive = do
			curDir <- getCurrentDirectory
			input <- if clientStdin copts
				then Just <$> L.getContents
				else return $ toUtf8 <$> Nothing -- arg "data" copts
			let
				parseData :: L.ByteString -> IO Value
				parseData cts = case eitherDecode cts of
					Left err -> putStrLn ("Invalid data: " ++ err) >> exitFailure
					Right v -> return v
			_ <- traverse parseData input -- FIXME: Not used!

			s <- makeSocket (clientPort copts)
			addr' <- inet_addr "127.0.0.1"
			Net.connect s (sockAddr (clientPort copts) addr')
			bracket (socketToHandle s ReadWriteMode) hClose $ \h -> do
				L.hPutStrLn h $ encode $ Message Nothing $ Request c curDir noFile (clientTimeout copts) (clientSilent copts)
				hFlush h
				peekResponse h

		peekResponse h = do
			resp <- hGetLineBS h
			parseResponse h resp

		parseResponse h str = case eitherDecode str of
			Left e -> return $ Error $ ResponseError ("can't parse: {}" ~~ e) (fromUtf8 str)
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
		script = "try {{ start-process {process} {args} -WindowStyle Hidden -WorkingDirectory {dir} }} catch {{ $_.Exception, $_.InvocationInfo.Line }}"
			~~ ("process" ~% escape quote myExe)
			~~ ("args" ~% intercalate ", " (map biescape args))
			~~ ("dir" ~% escape quote curDir)
	r <- runTool_ "powershell" [
		"-Command",
		script]
	if all isSpace r
		then putStrLn $ "Server started at port {}" ~~ serverPort sopts
		else mapM_ putStrLn [
			"Failed to start server",
			"\tCommand: {}" ~~ script,
			"\tResult: {}" ~~ r]
#else
	let
		forkError :: SomeException -> IO ()
		forkError e  = putStrLn $ "Failed to start server: {}" ~~ show e

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
		putStrLn $ "Server started at port {}" ~~ serverPort sopts
#endif
runServerCommand (Run sopts) = runServer sopts $ do
	q <- liftIO $ newQSem 0
	clientChan <- liftIO F.newChan
	session <- getSession
	_ <- liftIO $ async $ withSession session $ Log.scope "listener" $ flip finally serverExit $
		bracket (liftIO $ makeSocket (serverPort sopts)) (liftIO . close) $ \s -> do
			liftIO $ do
				setSocketOption s ReuseAddr 1
				addr' <- inet_addr "127.0.0.1"
				bind s (sockAddr (serverPort sopts) addr')
				listen s maxListenQueue
			forever $ logAsync (Log.sendLog Log.Fatal . fromString) $ logIO "exception: " (Log.sendLog Log.Error . fromString) $ do
				Log.sendLog Log.Trace "accepting connection..."
				liftIO $ signalQSem q
				(s', addr') <- liftIO $ accept s
				Log.sendLog Log.Trace $ "accepted {}" ~~ show addr'
				void $ liftIO $ forkIO $ withSession session $ Log.scope (T.pack $ show addr') $
					logAsync (Log.sendLog Log.Fatal . fromString) $ logIO "exception: " (Log.sendLog Log.Error . fromString) $
						flip finally (liftIO $ close s') $
							bracket (liftIO newEmptyMVar) (liftIO . (`putMVar` ())) $ \done -> do
								me <- liftIO myThreadId
								let
									timeoutWait = withSession session $ do
										notDone <- liftIO $ isEmptyMVar done
										when notDone $ do
											Log.sendLog Log.Trace $ "waiting for {} to complete" ~~ show addr'
											waitAsync <- liftIO $ async $ do
												threadDelay 1000000
												killThread me
											liftIO $ void $ waitCatch waitAsync
								liftIO $ F.putChan clientChan timeoutWait
								processClientSocket (show addr') s'

	Log.sendLog Log.Trace "waiting for starting accept thread..."
	liftIO $ waitQSem q
	liftIO $ putStrLn $ "Server started at port {}" ~~ serverPort sopts
	Log.sendLog Log.Info $ "server started at port {}" ~~ serverPort sopts
	Log.sendLog Log.Trace "waiting for accept thread..."
	serverWait
	Log.sendLog Log.Trace "accept thread stopped"
	liftIO $ unlink (serverPort sopts)
	askSession sessionDatabase >>= liftIO . DB.readAsync >>= writeCache sopts
	Log.sendLog Log.Trace "waiting for clients..."
	liftIO (F.stopChan clientChan) >>= sequence_
	Log.sendLog Log.Info "server stopped"
runServerCommand (Stop copts) = runServerCommand (Remote copts False Exit)
runServerCommand (Connect copts) = do
	curDir <- getCurrentDirectory
	s <- makeSocket $ clientPort copts
	addr' <- inet_addr "127.0.0.1"
	Net.connect s $ sockAddr (clientPort copts) addr'
	bracket (socketToHandle s ReadWriteMode) hClose $ \h -> forM_ [(1 :: Integer)..] $ \i -> ignoreIO $ do
		input' <- hGetLineBS stdin
		case decodeMsg input' of
			Left em -> L.putStrLn $ encodeMessage $ set msg (Message Nothing $ responseError $ OtherError "invalid command") em
			Right m -> do
				L.hPutStrLn h $ encodeMessage $ set msg (Message (Just $ show i) $ Request (view msg m) curDir True (clientTimeout copts) False) m
				waitResp h
	where
		waitResp h = do
			resp <- hGetLineBS h
			parseResp h resp

		parseResp h str = case decodeMessage str of
			Left em -> putStrLn $ "Can't decode response: {}" ~~ view msg em
			Right m -> do
				Response r' <- unMmap $ view (msg . message) m
				putStrLn $ "{id}: {response}"
					~~ ("id" ~% fromMaybe "_" (view (msg . messageId) m))
					~~ ("response" ~% fromUtf8 (encodeMsg $ set msg (Response r') m))
				case unResponse (view (msg . message) m) of
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
		| isRelative f = commandOptionsRoot copts </> f
		| otherwise = f

type Msg a = (Bool, a)

isLisp :: Lens' (Msg a) Bool
isLisp = _1

msg :: Lens (Msg a) (Msg b) a b
msg = _2

jsonMsg :: a -> Msg a
jsonMsg = (,) False

lispMsg :: a -> Msg a
lispMsg = (,) True

-- | Decode lisp or json
decodeMsg :: FromJSON a => ByteString -> Either (Msg String) (Msg a)
decodeMsg bstr = over _Left decodeType' decodeMsg' where
	decodeType'
		| isLisp' = lispMsg
		| otherwise = jsonMsg
	decodeMsg' = (lispMsg <$> decodeLisp bstr) <|> (jsonMsg <$> eitherDecode bstr)
	isLisp' = fromMaybe False $ mplus (try' eitherDecode False) (try' decodeLisp True)
	try' :: (ByteString -> Either String Value) -> Bool -> Maybe Bool
	try' f l = either (const Nothing) (const $ Just l) $ f bstr

-- | Encode lisp or json
encodeMsg :: ToJSON a => Msg a -> ByteString
encodeMsg m
	| view isLisp m = encodeLisp $ view msg m
	| otherwise = encode $ view msg m

-- | Decode lisp or json request
decodeMessage :: FromJSON a => ByteString -> Either (Msg String) (Msg (Message a))
decodeMessage = decodeMsg

encodeMessage :: ToJSON a => Msg (Message a) -> ByteString
encodeMessage = encodeMsg

-- | Process request, notifications can be sent during processing
processRequest :: SessionMonad m => CommandOptions -> Command -> m Result
processRequest copts c = do
	c' <- paths (findPath copts) c
	s <- getSession
	withSession s $ Client.runClient copts $ Client.runCommand c'

-- | Process client, listen for requests and process them
processClient :: SessionMonad m => String -> F.Chan ByteString -> (ByteString -> IO ()) -> m ()
processClient name rchan send' = do
	Log.sendLog Log.Info "connected"
	respChan <- liftIO newChan
	liftIO $ void $ forkIO $ getChanContents respChan >>= mapM_ (send' . encodeMessage)
	linkVar <- liftIO $ newMVar $ return ()
	s <- getSession
	exit <- askSession sessionExit
	let
		answer :: SessionMonad m => Msg (Message Response) -> m ()
		answer m = do
			unless (isNotification $ view (msg . message) m) $
				Log.sendLog Log.Trace $ "responsed << {}" ~~ ellipsis (fromUtf8 (encode $ view (msg . message) m))
			liftIO $ writeChan respChan m
			where
				ellipsis :: String -> String
				ellipsis str
					| length str < 100 = str
					| otherwise = take 100 str ++ "..."
	-- flip finally (disconnected linkVar) $ forever $ Log.scopeLog (commandLogger copts) (T.pack name) $ do
	reqs <- liftIO $ F.readChan rchan
	flip finally (disconnected linkVar) $
		forM_ reqs $ \req' -> do
			Log.sendLog Log.Trace $ "received >> {}" ~~ fromUtf8 req'
			case decodeMessage req' of
				Left em -> do
					Log.sendLog Log.Warning $ "Invalid request {}" ~~ fromUtf8 req'
					answer $ set msg (Message Nothing $ responseError $ RequestError "invalid request" $ fromUtf8 req') em
				Right m -> void $ liftIO $ forkIO $ withSession s $ Log.scope (T.pack name) $ Log.scope "req" $
					Log.scope (T.pack $ fromMaybe "_" (view (msg . messageId) m)) $ do
						resp' <- flip (traverseOf (msg . message)) m $ \(Request c cdir noFile tm silent) -> do
							let
								onNotify n
									| silent = return ()
									| otherwise = traverseOf (msg . message) (const $ mmap' noFile (Response $ Left n)) m >>= answer
							Log.sendLog Log.Trace $ "requested >> {}" ~~ fromUtf8 (encode c)
							resp <- liftIO $ fmap (Response . Right) $ handleTimeout tm $ hsdevLiftIO $ withSession s $
								processRequest
									CommandOptions {
										commandOptionsRoot = cdir,
										commandOptionsNotify = withSession s . onNotify,
										commandOptionsLink = void (swapMVar linkVar exit),
										commandOptionsHold = forever (F.getChan rchan) }
									c
							mmap' noFile resp
						answer resp'
	where
		handleTimeout :: Int -> IO Result -> IO Result
		handleTimeout 0 = id
		handleTimeout tm = fmap (fromMaybe $ Error $ OtherError "timeout") . timeout tm

		mmap' :: SessionMonad m => Bool -> Response -> m Response
#if mingw32_HOST_OS
		mmap' False r = do
			mpool <- askSession sessionMmapPool
			case mpool of
				Just pool -> liftIO $ mmap pool r
				Nothing -> return r
#endif
		mmap' _ r = return r

		-- Call on disconnected, either no action or exit command
		disconnected :: SessionMonad m => MVar (IO ()) -> m ()
		disconnected var = do
			Log.sendLog Log.Info "disconnected"
			liftIO $ join $ takeMVar var

-- | Process client by socket
processClientSocket :: SessionMonad m => String -> Socket -> m ()
processClientSocket name s = do
	recvChan <- liftIO F.newChan
	liftIO $ void $ forkIO $ finally
		(Net.getContents s >>= mapM_ (F.putChan recvChan) . L.lines)
		(F.closeChan recvChan)
	processClient name recvChan (sendLine s)
	where
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
	| L.length msg' <= 1024 = return r
	| otherwise = do
		rvar <- newEmptyMVar
		_ <- forkIO $ flip finally (tryPutMVar rvar r) $ void $ withName mmapPool $ \mmapName -> runExceptT $ catchError
			(withMapFile mmapName (L.toStrict msg') $ liftIO $ do
				_ <- tryPutMVar rvar $ result $ MmapFile mmapName
				-- give 10 seconds for client to read data
				threadDelay 10000000)
			(\_ -> liftIO $ void $ tryPutMVar rvar r)
		takeMVar rvar
	where
		msg' = encode r
#endif

-- | If response points to mmap, get its contents and parse
unMmap :: Response -> IO Response
#if mingw32_HOST_OS
unMmap (Response (Right (Result v)))
	| Just (MmapFile f) <- parseMaybe parseJSON v = do
		cts <- runExceptT (fmap L.fromStrict (readMapFile f))
		case cts of
			Left _ -> return $ responseError $ ResponseError "can't read map view of file" f
			Right r' -> case eitherDecode r' of
				Left e' -> return $ responseError $ ResponseError ("can't parse response: {}" ~~ e') (fromUtf8 r')
				Right r'' -> return r''
#endif
unMmap r = return r

makeSocket :: ConnectionPort -> IO Socket
makeSocket (NetworkPort _) = socket AF_INET Stream defaultProtocol
makeSocket (UnixPort _) = socket AF_UNIX Stream defaultProtocol

sockAddr :: ConnectionPort -> HostAddress -> SockAddr
sockAddr (NetworkPort p) addr = SockAddrInet (fromIntegral p) addr
sockAddr (UnixPort s) _ = SockAddrUnix s

unlink :: ConnectionPort -> IO ()
unlink (NetworkPort _) = return ()
#if mingw32_HOST_OS
unlink (UnixPort _) = return ()
#else
unlink (UnixPort s) = removeLink s
#endif
