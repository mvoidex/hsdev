{-# LANGUAGE OverloadedStrings, CPP, PatternGuards, LambdaCase, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Server.Commands (
	ServerCommand(..), ServerOpts(..), ClientOpts(..),
	Request(..),
	sendCommand, runServerCommand,
	initLog, runServer, startServer, inServer,
	processRequest, processClient, processClientSocket,
	withCache, writeCache, readCache,
	module HsDev.Server.Types
	) where

import Control.Applicative
import Control.Arrow (second)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson hiding (Result, Error)
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Default
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Either (isLeft)
import qualified Data.Map as M
import Data.Foldable (asum)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Network.Socket hiding (connect)
import qualified Network.Socket as Net hiding (send)
import qualified Network.Socket.ByteString as Net (send)
import qualified Network.Socket.ByteString.Lazy as Net (getContents)
import Options.Applicative
import System.Directory
import System.Exit
import System.IO
import System.Log.Simple hiding (Level(..), Message(..), Command(..))
import System.Log.Simple.Base (writeLog)
import qualified System.Log.Simple.Base as Log
import Text.Read (readMaybe)

import Control.Apply.Util
import Control.Concurrent.Util
import qualified Control.Concurrent.FiniteChan as F
import Data.Lisp
import qualified System.Directory.Watcher as Watcher
import System.Console.Cmd hiding (run, cmd, flag, short, help)
import qualified System.Console.Cmd as C (cmd, flag, short, help)
import Text.Format ((~~), FormatBuild(..))

import qualified HsDev.Cache.Structured as SC
import qualified HsDev.Client.Commands as Client
import HsDev.Database
import qualified HsDev.Database.Async as DB
import qualified HsDev.Database.Update as Update
import HsDev.Tools.Ghc.Worker
import HsDev.Tools.GhcMod (ghcModMultiWorker)
import HsDev.Server.Message as M
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

-- | Server control command
data ServerCommand =
	Version |
	Start ServerOpts |
	Run ServerOpts |
	Stop ClientOpts |
	Connect ClientOpts |
	Remote ClientOpts Bool Command
		deriving (Show)

-- | Server options
data ServerOpts = ServerOpts {
	serverPort :: Int,
	serverTimeout :: Int,
	serverLog :: Maybe FilePath,
	serverLogConfig :: String,
	serverCache :: Maybe FilePath,
	serverLoad :: Bool }
		deriving (Show)

instance Default ServerOpts where
	def = ServerOpts 1234 0 Nothing "use default" Nothing False

-- | Client options
data ClientOpts = ClientOpts {
	clientPort :: Int,
	clientPretty :: Bool,
	clientStdin :: Bool,
	clientTimeout :: Int,
	clientSilent :: Bool }
		deriving (Show)

instance Default ClientOpts where
	def = ClientOpts 1234 False False 0 False

instance FromCmd ServerCommand where
	cmdP = serv <|> remote where
		serv = subparser $ mconcat [
			cmd "version" "hsdev version" (pure Version),
			cmd "start" "start remote server" (Start <$> cmdP),
			cmd "run" "run server" (Run <$> cmdP),
			cmd "stop" "stop remote server" (Stop <$> cmdP),
			cmd "connect" "connect to send commands directly" (Connect <$> cmdP)]
		remote = Remote <$> cmdP <*> noFileFlag <*> cmdP

instance FromCmd ServerOpts where
	cmdP = ServerOpts <$>
		(portArg <|> pure (serverPort def)) <*>
		(timeoutArg <|> pure (serverTimeout def)) <*>
		optional logArg <*>
		(logConfigArg <|> pure (serverLogConfig def)) <*>
		optional cacheArg <*>
		loadFlag

instance FromCmd ClientOpts where
	cmdP = ClientOpts <$>
		(portArg <|> pure (clientPort def)) <*>
		prettyFlag <*>
		stdinFlag <*>
		(timeoutArg <|> pure (clientTimeout def)) <*>
		silentFlag

serverOptsArgs :: ServerOpts -> [String]
serverOptsArgs sopts = concat [
	["--port", show $ serverPort sopts],
	["--timeout", show $ serverTimeout sopts],
	marg "--log" (serverLog sopts),
	["--log-config", serverLogConfig sopts],
	marg "--cache" (serverCache sopts),
	if serverLoad sopts then ["--load"] else []]
	where
		marg :: String -> Maybe String -> [String]
		marg n (Just v) = [n, v]
		marg _ _ = []

data Request = Request {
	requestCommand :: Command,
	requestDirectory :: FilePath,
	requestNoFile :: Bool,
	requestTimeout :: Int,
	requestSilent :: Bool }

instance ToJSON Request where
	toJSON (Request c dir f tm s) = object ["command" .= c, "current-directory" .= dir, "no-file" .= f, "timeout" .= tm, "silent" .= s]

instance FromJSON Request where
	parseJSON = withObject "request" $ \v -> Request <$> v .:: "command" <*> v .:: "current-directory" <*> v .:: "no-file" <*> v .:: "timeout" <*> v .:: "silent"

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
			dat <- traverse parseData input

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
				r' <- unMmap r
				case r' of
					Left n -> onNotification n >> peekResponse h
					Right r -> return r

runServerCommand :: ServerCommand -> IO ()
runServerCommand Version = putStrLn $cabalVersion
runServerCommand (Start sopts) = do
#if mingw32_HOST_OS
	let
		args = ["run"] ++ serverOptsArgs sopts
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
		bind s $ SockAddrInet (fromIntegral $ serverPort sopts) iNADDR_ANY
		listen s maxListenQueue
		forever $ logAsync (commandLog copts Log.Fatal) $ logIO "accept client exception: " (commandLog copts Log.Error) $ do
			s' <- fst <$> accept s
			void $ forkIO $ logAsync (commandLog copts Log.Fatal) $ logIO (show s' ++ " exception: ") (commandLog copts Log.Error) $
				flip finally (close s') $
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
							-- waitForever = forever $ hGetLineBS h
						F.putChan clientChan timeoutWait
						processClientSocket s' (copts {
							-- commandHold = waitForever,
							commandExit = serverStop })

	takeMVar waitListen
	DB.readAsync (commandDatabase copts) >>= writeCache sopts (commandLog copts)
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
				r' <- unMmap r
				putStrLn $ fromMaybe "_" i ++ ":" ++ fromUtf8 (encodeValue r')
				case r of
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

portArg = option auto (long "port" <> metavar "number" <> help "connection port")
timeoutArg = option auto (long "timeout" <> metavar "msec" <> help "query timeout")
logArg = strOption (long "log" <> short 'l' <> metavar "file" <> help "log file")
logConfigArg = strOption (long "log-config" <> metavar "rule" <> help "log config: low [low], high [high], set [low] [high], use [default/debug/trace/silent/supress]")
cacheArg = strOption (long "cache" <> metavar "path" <> help "cache directory")
noFileFlag = switch (long "no-file" <> help "don't use mmap files")
loadFlag = switch (long "load" <> help "force load all data from cache on startup")
prettyFlag = switch (long "pretty" <> help "pretty json output")
stdinFlag = switch (long "stdin" <> help "pass data to stdin")
silentFlag = switch (long "silent" <> help "supress notifications")

chaner :: F.Chan String -> Consumer Text
chaner ch = Consumer withChan where
	withChan f = f (F.putChan ch . T.unpack)

instance FormatBuild Log.Level where

-- | Inits log chan and returns functions (print message, wait channel)
initLog :: ServerOpts -> IO (Log, Log.Level -> String -> IO (), ([String] -> IO ()) -> IO (), IO ())
initLog sopts = do
	msgs <- F.newChan
	outputDone <- newEmptyMVar
	void $ forkIO $ finally
		(F.readChan msgs >>= mapM_ (const $ return ()))
		(putMVar outputDone ())
	l <- newLog (constant [rule']) $ concat [
		[logger text console],
		[logger text (chaner msgs)],
		maybeToList $ (logger text . file) <$> serverLog sopts]
	Log.writeLog l Log.Info ("Log politics: low = {}, high = {}" ~~ logLow ~~ logHigh)
	let
		listenLog f = logException "listen log" (F.putChan msgs) $ do
			msgs' <- F.dupChan msgs
			F.readChan msgs' >>= f
	return (l, \lev -> writeLog l lev . T.pack, listenLog, F.closeChan msgs >> takeMVar outputDone)
	where
		rule' :: Log.Rule
		rule' = parseRule_ $ T.pack ("/: " ++ serverLogConfig sopts)
		(Log.Politics logLow logHigh) = Log.rulePolitics rule' Log.defaultPolitics

-- | Run server
runServer :: ServerOpts -> (CommandOptions -> IO ()) -> IO ()
runServer sopts act = bracket (initLog sopts) (\(_, _, _, x) -> x) $ \(logger', outputStr, listenLog, waitOutput) -> Log.scopeLog logger' (T.pack "hsdev") $ Watcher.withWatcher $ \watcher -> do
	db <- DB.newAsync
	when (serverLoad sopts) $ withCache sopts () $ \cdir -> do
		outputStr Log.Info $ "Loading cache from " ++ cdir
		dbCache <- liftA merge <$> SC.load cdir
		case dbCache of
			Left err -> outputStr Log.Error $ "Failed to load cache: " ++ err
			Right dbCache' -> DB.update db (return dbCache')
#if mingw32_HOST_OS
	mmapPool <- Just <$> createPool "hsdev"
#endif
	ghcw <- ghcWorker [] (return ())
	ghciw <- ghciWorker
	ghcmodw <- ghcModMultiWorker
	let
		copts = CommandOptions
			db
			(writeCache sopts outputStr)
			(readCache sopts outputStr)
			"."
			outputStr
			logger'
			listenLog
			waitOutput
			watcher
#if mingw32_HOST_OS
			mmapPool
#endif
			ghcw
			ghciw
			ghcmodw
			(const $ return ())
			(return ())
			(return ())
			(return ())
	_ <- forkIO $ Update.onEvent watcher (Update.processEvent $ Update.settings copts [] False False)
	act copts

type Server = Worker (ReaderT CommandOptions IO)

startServer :: ServerOpts -> IO Server
startServer sopts = startWorker (\act -> runServer sopts (runReaderT act)) id id

inServer :: Server -> Command -> IO Result
inServer srv c = inWorker srv (ReaderT (flip Client.runCommand c))

decodeLispOrJSON :: FromJSON a => ByteString -> Either String (Bool, a)
decodeLispOrJSON str =
	((,) <$> pure False <*> eitherDecode str) <|>
	((,) <$> pure True <*> decodeLisp str)

encodeLispOrJSON :: ToJSON a => Bool -> a -> ByteString
encodeLispOrJSON True = encodeLisp
encodeLispOrJSON False = encode

-- | Process request, notifications can be sent during processing
processRequest :: CommandOptions -> (Notification -> IO ()) -> Command -> IO Result
processRequest copts onNotify c = Client.runCommand (copts { commandNotify = onNotify }) c

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
			when (not $ isNotification r) $
				commandLog copts Log.Trace $ " << " ++ ellipsis (fromUtf8 (encode r))
			writeChan respChan m
			where
				ellipsis :: String -> String
				ellipsis s
					| length s < 100 = s
					| otherwise = take 100 s ++ "..."
	flip finally (disconnected linkVar) $ forever $ Log.scopeLog (commandLogger copts) (T.pack name) $ do
		req' <- receive
		commandLog copts Log.Trace $ " => " ++ fromUtf8 req'
		case eitherDecode req' of
			Left _ -> do
				commandLog copts Log.Warning $ "Invalid request: " ++ fromUtf8 req'
				answer $ Message Nothing $ responseError "Invalid request" [
					"request" .= fromUtf8 req']
			Right m -> Log.scopeLog (commandLogger copts) (T.pack $ fromMaybe "_" (messageId m)) $ do
				resp' <- flip traverse m $ \(Request c cdir noFile tm silent) -> do
					let
						onNotify n
							| silent = return ()
							| otherwise = traverse (const $ mmap' noFile (Left n)) m >>= answer
					commandLog copts Log.Trace $ name ++ " >> " ++ fromUtf8 (encode c)
					resp <- fmap Right $ handleTimeout tm $ handleError $
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

		disconnected :: MVar (IO ()) -> IO ()
		disconnected var = do
			commandLog copts Log.Info $ name ++ " disconnected"
			join $ takeMVar var

{-
-- | Process client by Handle
processClientHandle :: Show a => a -> Handle -> CommandOptions -> IO ()
processClientHandle n h = processClient (show n) (hGetLineBS h) (\s -> L.hPutStrLn h s >> hFlush h)
-}

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

-- | Perform action on cache
withCache :: ServerOpts -> a -> (FilePath -> IO a) -> IO a
withCache sopts v onCache = case serverCache sopts of
	Nothing -> return v
	Just cdir -> onCache cdir

writeCache :: ServerOpts -> (Log.Level -> String -> IO ()) -> Database -> IO ()
writeCache sopts logMsg' d = withCache sopts () $ \cdir -> do
	logMsg' Log.Info $ "writing cache to " ++ cdir
	logIO "cache writing exception: " (logMsg' Log.Error) $ do
		let
			sd = structurize d
		SC.dump cdir sd
		forM_ (M.keys (structuredCabals sd)) $ \c -> logMsg' Log.Debug ("cache write: cabal " ++ show c)
		forM_ (M.keys (structuredProjects sd)) $ \p -> logMsg' Log.Debug ("cache write: project " ++ p)
		case allModules (structuredFiles sd) of
			[] -> return ()
			ms -> logMsg' Log.Debug $ "cache write: " ++ show (length ms) ++ " files"
	logMsg' Log.Info $ "cache saved to " ++ cdir

readCache :: ServerOpts -> (Log.Level -> String -> IO ()) -> (FilePath -> ExceptT String IO Structured) -> IO (Maybe Database)
readCache sopts logMsg' act = withCache sopts Nothing $ join . liftM (either cacheErr cacheOk) . runExceptT . act where
	cacheErr e = logMsg' Log.Error ("Error reading cache: " ++ e) >> return Nothing
	cacheOk s = do
		forM_ (M.keys (structuredCabals s)) $ \c -> logMsg' Log.Debug ("cache read: cabal " ++ show c)
		forM_ (M.keys (structuredProjects s)) $ \p -> logMsg' Log.Debug ("cache read: project " ++ p)
		case allModules (structuredFiles s) of
			[] -> return ()
			ms -> logMsg' Log.Debug $ "cache read: " ++ show (length ms) ++ " files"
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
			runExceptT $ flip catchError
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
		cts <- runExceptT (fmap L.fromStrict (readMapFile f))
		case cts of
			Left _ -> return $ responseError "Unable to read map view of file" ["file" .= f]
			Right r' -> case eitherDecode r' of
				Left e' -> return $ responseError "Invalid response" ["response" .= fromUtf8 r', "parser error" .= e']
				Right r'' -> return r''
#endif
unMmap r = return r
