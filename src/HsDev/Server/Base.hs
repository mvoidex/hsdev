{-# LANGUAGE CPP, OverloadedStrings, CPP, PatternGuards, TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Server.Base (
	initLog, runServer, Server,
	setupServer, shutdownServer,
	startServer, startServer_, stopServer, withServer, withServer_, inServer, clientCommand, parseCommand, readCommand,
	sendServer, sendServer_,
	findPath,
	processRequest, processClient, processClientSocket,

	unMmap, makeSocket, sockAddr,

	module HsDev.Server.Types,
	module HsDev.Server.Message
	) where

import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Concurrent.Chan as C
import Control.Lens (set, traverseOf, view)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops
import Control.Monad.Reader
import Control.Monad.Catch (bracket_, bracket, finally)
import Data.Aeson hiding (Result, Error)
import Data.Default
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe
import Data.String (fromString)
import qualified Data.Text as T (pack)
import Data.Time.Clock.POSIX
import Options.Applicative (info, progDesc)
import System.Log.Simple hiding (Level(..), Message)
import qualified System.Log.Simple.Base as Log (level_)
import qualified System.Log.Simple as Log
import Network.Socket
import qualified Network.Socket.ByteString as Net (send)
import qualified Network.Socket.ByteString.Lazy as Net (getContents)
import System.FilePath
import System.IO
import Text.Format ((~~))

import Control.Concurrent.Util
import qualified Control.Concurrent.FiniteChan as F
import Data.LookupTable
import System.Directory.Paths
import qualified System.Directory.Watcher as Watcher

import qualified HsDev.Client.Commands as Client
import qualified HsDev.Database.SQLite as SQLite
import HsDev.Error
import qualified HsDev.Database.Update as Update
import HsDev.Inspect (getDefines)
import HsDev.Tools.Ghc.Worker hiding (Session)
import HsDev.Server.Types
import HsDev.Server.Message
import HsDev.Symbols.Location (ModuleLocation(..), globalDb)
import qualified HsDev.Watcher as W
import HsDev.Util

#if mingw32_HOST_OS
import Data.Aeson.Types hiding (Result, Error)
import System.Win32.FileMapping.Memory (withMapFile, readMapFile)
import System.Win32.FileMapping.NamePool
#else
import System.Posix.Files (removeLink)
#endif

-- | Inits log chan and returns functions (print message, wait channel)
initLog :: ServerOpts -> IO SessionLog
initLog sopts = do
	msgs <- C.newChan
	l <- newLog (logCfg [("", Log.level_ . T.pack . serverLogLevel $ sopts)]) $ concat [
		[logHandler | not $ serverSilent sopts],
		[chaner msgs],
		[handler text (file f) | f <- maybeToList (serverLog sopts)]]
	let
		listenLog = C.dupChan msgs >>= C.getChanContents
	return $ SessionLog l listenLog (stopLog l)
	where
		logHandler
			| serverLogNoColor sopts = handler text console
			| otherwise = handler text coloredConsole

-- | Run server
runServer :: ServerOpts -> ServerM IO () -> IO ()
runServer sopts act = bracket (initLog sopts) sessionLogWait $ \slog -> Watcher.withWatcher $ \watcher -> withLog (sessionLogger slog) $ do
	waitSem <- liftIO $ newQSem 0
	sqlDb <- liftIO $ SQLite.initialize (fromMaybe SQLite.sharedMemory $ serverDbFile sopts)
	clientChan <- liftIO F.newChan
#if mingw32_HOST_OS
	mmapPool <- Just <$> liftIO (createPool "hsdev")
#endif
	ghcw <- ghcWorker
	liftIO $ inWorker ghcw $ tmpSession globalDb []
	defs <- liftIO getDefines

	session <- liftIO $ fixIO $ \sess -> do
		let
			setFileCts fpath Nothing = void $ withSession sess $ postSessionUpdater $ do
				Log.sendLog Log.Trace $ "dropping file contents for {}" ~~ fpath
				SQLite.execute "delete from file_contents where file = ?;" (SQLite.Only fpath)
			setFileCts fpath (Just cts) = do
				tm <- getPOSIXTime
				withSession sess $ do
					notChanged <- SQLite.query @_ @(SQLite.Only Bool) "select contents == ? from file_contents where file = ?;" (cts, fpath)
					let
						notChanged' = any SQLite.fromOnly notChanged
					void $ postSessionUpdater $ do
						Log.sendLog Log.Trace $ "setting file contents for {} with mtime = {}" ~~ fpath ~~ show tm
						SQLite.execute "insert or replace into file_contents (file, contents, mtime) values (?, ?, ?);" (fpath, cts, (fromRational (toRational tm) :: Double))
						unless notChanged' $ liftIO $
							writeChan (W.watcherChan watcher) (W.WatchedModule, W.Event W.Modified (view path fpath) tm)

		uw <- startWorker (withSession sess . withSqlConnection) id logAll
		resolveEnvTable <- newLookupTable

		return $ Session
			sqlDb
			(fromMaybe SQLite.sharedMemory $ serverDbFile sopts)
			slog
			watcher
			setFileCts
#if mingw32_HOST_OS
			mmapPool
#endif
			ghcw
			uw
			resolveEnvTable
			(do
				withLog (sessionLogger slog) $ Log.sendLog Log.Trace "stopping server"
				signalQSem waitSem)
			(waitQSem waitSem)
			clientChan
			defs

	_ <- fork $ do
		emptyTask <- async $ return ()
		updaterTask <- newMVar emptyTask
		tasksVar <- newMVar []
		Update.onEvents_ watcher $ \evs -> withSession session $
			void $ Client.runClient def $ Update.processEvents (withSession session . inSessionUpdater . void . Client.runClient def . Update.applyUpdates def) updaterTask tasksVar evs
	liftIO $ runReaderT (runServerM $ (watchDb >> act) `finally` closeSession) session
	where
		closeSession = do
			askSession sessionUpdater >>= liftIO . joinWorker
			Log.sendLog Log.Info "updater worker stopped"
			askSession sessionGhc >>= liftIO . joinWorker
			Log.sendLog Log.Info "ghc worker stopped"
			askSession sessionSqlDatabase >>= liftIO . SQLite.close
			Log.sendLog Log.Info "sql connection closed"


-- | Set initial watch: package-dbs, projects and standalone sources
watchDb :: SessionMonad m => m ()
watchDb = do
	w <- askSession sessionWatcher
	-- TODO: Implement watching package-dbs
	cabals <- SQLite.query_ "select cabal from projects;"
	projects <- mapM (SQLite.loadProject . SQLite.fromOnly) cabals
	liftIO $ mapM_ (\proj -> W.watchProject w proj []) projects

	files <- SQLite.query_ "select file from modules where file is not null and cabal is null;"
	liftIO $ mapM_ (\(SQLite.Only f) -> W.watchModule w (FileModule f Nothing)) files

type Server = Worker (ServerM IO)

-- | Start listening for incoming connections
setupServer :: ServerOpts -> ServerM IO ()
setupServer sopts = do
	q <- liftIO $ newQSem 0
	clientChan <- askSession sessionClients
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
				fork $ withSession session $ Log.scope (T.pack $ show addr') $
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
								liftIO $ void $ F.sendChan clientChan timeoutWait
								processClientSocket (show addr') s'

	Log.sendLog Log.Trace "waiting for starting accept thread..."
	liftIO $ waitQSem q
	logIO "error writing to stdout: " (Log.sendLog Log.Error . fromString) $ liftIO $ putStrLn $ "Server started at port {}" ~~ serverPort sopts
	Log.sendLog Log.Info $ "Server started at port {}" ~~ serverPort sopts

-- | Shutdown server
shutdownServer :: ServerOpts -> ServerM IO ()
shutdownServer sopts = do
	Log.sendLog Log.Trace "waiting for accept thread..."
	serverWait
	Log.sendLog Log.Trace "accept thread stopped"
	liftIO $ unlink (serverPort sopts)
	Log.sendLog Log.Trace "waiting for clients..."
	serverWaitClients
	Log.sendLog Log.Info "server stopped"

startServer :: ServerOpts -> IO Server
startServer sopts = startWorker (runServer sopts) (bracket_ (setupServer sopts) (shutdownServer sopts)) logAll

-- Tiny version with no network stuff
startServer_ :: ServerOpts -> IO Server
startServer_ sopts = startWorker (runServer sopts) id logAll

stopServer :: Server -> IO ()
stopServer s = sendServer_ s ["exit"] >> joinWorker s

withServer :: ServerOpts -> (Server -> IO a) -> IO a
withServer sopts = bracket (startServer sopts) stopServer

withServer_ :: ServerOpts -> (Server -> IO a) -> IO a
withServer_ sopts = bracket (startServer_ sopts) stopServer

inServer :: Server -> ServerM IO a -> IO a
inServer = inWorker

clientCommand :: CommandOptions -> Command -> ServerM IO Result
clientCommand copts c = do
	c' <- liftIO $ canonicalize c
	Client.runClient copts (Client.runCommand c')

parseCommand :: [String] -> Either String Command
parseCommand = parseArgs "hsdev" (info cmdP (progDesc "hsdev tool"))

readCommand :: [String] -> Command
readCommand = either error id . parseCommand

sendServer :: Server -> CommandOptions -> [String] -> IO Result
sendServer srv copts args = case parseCommand args of
	Left e -> hsdevError $ RequestError e (unwords args)
	Right c -> inServer srv (clientCommand copts c)

sendServer_ :: Server -> [String] -> IO Result
sendServer_ srv = sendServer srv def

chaner :: C.Chan Log.Message -> Consumer Log.Message
chaner ch = return $ C.writeChan ch

findPath :: MonadIO m => CommandOptions -> FilePath -> m FilePath
findPath copts f = liftIO $ canonicalize (normalise f') where
	f' = absolutise (fromFilePath $ commandOptionsRoot copts) f

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
	fork $ getChanContents respChan >>= mapM_ (send' . encodeMessage)
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

	flip finally (disconnected linkVar) $
		whileJust_ (liftIO $ F.getChan rchan) $ \req' -> do
			Log.sendLog Log.Trace $ "received >> {}" ~~ fromUtf8 req'
			case decodeMessage req' of
				Left em -> do
					Log.sendLog Log.Warning $ "Invalid request {}" ~~ fromUtf8 req'
					answer $ set msg (Message Nothing $ responseError $ RequestError "invalid request" $ fromUtf8 req') em
				Right m -> fork $ withSession s $ Log.scope (T.pack name) $ Log.scope "req" $
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
	fork $ finally
		(Net.getContents s >>= mapM_ (F.sendChan recvChan) . L.lines)
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
newtype MmapFile = MmapFile String

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
