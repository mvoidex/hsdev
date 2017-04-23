{-# LANGUAGE CPP, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Server.Base (
	initLog, runServer, Server, startServer, inServer, clientCommand, parseCommand, readCommand,
	sendServer, sendServer_,
	withCache, writeCache, readCache,

	module HsDev.Server.Types,
	module HsDev.Server.Message
	) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Default
import Data.Maybe
import qualified Data.Text as T (pack)
import Options.Applicative (info, progDesc)
import System.Log.Simple hiding (Level(..), Message)
import qualified System.Log.Simple.Base as Log (level_)
import qualified System.Log.Simple as Log
import System.Directory (removeDirectoryRecursive, createDirectoryIfMissing)
import System.FilePath

import qualified Control.Concurrent.FiniteChan as F
import System.Directory.Paths (canonicalize)
import qualified System.Directory.Watcher as Watcher
import Text.Format ((~~), (~%))

import qualified HsDev.Cache as Cache
import qualified HsDev.Client.Commands as Client
import HsDev.Database
import HsDev.Error
import qualified HsDev.Database.Async as DB
import qualified HsDev.Database.Update as Update
import HsDev.Inspect (getDefines)
import HsDev.Tools.Ghc.Worker
import HsDev.Server.Types
import HsDev.Server.Message
import HsDev.Util

#if mingw32_HOST_OS
import System.Win32.FileMapping.NamePool
#endif

-- | Inits log chan and returns functions (print message, wait channel)
initLog :: ServerOpts -> IO SessionLog
initLog sopts = do
	msgs <- F.newChan
	l <- newLog (logCfg [("", Log.level_ . T.pack . serverLogLevel $ sopts)]) $ concat [
		[handler text coloredConsole | not $ serverSilent sopts],
		[chaner msgs],
		[handler text (file f) | f <- maybeToList (serverLog sopts)]]
	let
		listenLog = F.dupChan msgs >>= F.readChan
	return $ SessionLog l listenLog (stopLog l)

-- | Run server
runServer :: ServerOpts -> ServerM IO () -> IO ()
runServer sopts act = bracket (initLog sopts) sessionLogWait $ \slog -> Watcher.withWatcher $ \watcher -> withLog (sessionLogger slog) $ do
	waitSem <- liftIO $ newQSem 0
	db <- liftIO $ DB.newAsync
	withCache sopts () $ \cdir -> do
		Log.sendLog Log.Trace $ "Checking cache version in {}" ~~ cdir 
		ver <- liftIO $ Cache.readVersion $ cdir </> Cache.versionCache
		Log.sendLog Log.Debug $ "Cache version: {}" ~~ strVersion ver
		unless (sameVersion (cutVersion version) (cutVersion ver)) $ ignoreIO $ do
			Log.sendLog Log.Info $ "Cache version ({cache}) is incompatible with hsdev version ({hsdev}), removing cache ({dir})" ~~
				("cache" ~% strVersion ver) ~~
				("hsdev" ~% strVersion version) ~~
				("dir" ~% cdir)
			-- drop cache
			liftIO $ removeDirectoryRecursive cdir
		Log.sendLog Log.Debug $ "Writing new cache version: {}" ~~ strVersion version
		liftIO $ createDirectoryIfMissing True cdir
		liftIO $ Cache.writeVersion $ cdir </> Cache.versionCache
	when (serverLoad sopts) $ withCache sopts () $ \cdir -> do
		Log.sendLog Log.Info $ "Loading cache from {}" ~~ cdir
		-- dbCache <- liftA merge <$> SC.load cdir
		-- case dbCache of
		-- 	Left err -> outputStr Log.Error $ "Failed to load cache: {}" ~~ err
		-- 	Right dbCache' -> DB.update db (return dbCache')
#if mingw32_HOST_OS
	mmapPool <- Just <$> liftIO (createPool "hsdev")
#endif
	ghcw <- ghcWorker
	defs <- liftIO getDefines
	let
		session = Session
			db
			(writeCache sopts)
			(readCache sopts)
			slog
			watcher
#if mingw32_HOST_OS
			mmapPool
#endif
			ghcw
			(do
				withLog (sessionLogger slog) $ Log.sendLog Log.Trace "stopping server"
				signalQSem waitSem)
			(waitQSem waitSem)
			defs
	_ <- liftIO $ forkIO $ Update.onEvent watcher $ \w e -> withSession session $
		void $ Client.runClient def $ Update.processEvent def w e
	liftIO $ runReaderT (runServerM act) session

type Server = Worker (ServerM IO)

startServer :: ServerOpts -> IO Server
startServer sopts = startWorker (runServer sopts) id id

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
sendServer srv copts args = do
	case parseCommand args of
		Left e -> hsdevError $ RequestError e (unwords args)
		Right c -> inServer srv (clientCommand copts c)

sendServer_ :: Server -> [String] -> IO Result
sendServer_ srv = sendServer srv def

chaner :: F.Chan Log.Message -> Consumer Log.Message
chaner ch = return $ F.putChan ch

-- | Perform action on cache
withCache :: Monad m => ServerOpts -> a -> (FilePath -> m a) -> m a
withCache sopts v onCache = case serverCache sopts of
	Nothing -> return v
	Just cdir -> onCache cdir

writeCache :: SessionMonad m => ServerOpts -> Database -> m ()
writeCache sopts _ = withCache sopts () $ \cdir -> do
	Log.sendLog Log.Info $ "writing cache to {}" ~~ cdir
	Log.sendLog Log.Warning $ "not implemented {}" ~~ cdir

readCache :: SessionMonad m => ServerOpts -> (FilePath -> ExceptT String IO Database) -> m (Maybe Database)
readCache sopts act = do
	s <- getSession
	liftIO $ withSession s $ withCache sopts Nothing $ \fpath -> do
		res <- liftIO $ runExceptT $ act fpath
		either cacheErr cacheOk res
	where
		cacheErr e = Log.sendLog Log.Error ("Error reading cache: {}" ~~ e) >> return Nothing
		cacheOk = return . Just
