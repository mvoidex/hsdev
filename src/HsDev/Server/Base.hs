{-# LANGUAGE CPP, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Server.Base (
	initLog, runServer, Server, startServer, inServer,
	withCache, writeCache, readCache,

	module HsDev.Server.Types,
	module HsDev.Server.Message
	) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Default
import qualified Data.Map as M
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import System.Log.Simple hiding (Level(..), Message(..), Command(..))
import qualified System.Log.Simple.Base as Log
import qualified System.Log.Simple as Log

import qualified Control.Concurrent.FiniteChan as F
import System.Directory.Paths (canonicalize)
import qualified System.Directory.Watcher as Watcher
import Text.Format ((~~), FormatBuild(..))

import qualified HsDev.Cache.Structured as SC
import qualified HsDev.Client.Commands as Client
import HsDev.Database
import qualified HsDev.Database.Async as DB
import qualified HsDev.Database.Update as Update
import HsDev.Inspect (getDefines)
import HsDev.Tools.Ghc.Worker
import HsDev.Tools.GhcMod (ghcModMultiWorker)
import HsDev.Server.Types
import HsDev.Server.Message
import HsDev.Util

#if mingw32_HOST_OS
import System.Win32.FileMapping.NamePool
#endif

-- | Inits log chan and returns functions (print message, wait channel)
initLog :: ServerOpts -> IO (Log, Log.Level -> String -> IO (), IO [String], IO ())
initLog sopts = do
	msgs <- F.newChan
	l <- newLog (constant [rule']) $ concat [
		[logger text console],
		[logger text (chaner msgs)],
		maybeToList $ (logger text . file) <$> serverLog sopts]
	Log.writeLog l Log.Info ("Log politics: low = {}, high = {}" ~~ logLow ~~ logHigh)
	let
		listenLog = F.dupChan msgs >>= F.readChan
	return (l, \lev -> writeLog l lev . T.pack, listenLog, stopLog l)
	where
		rule' :: Log.Rule
		rule' = parseRule_ $ T.pack ("/: " ++ serverLogConfig sopts)
		(Log.Politics logLow logHigh) = Log.rulePolitics rule' Log.defaultPolitics

instance FormatBuild Log.Level where

-- | Run server
runServer :: ServerOpts -> ServerM IO () -> IO ()
runServer sopts act = bracket (initLog sopts) (\(_, _, _, x) -> x) $ \(logger', outputStr, listenLog, waitOutput) -> Log.scopeLog logger' (T.pack "hsdev") $ Watcher.withWatcher $ \watcher -> do
	waitSem <- newQSem 0
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
	defs <- getDefines
	let
		session = Session
			db
			(writeCache sopts)
			(readCache sopts)
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
			(do
				outputStr Log.Trace "stopping server"
				signalQSem waitSem)
			(waitQSem waitSem)
			defs
	_ <- forkIO $ Update.onEvent watcher $ \w e -> withSession session $
		void $ Client.runClient def $ Update.processEvent def w e
	runReaderT (runServerM act) session

type Server = Worker (ServerM IO)

startServer :: ServerOpts -> IO Server
startServer sopts = startWorker (runServer sopts) id id

inServer :: Server -> CommandOptions -> Command -> IO Result
inServer srv copts c = do
	c' <- canonicalize c
	inWorker srv (Client.runClient copts $ Client.runCommand c')

chaner :: F.Chan String -> Consumer Text
chaner ch = Consumer withChan where
	withChan f = f (F.putChan ch . T.unpack)

-- | Perform action on cache
withCache :: Monad m => ServerOpts -> a -> (FilePath -> m a) -> m a
withCache sopts v onCache = case serverCache sopts of
	Nothing -> return v
	Just cdir -> onCache cdir

writeCache :: SessionMonad m => ServerOpts -> Database -> m ()
writeCache sopts db = withCache sopts () $ \cdir -> do
	Log.log Log.Info $ "writing cache to {}" ~~ cdir
	logIO "cache writing exception: " (Log.log Log.Error . fromString) $ do
		let
			sd = structurize db
		liftIO $ SC.dump cdir sd
		forM_ (M.keys (structuredCabals sd)) $ \c -> Log.log Log.Debug ("cache write: cabal {}" ~~ show c)
		forM_ (M.keys (structuredProjects sd)) $ \p -> Log.log Log.Debug ("cache write: project {}" ~~ p)
		case allModules (structuredFiles sd) of
			[] -> return ()
			ms -> Log.log Log.Debug $ "cache write: {} files" ~~ length ms
	Log.log Log.Info $ "cache saved to {}" ~~ cdir

readCache :: SessionMonad m => ServerOpts -> (FilePath -> ExceptT String IO Structured) -> m (Maybe Database)
readCache sopts act = do
	s <- getSession
	liftIO $ withSession s $ withCache sopts Nothing $ \fpath -> do
		res <- liftIO $ runExceptT $ act fpath
		either cacheErr cacheOk res
	where
		cacheErr e = Log.log Log.Error ("Error reading cache: {}" ~~ e) >> return Nothing
		cacheOk s = do
			forM_ (M.keys (structuredCabals s)) $ \c -> Log.log Log.Debug ("cache read: cabal {}" ~~ show c)
			forM_ (M.keys (structuredProjects s)) $ \p -> Log.log Log.Debug ("cache read: project {}" ~~ p)
			case allModules (structuredFiles s) of
				[] -> return ()
				ms -> Log.log Log.Debug $ "cache read: {} files" ~~ length ms
			return $ Just $ merge s
