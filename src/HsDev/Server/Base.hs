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
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Default
import qualified Data.Map as M
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import System.Log.Simple hiding (Level(..), Message(..), Command(..), (%=))
import qualified System.Log.Simple.Base as Log
import qualified System.Log.Simple as Log
import System.Directory (removeDirectoryRecursive, createDirectoryIfMissing)
import System.FilePath

import qualified Control.Concurrent.FiniteChan as F
import System.Directory.Paths (canonicalize)
import qualified System.Directory.Watcher as Watcher
import Text.Format ((~~), FormatBuild(..), (~%))

import qualified HsDev.Cache as Cache
import qualified HsDev.Cache.Structured as SC
import qualified HsDev.Client.Commands as Client
import HsDev.Database
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
	rulesVar <- newMVar [ruleStr]
	let
		getRules = do
			rs <- readMVar rulesVar
			return $ map (parseRule_ . fromString) rs
	l <- newLog (return getRules) $ concat [
		[logger text console | not $ serverSilent sopts],
		[logger text (chaner msgs)],
		maybeToList $ (logger text . file) <$> serverLog sopts]
	Log.writeLog l Log.Info ("Log politics: low = {}, high = {}" ~~ logLow ~~ logHigh)
	let
		listenLog = F.dupChan msgs >>= F.readChan
	return $ SessionLog l rulesVar listenLog (stopLog l)
	where
		ruleStr :: String
		ruleStr = "/: {}" ~~ serverLogConfig sopts
		(Log.Politics logLow logHigh) = Log.rulePolitics (parseRule_ (fromString ruleStr)) Log.defaultPolitics

instance FormatBuild Log.Level where

-- | Run server
runServer :: ServerOpts -> ServerM IO () -> IO ()
runServer sopts act = bracket (initLog sopts) sessionLogWait $ \slog -> Log.scopeLog (sessionLogger slog) (T.pack "hsdev") $ Watcher.withWatcher $ \watcher -> do
	waitSem <- newQSem 0
	db <- DB.newAsync
	let
		outputStr = Log.writeLog (sessionLogger slog)
	withCache sopts () $ \cdir -> do
		outputStr Log.Trace $ "Checking cache version in {}" ~~ cdir 
		ver <- Cache.readVersion $ cdir </> Cache.versionCache
		outputStr Log.Debug $ "Cache version: {}" ~~ strVersion ver
		unless (sameVersion (cutVersion version) (cutVersion ver)) $ ignoreIO $ do
			outputStr Log.Info $ "Cache version ({cache}) is incompatible with hsdev version ({hsdev}), removing cache ({dir})" ~~
				("cache" ~% strVersion ver) ~~
				("hsdev" ~% strVersion version) ~~
				("dir" ~% cdir)
			-- drop cache
			removeDirectoryRecursive cdir
		outputStr Log.Debug $ "Writing new cache version: {}" ~~ strVersion version
		createDirectoryIfMissing True cdir
		Cache.writeVersion $ cdir </> Cache.versionCache
	when (serverLoad sopts) $ withCache sopts () $ \cdir -> do
		outputStr Log.Info $ "Loading cache from {}" ~~ cdir
		dbCache <- liftA merge <$> SC.load cdir
		case dbCache of
			Left err -> outputStr Log.Error $ "Failed to load cache: {}" ~~ err
			Right dbCache' -> DB.update db (return dbCache')
#if mingw32_HOST_OS
	mmapPool <- Just <$> createPool "hsdev"
#endif
	ghcw <- withLog (sessionLogger slog) $ ghcWorker
	defs <- getDefines
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
		forM_ (M.keys (structuredPackageDbs sd)) $ \c -> Log.log Log.Debug ("cache write: cabal {}" ~~ show c)
		forM_ (M.keys (structuredProjects sd)) $ \p -> Log.log Log.Debug ("cache write: project {}" ~~ p)
		case (structuredFiles sd) ^.. databaseModules . each of
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
			forM_ (M.keys (structuredPackageDbs s)) $ \c -> Log.log Log.Debug ("cache read: cabal {}" ~~ show c)
			forM_ (M.keys (structuredProjects s)) $ \p -> Log.log Log.Debug ("cache read: project {}" ~~ p)
			case (structuredFiles s) ^.. databaseModules . each of
				[] -> return ()
				ms -> Log.log Log.Debug $ "cache read: {} files" ~~ length ms
			return $ Just $ merge s
