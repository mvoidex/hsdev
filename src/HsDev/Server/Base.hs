{-# LANGUAGE CPP, OverloadedStrings #-}

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
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import System.Log.Simple hiding (Level(..), Message(..), Command(..))
import System.Log.Simple.Base (writeLog)
import qualified System.Log.Simple.Base as Log

import qualified Control.Concurrent.FiniteChan as F
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
#else
import System.Posix.Process
import System.Posix.IO
#endif


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

instance FormatBuild Log.Level where

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
	defs <- getDefines
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
			defs
	_ <- forkIO $ Update.onEvent watcher (Update.processEvent $ Update.settings copts [] False False)
	act copts

type Server = Worker (ReaderT CommandOptions IO)

startServer :: ServerOpts -> IO Server
startServer sopts = startWorker (runServer sopts . runReaderT) id id

inServer :: Server -> Command -> IO Result
inServer srv c = inWorker srv (ReaderT (`Client.runCommand` c))

chaner :: F.Chan String -> Consumer Text
chaner ch = Consumer withChan where
	withChan f = f (F.putChan ch . T.unpack)

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
