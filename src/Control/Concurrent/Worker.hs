{-# LANGUAGE RankNTypes #-}

module Control.Concurrent.Worker (
	Worker(..), WorkerStopped(..),
	startWorker, workerAlive, workerDone,
	sendTask, stopWorker, joinWorker, syncTask,
	inWorkerWith, inWorker,

	module Control.Concurrent.Async
	) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Except
import Data.Maybe (isNothing)
import Data.Typeable

import Control.Concurrent.FiniteChan
import Control.Concurrent.Async

data Worker m = Worker {
	workerChan :: Chan (Async (), m ()),
	workerTask :: MVar (Async ()) }

data WorkerStopped = WorkerStopped deriving (Show, Typeable)

instance Exception WorkerStopped

-- | Create new worker
startWorker :: MonadIO m => (m () -> IO ()) -> (m () -> m ()) -> (m () -> m ()) -> IO (Worker m)
startWorker run initialize handleErrs = do
	ch <- newChan
	taskVar <- newEmptyMVar
	let
		job = onException (run $ initialize go) $ do
			closeChan ch
			abort
		go = do
			t <- fmap snd <$> liftIO (getChan ch)
			maybe (return ()) (\f' -> handleErrs f' >> go) t
		abort = do
			a <- fmap fst <$> liftIO (getChan ch)
			maybe (return ()) (\a' -> liftIO (cancel a') >> abort) a
	async job >>= putMVar taskVar
	return $ Worker ch taskVar

-- | Check whether worker alive
workerAlive :: Worker m -> IO Bool
workerAlive w = do
	task <- readMVar $ workerTask w
	isNothing <$> poll task

workerDone :: Worker m -> IO Bool
workerDone = doneChan . workerChan

sendTask :: (MonadCatch m, MonadIO m) => Worker m -> m a -> IO (Async a)
sendTask w act = mfix $ \async' -> do
	var <- newEmptyMVar
	let
		act' = (act >>= liftIO . putMVar var . Right) `catch` onErr
		onErr :: MonadIO m => SomeException -> m ()
		onErr = liftIO . putMVar var . Left
		f = do
			p <- sendChan (workerChan w) (void async', void act')
			unless p $ putMVar var (Left $ SomeException WorkerStopped)
			r <- takeMVar var
			either throwM return r
	async f

-- | Close worker channel
stopWorker :: Worker m -> IO ()
stopWorker = closeChan . workerChan

-- | Stop worker and wait for it
joinWorker :: Worker m -> IO ()
joinWorker w = do
	stopWorker w
	async' <- readMVar $ workerTask w
	void $ waitCatch async'

-- | Send empty task and wait until worker run it
syncTask :: (MonadCatch m, MonadIO m) => Worker m -> IO ()
syncTask w = sendTask w (return ()) >>= void . wait

-- | Run action in worker and wait for result
inWorkerWith :: (MonadIO m, MonadCatch m, MonadIO n) => (SomeException -> n a) -> Worker m -> m a -> n a
inWorkerWith err w act = liftIO (sendTask w act) >>= (liftIO . waitCatch >=> either err return)

-- | Run action in worker and wait for result
inWorker :: (MonadIO m, MonadCatch m) => Worker m -> m a -> IO a
inWorker w act = sendTask w act >>= liftIO . wait
