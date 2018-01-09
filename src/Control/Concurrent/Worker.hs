{-# LANGUAGE RankNTypes #-}

module Control.Concurrent.Worker (
	Worker(..), WorkerStopped(..),
	startWorker, workerAlive, workerAbort, workerDone,
	sendTask, pushTask,
	restartWorker, stopWorker, joinWorker, syncTask,
	inWorkerWith, inWorker,

	module Control.Concurrent.Async
	) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Except
import Data.Maybe (isJust, isNothing)
import Data.Typeable

import Control.Concurrent.FiniteChan
import Control.Concurrent.Async

data Worker m = Worker {
	workerChan :: Chan (Async (), m ()),
	workerWrap :: forall a. m a -> m a,
	workerTask :: MVar (Async ()),
	workerTouch :: IO () }

data WorkerStopped = WorkerStopped deriving (Show, Typeable)

instance Exception WorkerStopped

-- | Create new worker
startWorker :: MonadIO m => (m () -> IO ()) -> (m () -> m ()) -> (forall a. m a -> m a) -> IO (Worker m)
startWorker run initialize wrap = do
	ch <- newChan
	taskVar <- newEmptyMVar
	let
		start = async $ run $ initialize go
		go = do
			t <- fmap snd <$> liftIO (getChan ch)
			maybe (return ()) (>> go) t
	start >>= putMVar taskVar
	let
		restart = do
			done <- doneChan ch
			unless done $ do
				task <- readMVar taskVar
				stopped <- isJust <$> poll task
				when stopped (start >>= void . swapMVar taskVar)
	return $ Worker ch wrap taskVar restart

-- | Check whether worker alive
workerAlive :: Worker m -> IO Bool
workerAlive w = do
	task <- readMVar $ workerTask w
	isNothing <$> poll task

-- | Abort all unconsumed tasks
workerAbort :: Worker w -> IO ()
workerAbort w = do
	alive' <- workerAlive w
	unless alive' $ fix $ \loop -> do
		waiting <- fmap fst <$> liftIO (getChan $ workerChan w)
		maybe (return ()) (\w' -> cancel w' >> loop) waiting

workerDone :: Worker m -> IO Bool
workerDone = doneChan . workerChan

sendTask :: (MonadCatch m, MonadIO m) => Worker m -> m a -> IO (Async a)
sendTask w act = mfix $ \async' -> do
	var <- newEmptyMVar
	let
		act' = (workerWrap w act >>= liftIO . putMVar var . Right) `catch` onError
		onError :: MonadIO m => SomeException -> m ()
		onError = liftIO . putMVar var . Left
		f = do
			p <- sendChan (workerChan w) (void async', void act')
			unless p $ putMVar var (Left $ SomeException WorkerStopped)
			r <- takeMVar var
			either throwM return r
	async f

pushTask :: (MonadCatch m, MonadIO m) => Worker m -> m a -> IO (Async a)
pushTask w act = workerTouch w >> sendTask w act

restartWorker :: Worker m -> IO ()
restartWorker w = do
	async' <- readMVar (workerTask w)
	cancel async'
	void $ waitCatch async'

-- | Close worker channel
stopWorker :: Worker m -> IO ()
stopWorker = closeChan . workerChan

-- | Stop worker and wait for it
joinWorker :: Worker m -> IO ()
joinWorker w = do
	stopWorker w
	async' <- readMVar $ workerTask w
	void $ waitCatch async'
	workerAbort w

-- | Send empty task and wait until worker run it
syncTask :: (MonadCatch m, MonadIO m) => Worker m -> IO ()
syncTask w = pushTask w (return ()) >>= void . wait

-- | Run action in worker and wait for result
inWorkerWith :: (MonadIO m, MonadCatch m, MonadIO n) => (SomeException -> n a) -> Worker m -> m a -> n a
inWorkerWith err w act = liftIO (pushTask w act) >>= (liftIO . waitCatch >=> either err return)

-- | Run action in worker and wait for result
inWorker :: (MonadIO m, MonadCatch m) => Worker m -> m a -> IO a
inWorker w act = pushTask w act >>= liftIO . wait
