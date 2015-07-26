{-# LANGUAGE RankNTypes #-}

module Control.Concurrent.Worker (
	Worker(..),
	startWorker,
	sendTask, pushTask,
	stopWorker, syncTask,

	module Control.Concurrent.Task
	) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Except

import Control.Concurrent.FiniteChan
import Control.Concurrent.Task

data Worker m = Worker {
	workerChan :: Chan (Task (), m ()),
	workerWrap :: forall a. m a -> m a,
	workerTask :: MVar (Task ()),
	workerRestart :: IO Bool }

-- | Create new worker
startWorker :: MonadIO m => (m () -> IO ()) -> (m () -> m ()) -> (forall a. m a -> m a) -> IO (Worker m)
startWorker run initialize wrap = do
	ch <- newChan
	taskVar <- newEmptyMVar
	let
		start = forkTask $ do
			run $ initialize processWork
			processSkip
		processWork = whileJust (liftM (fmap snd) $ liftIO $ getChan ch) id
		processSkip = whileJust (liftM (fmap fst) $ getChan ch) taskStop
		whileJust :: Monad m => m (Maybe a) -> (a -> m b) -> m  ()
		whileJust v act = v >>= maybe (return ()) (\x -> act x >> whileJust v act)
	start >>= putMVar taskVar
	let
		restart = do
			task <- readMVar taskVar
			stopped <- taskStopped task
			when stopped (start >>= void . swapMVar taskVar)
			return stopped
	return $ Worker ch wrap taskVar restart

sendTask :: (MonadCatch m, MonadIO m) => Worker m -> m a -> IO (Task a)
sendTask w = runTask_ putTask' . workerWrap w where
	putTask' t act = putChan (workerChan w) (fmap (const ()) t, act)

pushTask :: (MonadCatch m, MonadIO m) => Worker m -> m a -> IO (Task a)
pushTask w act = workerRestart w >> sendTask w act

stopWorker :: Worker m -> IO ()
stopWorker = closeChan . workerChan

-- | Send empty task and wait until worker run it
syncTask :: (MonadCatch m, MonadIO m) => Worker m -> IO ()
syncTask w = pushTask w (return ()) >>= void . taskWait
