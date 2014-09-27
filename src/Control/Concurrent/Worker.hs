module Control.Concurrent.Worker (
	Worker(..),
	startWorker,
	sendTask, pushTask,
	stopWorker,

	module Control.Concurrent.Task
	) where

import Control.Concurrent.MVar
import Control.Monad (void, when)
import Control.Monad.IO.Class
import Control.Monad.Catch

import Control.Concurrent.FiniteChan
import Control.Concurrent.Task

data Worker m = Worker {
	workerChan :: Chan (m ()),
	workerTask :: MVar (Task ()),
	workerRestart :: IO Bool }

startWorker :: MonadIO m => (m () -> IO ()) -> (m () -> m ()) -> IO (Worker m)
startWorker run initialize = do
	ch <- newChan
	taskVar <- newEmptyMVar
	let
		start = forkTask $ run $ initialize processWork
		processWork = liftIO (getChan ch) >>= maybe (return ()) (>> processWork)
	start >>= putMVar taskVar
	let
		restart = do
			f <- readMVar taskVar >>= taskFailed
			when f (start >>= void . swapMVar taskVar)
			return f
	return $ Worker ch taskVar restart

sendTask :: (MonadCatch m, MonadIO m) => Worker m -> m a -> IO (Task a)
sendTask w = runTask $ putChan (workerChan w)

pushTask :: (MonadCatch m, MonadIO m) => Worker m -> m a -> IO (Task a)
pushTask w act = workerRestart w >> sendTask w act

stopWorker :: Worker m -> IO ()
stopWorker = closeChan . workerChan
