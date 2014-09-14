module Control.Concurrent.Worker (
	Worker(..),
	worker_, worker,
	stopWorker,
	ignoreException
	) where

import Control.Exception (SomeException)
import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.CatchIO

import Control.Concurrent (forkIO)
import Control.Concurrent.FiniteChan

data Worker a = Worker {
	sendWork :: a -> IO (),
	workerChan :: Chan a }

worker_ :: MonadIO m => (m () -> IO ()) -> (m () -> m ()) -> (a -> m b) -> IO (Worker a)
worker_ run initialize work = worker run initialize' work' where
	initialize' f = initialize $ f ()
	work' _ = work

worker :: MonadIO m => (m () -> IO ()) -> ((s -> m ()) -> m ()) -> (s -> a -> m b) -> IO (Worker a)
worker run initialize work = do
	ch <- newChan
	void $ forkIO $ run $ initialize $ \s ->
		liftIO (readChan ch) >>= mapM_ (work s)
	return $ Worker (putChan ch) ch

stopWorker :: Worker a -> IO ()
stopWorker = closeChan . workerChan

ignoreException :: MonadCatchIO m => m () -> m ()
ignoreException act = catch act onError where
	onError :: MonadCatchIO m => SomeException -> m ()
	onError _ = return ()
