module Control.Concurrent.Util (
	fork, race, timeout, withSync, withSync_
	) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class

fork :: (MonadIO m, Functor m) => IO () -> m ()
fork = liftIO . void . forkIO

race :: [IO a] -> IO a
race acts = do
	var <- newEmptyMVar
	ids <- forM acts $ \a -> forkIO ((a >>= putMVar var) `catch` ignoreError)
	r <- takeMVar var
	forM_ ids killThread
	return r
	where
		ignoreError :: SomeException -> IO ()
		ignoreError _ = return ()

timeout :: Int -> IO a -> IO (Maybe a)
timeout 0 act = fmap Just act
timeout tm act = race [
	fmap Just act,
	threadDelay tm >> return Nothing]

withSync :: a -> ((a -> IO ()) -> IO b) -> IO a
withSync v act = do
	sync <- newEmptyMVar
	void $ forkIO $ void $ act (putMVar sync) `onException` putMVar sync v
	takeMVar sync

withSync_ :: (IO () -> IO a) -> IO ()
withSync_ act = withSync () $ \sync -> act (sync ())
