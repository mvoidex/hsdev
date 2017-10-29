module Control.Concurrent.Util (
	fork, timeout, sync
	) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class

fork :: MonadIO m => IO () -> m ()
fork = liftIO . void . forkIO

timeout :: Int -> IO a -> IO (Maybe a)
timeout 0 act = liftM Just act
timeout tm act = liftM (either Just (const Nothing)) $ race act (threadDelay tm)

sync :: MonadIO m => (IO () -> m a) -> m a
sync act = do
	syncVar <- liftIO newEmptyMVar
	r <- act $ putMVar syncVar ()
	liftIO $ takeMVar syncVar
	return r
