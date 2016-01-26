module Control.Concurrent.Util (
	fork, timeout
	) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class

fork :: (MonadIO m, Functor m) => IO () -> m ()
fork = liftIO . void . forkIO

timeout :: Int -> IO a -> IO (Maybe a)
timeout 0 act = liftM Just act
timeout tm act = liftM (either Just (const Nothing)) $ race act (threadDelay tm)
