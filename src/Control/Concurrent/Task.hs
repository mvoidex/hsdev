{-# LANGUAGE DeriveDataTypeable #-}

module Control.Concurrent.Task (
	Task(..), TaskException(..),
	taskStarted, taskRunning, taskStopped, taskDone, taskFailed, taskCancelled,
	taskWaitStart, taskWait, taskKill, taskCancel,
	runTask, forkTask
	) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Either
import Data.Maybe
import Data.Traversable
import Data.Typeable

-- | Task result
data Task a = Task {
	taskStart :: MVar (Maybe (SomeException -> IO ())),
	taskResult :: MVar (Either SomeException a) }

data TaskException = TaskCancelled | TaskKilled deriving (Eq, Ord, Enum, Bounded, Read, Show, Typeable)

instance Exception TaskException

taskStarted :: Task a -> IO Bool
taskStarted = fmap (maybe False isJust) . tryReadMVar . taskStart

taskRunning :: Task a -> IO Bool
taskRunning t = (&&) <$> taskStarted t <*> (not <$> taskStopped t)

taskStopped :: Task a -> IO Bool
taskStopped = fmap not . isEmptyMVar . taskResult

taskDone :: Task a -> IO Bool
taskDone = fmap (maybe False isRight) . tryReadMVar . taskResult

taskFailed :: Task a -> IO Bool
taskFailed = fmap (maybe False isLeft) . tryReadMVar . taskResult

taskCancelled :: Task a -> IO Bool
taskCancelled = fmap (maybe False isNothing) . tryReadMVar . taskStart

-- | Wait until task starts or be cancelled, returns True if started
taskWaitStart :: Task a -> IO Bool
taskWaitStart = (`withMVar` (return . isJust)) . taskStart

-- | Wait for task
taskWait :: Task a -> IO (Either SomeException a)
taskWait = takeMVar . taskResult

-- | Kill task
taskKill :: Task a -> IO ()
taskKill =
	tryTakeMVar . taskStart >=>
	void . traverse ($ toException TaskKilled) . join

-- | Cancel task if it is not started yet
taskCancel :: Task a -> IO Bool
taskCancel t = do
	aborted <- tryPutMVar (taskStart t) Nothing
	when aborted $ void $ tryPutMVar (taskResult t) (Left $ toException TaskCancelled)
	return aborted

runTask :: (MonadCatch m, MonadIO m, MonadIO n) => (m () -> n ()) -> m a -> n (Task a)
runTask f act = do
	throwVar <- liftIO newEmptyMVar
	resultVar <- liftIO newEmptyMVar
	f $ handle (liftIO . putMVar resultVar . Left) $ do
		th <- liftIO myThreadId
		ok <- liftIO $ tryPutMVar throwVar (Just $ throwTo th)
		when ok $ act >>= liftIO . putMVar resultVar . Right
	return $ Task throwVar resultVar

-- | Run task in separate thread
forkTask :: IO a -> IO (Task a)
forkTask = runTask (void . forkIO)
