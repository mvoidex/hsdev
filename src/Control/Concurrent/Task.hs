{-# LANGUAGE DeriveDataTypeable #-}

module Control.Concurrent.Task (
	Task(..), TaskException(..), TaskResult(..),
	taskStarted, taskRunning, taskStopped, taskDone, taskFailed, taskCancelled,
	taskWaitStart, taskWait, taskKill, taskCancel, taskStop,
	runTask, runTask_, runTaskTry, runTaskError, forkTask, tryT
	) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Error
import Control.Monad.Catch
import Data.Either
import Data.Maybe
import Data.Traversable
import Data.Typeable

-- | Task result
data Task a = Task {
	taskStart :: MVar (Maybe (SomeException -> IO ())),
	taskResult :: TaskResult a }

data TaskException = TaskCancelled | TaskKilled deriving (Eq, Ord, Enum, Bounded, Read, Show, Typeable)

instance Exception TaskException

data TaskResult a = TaskResult {
	taskResultEmpty :: IO Bool,
	taskResultTryRead :: IO (Maybe (Either SomeException a)),
	taskResultTake :: IO (Either SomeException a),
	taskResultFail :: SomeException -> IO () }

instance Functor TaskResult where
	fmap f r = TaskResult {
		taskResultEmpty = taskResultEmpty r,
		taskResultTryRead = fmap (fmap (fmap f)) $ taskResultTryRead r,
		taskResultTake = fmap (fmap f) $ taskResultTake r,
		taskResultFail = taskResultFail r }

instance Functor Task where
	fmap f t = Task {
		taskStart = taskStart t,
		taskResult = fmap f (taskResult t) }

taskStarted :: Task a -> IO Bool
taskStarted = fmap (maybe False isJust) . tryReadMVar . taskStart

taskRunning :: Task a -> IO Bool
taskRunning t = (&&) <$> taskStarted t <*> (not <$> taskStopped t)

taskStopped :: Task a -> IO Bool
taskStopped = fmap not . taskResultEmpty . taskResult

taskDone :: Task a -> IO Bool
taskDone = fmap (maybe False isRight) . taskResultTryRead . taskResult

taskFailed :: Task a -> IO Bool
taskFailed = fmap (maybe False isLeft) . taskResultTryRead . taskResult

taskCancelled :: Task a -> IO Bool
taskCancelled = fmap (maybe False isNothing) . tryReadMVar . taskStart

-- | Wait until task starts or be cancelled, returns True if started
taskWaitStart :: Task a -> IO Bool
taskWaitStart = (`withMVar` (return . isJust)) . taskStart

-- | Wait for task
taskWait :: Task a -> IO (Either SomeException a)
taskWait = taskResultTake . taskResult

-- | Kill task
taskKill :: Task a -> IO ()
taskKill =
	tryTakeMVar . taskStart >=>
	void . traverse ($ toException TaskKilled) . join

-- | Cancel task if it is not started yet
taskCancel :: Task a -> IO Bool
taskCancel t = do
	aborted <- tryPutMVar (taskStart t) Nothing
	when aborted $ void $ taskResultFail (taskResult t) (toException TaskCancelled)
	return aborted

-- | Cancel or kill task, returns whether it was cancelled
taskStop :: Task a -> IO Bool
taskStop t = do
	cancelled <- taskCancel t
	when (not cancelled) $ taskKill t
	return cancelled

runTask :: (MonadCatch m, MonadIO m, MonadIO n) => (m () -> n ()) -> m a -> n (Task a)
runTask f = runTask_ (const f)

runTask_ :: (MonadCatch m, MonadIO m, MonadIO n) => (Task a -> m () -> n ()) -> m a -> n (Task a)
runTask_ f = runTaskTry f . liftM Right

runTaskTry :: (MonadCatch m, MonadIO m, MonadIO n) => (Task a -> m () -> n ()) -> m (Either SomeException a) -> n (Task a)
runTaskTry f act = do
	throwVar <- liftIO newEmptyMVar
	resultVar <- liftIO newEmptyMVar
	f (Task throwVar $ toResult resultVar) $ handle (liftIO . putMVar resultVar . Left) $ do
		th <- liftIO myThreadId
		ok <- liftIO $ tryPutMVar throwVar (Just $ throwTo th)
		when ok $ act >>= liftIO . putMVar resultVar
	return $ Task throwVar $ toResult resultVar
	where
		toResult :: MVar (Either SomeException a) -> TaskResult a
		toResult var = TaskResult {
			taskResultEmpty = isEmptyMVar var,
			taskResultTryRead = tryReadMVar var,
			taskResultTake = takeMVar var,
			taskResultFail = void . tryPutMVar var . Left }	

runTaskError :: (Show e, Error e, MonadError e m, MonadCatch m, MonadIO m, MonadIO n) => (Task a -> m () -> n ()) -> m a -> n (Task a)
runTaskError f = runTaskTry f . tryT

-- | Run task in separate thread
forkTask :: IO a -> IO (Task a)
forkTask = runTask (void . forkIO)

tryT :: (Show e, Error e, MonadError e m) => m a -> m (Either SomeException a)
tryT act = catchError (liftM Right act) (return . Left . toException . userError . show)
