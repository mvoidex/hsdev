{-# LANGUAGE RankNTypes #-}

module Control.Concurrent.Worker (
	Worker(..),
	startWorker,
	sendTask, pushTask,
	stopWorker, syncTask,
	inWorkerWith, inWorker, inWorker_,

	module Control.Concurrent.Async
	) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Except
import Data.Maybe (isJust)

import Control.Concurrent.FiniteChan
import Control.Concurrent.Async

data Worker m = Worker {
	workerChan :: Chan (Async (), m ()),
	workerWrap :: forall a. m a -> m a,
	workerTask :: MVar (Async ()),
	workerRestart :: IO Bool }

-- | Create new worker
startWorker :: MonadIO m => (m () -> IO ()) -> (m () -> m ()) -> (forall a. m a -> m a) -> IO (Worker m)
startWorker run initialize wrap = do
	ch <- newChan
	taskVar <- newEmptyMVar
	let
		start = async $ do
			run $ initialize processWork
			processSkip
		processWork = whileJust (liftM (fmap snd) $ liftIO $ getChan ch) id
		processSkip = whileJust (liftM (fmap fst) $ getChan ch) cancel
		whileJust :: Monad m => m (Maybe a) -> (a -> m b) -> m  ()
		whileJust v act = v >>= maybe (return ()) (\x -> act x >> whileJust v act)
	start >>= putMVar taskVar
	let
		restart = do
			task <- readMVar taskVar
			stopped <- liftM isJust $ poll task
			when stopped (start >>= void . swapMVar taskVar)
			return stopped
	return $ Worker ch wrap taskVar restart

sendTask :: (MonadCatch m, MonadIO m) => Worker m -> m a -> IO (Async a)
sendTask w act = mfix $ \async' -> do
	var <- newEmptyMVar
	let
		act' = (workerWrap w act >>= liftIO . putMVar var . Right) `catch` (liftIO . putMVar var . Left)
		f = putChan (workerChan w) (void async', void act') >> takeMVar var >>= either (throwM :: SomeException -> IO a) return
	async f

pushTask :: (MonadCatch m, MonadIO m) => Worker m -> m a -> IO (Async a)
pushTask w act = workerRestart w >> sendTask w act

stopWorker :: Worker m -> IO ()
stopWorker = closeChan . workerChan

-- | Send empty task and wait until worker run it
syncTask :: (MonadCatch m, MonadIO m) => Worker m -> IO ()
syncTask w = pushTask w (return ()) >>= void . wait

-- | Run action in worker and wait for result
inWorkerWith :: (MonadIO m, MonadCatch m, MonadIO n) => (SomeException -> n a) -> Worker m -> m a -> n a
inWorkerWith err w act = liftIO (pushTask w act) >>= (liftIO . waitCatch >=> either err return)

-- | Run action in worker and wait for result
inWorker :: (MonadIO m, MonadCatch m) => Worker m -> m a -> IO a
inWorker w act = pushTask w act >>= liftIO . wait

-- | Run action in worker and wait for result
inWorker_ :: (MonadIO m, MonadCatch m) => Worker m -> m a -> ExceptT SomeException IO a
inWorker_ w act = liftIO (pushTask w act) >>= ExceptT . waitCatch
