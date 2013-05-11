module Data.Async (
	Event(..),
	event,
	Async(..),
	newAsync, readAsync, modifyAsync,
	subscribe, subscribeEvents
	) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan

import Data.Group

-- | Event on async value
data Event a = Append a | Remove a | Clear | Modify (a -> a) | Action (a -> IO a)

-- | Event to function
event :: Group a => Event a -> a -> IO a
event (Append v) x = return $ add x v
event (Remove v) x = return $ sub x v
event Clear x = return zero
event (Modify p) x = return $ p x
event (Action p) x = p x

-- | Async variable
data Async a = Async {
	asyncVar :: MVar a,
	asyncTrace :: Chan a,
	asyncEvents :: Chan (Event a) }

newAsync :: Group a => IO (Async a)
newAsync = do
	var <- newMVar zero
	trace <- newChan
	events <- newChan
	dupChan events >>= getChanContents >>= forkIO . foldM_ (doUpdate trace) zero
	dupChan trace >>= getChanContents >>= forkIO . mapM_ (swapMVar var)
	return $ Async var trace events
	where
		doUpdate ch x e = do
			x' <- event e x
			writeChan ch x'
			return x'

readAsync :: Async a -> IO a
readAsync = readMVar . asyncVar

modifyAsync :: Async a -> Event a -> IO ()
modifyAsync avar e = writeChan (asyncEvents avar) e

subscribe :: Async a -> (a -> IO ()) -> IO ()
subscribe avar onUpdate = do
	dupChan (asyncTrace avar) >>= getChanContents >>= void . forkIO . mapM_ onUpdate

subscribeEvents :: Async a -> (Event a -> IO ()) -> IO ()
subscribeEvents avar onEvent = do
	dupChan (asyncEvents avar) >>= getChanContents >>= void . forkIO . mapM_ onEvent
