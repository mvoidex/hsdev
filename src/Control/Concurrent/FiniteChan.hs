module Control.Concurrent.FiniteChan (
	Chan,
	newChan, openedChan, closedChan, doneChan, sendChan, getChan, closeChan, stopChan
	) where

import Control.Monad (void, when, liftM2)
import Control.Monad.Loops
import Control.Concurrent.STM

-- | 'Chan' is stoppable channel
data Chan a = Chan {
	chanOpened :: TMVar Bool,
	chanQueue :: TQueue a }

-- -- | Create new channel
newChan :: IO (Chan a)
newChan = liftM2 Chan (newTMVarIO True) newTQueueIO

-- | Is channel opened
openedChan :: Chan a -> IO Bool
openedChan = atomically . readTMVar . chanOpened

-- | Is channel closed
closedChan :: Chan a -> IO Bool
closedChan = fmap not . openedChan

-- | Is channel closed and all data consumed
doneChan :: Chan a -> IO Bool
doneChan ch = atomically $ do
	o <- readTMVar $ chanOpened ch
	e <- isEmptyTQueue (chanQueue ch)
	return $ not o && e

-- | Write data to channel if it is open
sendChan :: Chan a -> a -> IO Bool
sendChan ch v = atomically $ do
	o <- readTMVar $ chanOpened ch
	when o $ writeTQueue (chanQueue ch) v
	return o

-- | Get data from channel
getChan :: Chan a -> IO (Maybe a)
getChan ch = atomically $ do
	o <- readTMVar $ chanOpened ch
	if o
		then fmap Just (readTQueue (chanQueue ch))
		else tryReadTQueue (chanQueue ch)

-- | Close channel
closeChan :: Chan a -> IO ()
closeChan ch = atomically $ void $ swapTMVar (chanOpened ch) False

-- | Close channel and read all messages
stopChan :: Chan a -> IO [a]
stopChan ch = do
	closeChan ch
	whileJust (getChan ch) return
