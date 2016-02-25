module Control.Concurrent.FiniteChan (
	Chan,
	newChan, dupChan, openedChan, closedChan, doneChan, sendChan, putChan, getChan, readChan, closeChan, stopChan
	) where

import Control.Monad (void, when, liftM2)
import qualified Control.Concurrent.Chan as C
import Control.Concurrent.MVar
import Data.Maybe

data State = Opened | Closing | Closed deriving (Eq, Ord, Enum, Bounded, Read, Show)

-- | 'Chan' is stoppable channel unline 'Control.Concurrent.Chan'
data Chan a = Chan (C.Chan (Maybe a)) (MVar State)

-- | Create channel
newChan :: IO (Chan a)
newChan = liftM2 Chan C.newChan (newMVar Opened)

-- | Duplicate channel
dupChan :: Chan a -> IO (Chan a)
dupChan (Chan ch st) = liftM2 Chan (C.dupChan ch) (return st)

-- | Is channel opened
openedChan :: Chan a -> IO Bool
openedChan (Chan _ st) = (== Opened) <$> readMVar st

-- | Is channel closing/closed
closedChan :: Chan a -> IO Bool
closedChan (Chan _ st) = (/= Opened) <$> readMVar st

-- | Is channed closed and all data allready read from it
doneChan :: Chan a -> IO Bool
doneChan (Chan _ st) = (== Closed) <$> readMVar st

-- | Write data to channel
sendChan :: Chan a -> a -> IO Bool
sendChan (Chan ch st) v = do
	state <- readMVar st
	if state == Opened
		then do
			C.writeChan ch (Just v)
			return True
		else return False

-- | Put data to channel
putChan :: Chan a -> a -> IO ()
putChan ch = void . sendChan ch

-- | Get data from channel
getChan :: Chan a -> IO (Maybe a)
getChan (Chan ch st) = do
	state <- readMVar st
	if state == Closed then return Nothing else do
		r <- C.readChan ch
		when (isNothing r) (void $ swapMVar st Closed)
		return r

-- | Read channel contents
readChan :: Chan a -> IO [a]
readChan (Chan ch _) = (catMaybes . takeWhile isJust) <$> C.getChanContents ch

-- | Close channel. 'putChan' will still work, but no data will be available on other ending
closeChan :: Chan a -> IO ()
closeChan (Chan ch st) = do
	state <- readMVar st
	when (state == Opened) $ do
		_ <- swapMVar st Closing
		C.writeChan ch Nothing

-- | Stop channel and return all data
stopChan :: Chan a -> IO [a]
stopChan ch = closeChan ch >> readChan ch
