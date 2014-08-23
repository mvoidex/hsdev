module Control.Concurrent.FiniteChan (
	Chan,
	newChan, dupChan, putChan, getChan, readChan, closeChan, stopChan
	) where

import qualified Control.Concurrent.Chan as C
import Data.Maybe

-- | 'Chan' is stoppable channel unline 'Control.Concurrent.Chan'
newtype Chan a = Chan (C.Chan (Maybe a))

-- | Create channel
newChan :: IO (Chan a)
newChan = fmap Chan C.newChan

-- | Duplicate channel
dupChan :: Chan a -> IO (Chan a)
dupChan (Chan ch) = fmap Chan $ C.dupChan ch

-- | Write data to channel
putChan :: Chan a -> a -> IO ()
putChan (Chan ch) = C.writeChan ch . Just

-- | Get data from channel
getChan :: Chan a -> IO (Maybe a)
getChan (Chan ch) = C.readChan ch

-- | Read channel contents
readChan :: Chan a -> IO [a]
readChan (Chan ch) = fmap (catMaybes . takeWhile isJust) $ C.getChanContents ch

-- | Close channel. 'putChan' will still work, but no data will be available on other ending
closeChan :: Chan a -> IO ()
closeChan (Chan ch) = C.writeChan ch Nothing

-- | Stop channel and return all data
stopChan :: Chan a -> IO [a]
stopChan ch = closeChan ch >> readChan ch
