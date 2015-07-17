module Data.Async (
	Event(..),
	event,
	Async(..),
	newAsync, readAsync, modifyAsync,

	-- * Reexports
	Group, NFData
	) where

import Control.DeepSeq (NFData, force)
import Control.Monad (forM_)
import Control.Concurrent

import Data.Group (Group(..))

-- | Event on async value
data Event a = Append a | Remove a | Clear | Modify (a -> a) | Action (a -> IO a)

-- | Event to function
event :: Group a => Event a -> a -> IO a
event (Append v) x = return $ add x v
event (Remove v) x = return $ sub x v
event Clear _ = return zero
event (Modify p) x = return $ p x
event (Action p) x = p x

data Async a = Async {
	asyncVar :: MVar a,
	asyncEvents :: Chan (Event a) }

newAsync :: (NFData a, Group a) => IO (Async a)
newAsync = do
	var <- newMVar zero
	events <- newChan
	_ <- forkIO $ do
		evs <- getChanContents events
		forM_ evs $ \e -> modifyMVar_ var $ \val -> do
			x' <- event e val
			force x' `seq` return x'
	return $ Async var events

readAsync :: Async a -> IO a
readAsync = readMVar . asyncVar

modifyAsync :: Async a -> Event a -> IO ()
modifyAsync avar = writeChan (asyncEvents avar)
