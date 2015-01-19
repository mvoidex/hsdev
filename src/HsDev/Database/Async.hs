module HsDev.Database.Async (
	update, clear, wait,
	module Data.Async
	) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class (MonadIO(..))
import Control.DeepSeq (force)

import Data.Async

import HsDev.Database (Database)

update :: MonadIO m => Async Database -> m Database -> m ()
update db act = do
	db' <- act
	force db' `seq` liftIO (modifyAsync db (Append db'))

clear :: MonadIO m => Async Database -> m Database -> m ()
clear db act = do
	db' <- act
	force db' `seq` liftIO (modifyAsync db (Remove db'))

-- | This function is used to ensure that all previous updates were applied
wait :: MonadIO m => Async Database -> m ()
wait db = liftIO $ do
	waitVar <- newEmptyMVar
	modifyAsync db (Action $ \d -> putMVar waitVar () >> return d)
	takeMVar waitVar
