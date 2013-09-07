module HsDev.Database.Async (
	update,
	module Data.Async
	) where

import Control.Monad.IO.Class
import Control.DeepSeq (force)

import Data.Async

import HsDev.Database

update :: MonadIO m => Async Database -> m Database -> m ()
update db act = do
	db' <- act
	force db' `seq` (liftIO $ modifyAsync db (Append db'))
