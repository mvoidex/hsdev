module Data.LookupTable (
	LookupTable,
	newLookupTable,
	lookupTable, lookupTableM, cacheInTableM,
	hasLookupTable
	) where

import Control.Monad.IO.Class
import Control.Concurrent.MVar
import Data.Map (Map)
import qualified Data.Map as M

-- | k-v table
type LookupTable k v = MVar (Map k v)

newLookupTable :: (Ord k, MonadIO m) => m (LookupTable k v)
newLookupTable = liftIO $ newMVar mempty

-- | Lookup, or insert if not exists
lookupTable :: (Ord k, MonadIO m) => k -> v -> LookupTable k v -> m v
lookupTable key value tbl = liftIO $ modifyMVar tbl $ \tbl' -> case M.lookup key tbl' of
	Just value' -> return (tbl', value')
	Nothing -> return (M.insert key value tbl', value)

-- | Lookup, or insert if not exists
lookupTableM :: (Ord k, MonadIO m) => k -> m v -> LookupTable k v -> m v
lookupTableM key mvalue tbl = do
	mv <- hasLookupTable key tbl
	case mv of
		Just value -> return value
		Nothing -> do
			value <- mvalue
			lookupTable key value tbl

-- | @lookupTableM@ with swapped args
cacheInTableM :: (Ord k, MonadIO m) => LookupTable k v -> k -> m v -> m v
cacheInTableM tbl key mvalue = lookupTableM key mvalue tbl

-- | Just check existable
hasLookupTable :: (Ord k, MonadIO m) => k -> LookupTable k v -> m (Maybe v)
hasLookupTable key tbl = liftIO $ withMVar tbl $ return . M.lookup key
