module Data.LookupTable (
	LookupTable,
	newLookupTable,
	lookupTable, lookupTableM, cacheInTableM,
	hasLookupTable,
	cachedInTable,
	insertTable, insertTableM, storeInTable, storeInTableM
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

-- | Make function caching results in @LookupTable@
cachedInTable :: (Ord k, MonadIO m) => LookupTable k v -> (k -> m v) -> k -> m v
cachedInTable tbl fn key = cacheInTableM tbl key (fn key)

-- | Insert value into table and return it
insertTable :: (Ord k, MonadIO m) => k -> v -> LookupTable k v -> m v
insertTable key value tbl = liftIO $ modifyMVar tbl $ \tbl' -> return (M.insert key value tbl', value)

-- | Insert value into table and return it
insertTableM :: (Ord k, MonadIO m) => k -> m v -> LookupTable k v -> m v
insertTableM key mvalue tbl = do
	value <- mvalue
	insertTable key value tbl

-- | @insertTable@ with flipped args
storeInTable :: (Ord k, MonadIO m) => LookupTable k v -> k -> v -> m v
storeInTable tbl key value = insertTable key value tbl

-- | @insertTable@ with flipped args
storeInTableM :: (Ord k, MonadIO m) => LookupTable k v -> k -> m v -> m v
storeInTableM tbl key mvalue = insertTableM key mvalue tbl
