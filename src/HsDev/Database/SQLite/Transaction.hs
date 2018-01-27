{-# LANGUAGE TypeApplications, MultiWayIf, OverloadedStrings #-}

module HsDev.Database.SQLite.Transaction (
	TransactionType(..),
	Retries(..), def, noRetry, retryForever, retryN,

	-- * Transactions
	withTransaction, beginTransaction, commitTransaction, rollbackTransaction,
	transaction, transaction_,

	-- * Retry functions
	retry, retry_
	) where

import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Default
import Database.SQLite.Simple as SQL hiding (withTransaction)

import HsDev.Server.Types (SessionMonad, serverSqlDatabase)

-- | Three types of transactions
data TransactionType = Deferred | Immediate | Exclusive
	deriving (Eq, Ord, Read, Show)

-- | Retry config
data Retries = Retries {
	retriesIntervals :: [Int],
	retriesError :: SQLError -> Bool }

instance Default Retries where
	def = Retries (replicate 10 100000) $ \e -> sqlError e `elem` [ErrorBusy, ErrorLocked]

-- | Don't retry
noRetry :: Retries
noRetry = Retries [] (const False)

-- | Retry forever
retryForever :: Int -> Retries
retryForever interval = def { retriesIntervals = repeat interval }

-- | Retry with interval N times
retryN :: Int -> Int -> Retries
retryN interval times = def { retriesIntervals = replicate times interval }

-- | Run actions inside transaction
withTransaction :: (MonadIO m, MonadMask m) => Connection -> TransactionType -> Retries -> m a -> m a
withTransaction conn t rs act = mask $ \restore -> do
	mretry restore (beginTransaction conn t)
	(restore act <* mretry restore (commitTransaction conn)) `onException` rollbackTransaction conn
	where
		mretry restore' fn = mretry' (retriesIntervals rs) where
			mretry' [] = fn
			mretry' (tm:tms) = catch @_ @SQLError fn $ \e -> if
				| retriesError rs e -> do
						_ <- restore' $ liftIO $ threadDelay tm
						mretry' tms
				| otherwise -> throwM e

-- | Begin transaction
beginTransaction :: MonadIO m => Connection -> TransactionType -> m ()
beginTransaction conn t = liftIO $ SQL.execute_ conn $ case t of
	Deferred -> "begin transaction;"
	Immediate -> "begin immediate transaction;"
	Exclusive -> "begin exclusive transaction;"

-- | Commit transaction
commitTransaction :: MonadIO m => Connection -> m ()
commitTransaction conn = liftIO $ SQL.execute_ conn "commit transaction;"

-- | Rollback transaction
rollbackTransaction :: MonadIO m => Connection -> m ()
rollbackTransaction conn = liftIO $ SQL.execute_ conn "rollback transaction;"

-- | Run transaction in @SessionMonad@
transaction :: SessionMonad m => TransactionType -> Retries -> m a -> m a
transaction t rs act = do
	conn <- serverSqlDatabase
	withTransaction conn t rs act

-- | Transaction with default retries config
transaction_ :: SessionMonad m => TransactionType -> m a -> m a
transaction_ t = transaction t def

-- | Retry operation
retry :: (MonadIO m, MonadCatch m) => Retries -> m a -> m a
retry rs = retry' (retriesIntervals rs) where
	retry' [] fn = fn
	retry' (tm:tms) fn = catch @_ @SQLError fn $ \e -> if
		| retriesError rs e -> do
			liftIO $ threadDelay tm
			retry' tms fn
		| otherwise -> throwM e

-- | Retry with default params
retry_ :: (MonadIO m, MonadCatch m) => m a -> m a
retry_ = retry def
