{-# LANGUAGE FlexibleContexts #-}

module HsDev.Error (
	hsdevError, hsdevOtherError, hsdevLift, hsdevLiftWith, hsdevCatch, hsdevExcept, hsdevLiftIO, hsdevLiftIOWith, hsdevIgnore,
	hsdevHandle, hsdevLog, hsdevOnError,

	module HsDev.Types
	) where

import Prelude hiding (log)

import Control.Exception (IOException)
import Control.Monad.Catch
import Control.Monad.Except
import Data.String (fromString)
import System.Log.Simple (MonadLog, sendLog, Level)

import HsDev.Types

-- | Throw `HsDevError`
hsdevError :: MonadThrow m => HsDevError -> m a
hsdevError = throwM

-- | Throw as `OtherError`
hsdevOtherError :: (Exception e, MonadThrow m) => e -> m a
hsdevOtherError = hsdevError . OtherError . displayException

-- | Throw as `OtherError`
hsdevLift :: MonadThrow m => ExceptT String m a -> m a
hsdevLift = hsdevLiftWith OtherError

-- | Throw as some `HsDevError`
hsdevLiftWith :: MonadThrow m => (String -> HsDevError) -> ExceptT String m a -> m a
hsdevLiftWith ctor act = runExceptT act >>= either (hsdevError . ctor) return

hsdevCatch :: MonadCatch m => m a -> m (Either HsDevError a)
hsdevCatch = try

hsdevExcept :: MonadCatch m => m a -> ExceptT HsDevError m a
hsdevExcept = ExceptT . hsdevCatch

-- | Rethrow IO exceptions as `HsDevError`
hsdevLiftIO :: MonadCatch m => m a -> m a
hsdevLiftIO = hsdevLiftIOWith IOFailed

-- | Rethrow IO exceptions
hsdevLiftIOWith :: MonadCatch m => (String -> HsDevError) -> m a -> m a
hsdevLiftIOWith ctor act = catch act onError where
	onError :: MonadThrow m => IOException -> m a
	onError = hsdevError . ctor . displayException

-- | Ignore hsdev exception
hsdevIgnore :: MonadCatch m => a -> m a -> m a
hsdevIgnore v act = hsdevCatch act >>= either (const $ return v) return

-- | Handle hsdev exception
hsdevHandle :: MonadCatch m => (HsDevError -> m a) -> m a -> m a
hsdevHandle h act = hsdevCatch act >>= either h return

-- | Log hsdev exception and rethrow
hsdevLog :: MonadLog m => Level -> m a -> m a
hsdevLog lev act = hsdevCatch act >>= either logError return where
	logError e = sendLog lev (fromString $ show e) >> hsdevError e

-- | Act on exception and throw again
hsdevOnError :: MonadCatch m => (HsDevError -> m b) -> m a -> m a
hsdevOnError h = hsdevHandle (\e -> h e >> hsdevError e)
