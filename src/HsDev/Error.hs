{-# LANGUAGE FlexibleContexts #-}

module HsDev.Error (
	hsdevError, hsdevOtherError, hsdevLift, hsdevLiftWith, hsdevCatch, hsdevLiftIO, hsdevLiftIOWith, hsdevIgnore,
	hsdevHandle,

	module HsDev.Types
	) where

import Prelude

import Control.Exception (IOException)
import Control.Monad.Catch
import Control.Monad.Except

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

-- | Rethrow IO exceptions as `HsDevError`
hsdevLiftIO :: MonadCatch m => m a -> m a
hsdevLiftIO = hsdevLiftIOWith IOFailed

-- | Rethrow IO exceptions
hsdevLiftIOWith :: MonadCatch m => (String -> HsDevError) -> m a -> m a
hsdevLiftIOWith ctor act = catch act onErr where
	onErr :: MonadThrow m => IOException -> m a
	onErr = hsdevError . ctor . displayException

-- | Ignore hsdev exception
hsdevIgnore :: MonadCatch m => a -> m a -> m a
hsdevIgnore v act = hsdevCatch act >>= either (const $ return v) return

-- | Handle hsdev exception
hsdevHandle :: MonadCatch m => (HsDevError -> m a) -> m a -> m a
hsdevHandle h act = hsdevCatch act >>= either h return
