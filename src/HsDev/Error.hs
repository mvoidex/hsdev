module HsDev.Error (
	hsdevError, hsdevLift, hsdevLiftWith, hsdevCatch, hsdevExcept, hsdevLiftIO, hsdevLiftIOWith, hsdevIgnore,

	module HsDev.Types
	) where

import Control.Exception (IOException)
import Control.Monad.Catch
import Control.Monad.Except

import HsDev.Types

hsdevError :: MonadThrow m => HsDevError -> m a
hsdevError = throwM

hsdevLift :: MonadThrow m => ExceptT String m a -> m a
hsdevLift = hsdevLiftWith StringError

hsdevLiftWith :: MonadThrow m => (String -> HsDevError) -> ExceptT String m a -> m a
hsdevLiftWith ctor act = runExceptT act >>= either (hsdevError . ctor) return

hsdevCatch :: MonadCatch m => m a -> m (Either HsDevError a)
hsdevCatch = try

hsdevExcept :: MonadCatch m => m a -> ExceptT HsDevError m a
hsdevExcept = ExceptT . hsdevCatch

hsdevLiftIO :: MonadCatch m => m a -> m a
hsdevLiftIO = hsdevLiftIOWith IOFailed

hsdevLiftIOWith :: MonadCatch m => (String -> HsDevError) -> m a -> m a
hsdevLiftIOWith ctor act = catch act onError where
	onError :: MonadThrow m => IOException -> m a
	onError = hsdevError . ctor . displayException

hsdevIgnore :: MonadCatch m => a -> m a -> m a
hsdevIgnore v act = hsdevCatch act >>= either (const $ return v) return
