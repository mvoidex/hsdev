{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances, ConstraintKinds, FlexibleContexts, TemplateHaskell #-}

module HsDev.Database.Update.Types (
	Status(..), Progress(..), Task(..),
	UpdateOptions(..), updateTasks, updateGhcOpts, updateDocs, updateInfer,
	UpdateState(..), updateOptions, updateChan, withUpdateState, sendUpdateAction,
	UpdateM(..), UpdateMonad,
	taskName, taskStatus, taskSubjectType, taskSubjectName, taskProgress,

	module HsDev.Server.Types
	) where

import Control.Applicative
import qualified Control.Concurrent.Async as Async
import Control.Lens (makeLenses)
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Trans.Control
import Data.Aeson
import Data.Default
import Text.Format ((~~))
import qualified System.Log.Simple as Log

import Control.Concurrent.Util (sync)
import qualified Control.Concurrent.FiniteChan as F
import HsDev.Database.SQLite
import HsDev.Server.Types hiding (Command(..))
import HsDev.Symbols
import HsDev.Types
import HsDev.Util ((.::))

data Status = StatusWorking | StatusOk | StatusError HsDevError

instance ToJSON Status where
	toJSON StatusWorking = toJSON ("working" :: String)
	toJSON StatusOk = toJSON ("ok" :: String)
	toJSON (StatusError e) = toJSON e

instance FromJSON Status where
	parseJSON v = msum $ map ($ v) [
		withText "status" $ \t -> guard (t == "working") *> return StatusWorking,
		withText "status" $ \t -> guard (t == "ok") *> return StatusOk,
		liftM StatusError . parseJSON,
		fail "invalid status"]

data Progress = Progress {
	progressCurrent :: Int,
	progressTotal :: Int }

instance ToJSON Progress where
	toJSON (Progress c t) = object [
		"current" .= c,
		"total" .= t]

instance FromJSON Progress where
	parseJSON = withObject "progress" $ \v -> Progress <$> (v .:: "current") <*> (v .:: "total")

data Task = Task {
	_taskName :: String,
	_taskStatus :: Status,
	_taskSubjectType :: String,
	_taskSubjectName :: String,
	_taskProgress :: Maybe Progress }

makeLenses ''Task

instance ToJSON Task where
	toJSON t = object [
		"task" .= _taskName t,
		"status" .= _taskStatus t,
		"type" .= _taskSubjectType t,
		"name" .= _taskSubjectName t,
		"progress" .= _taskProgress t]

instance FromJSON Task where
	parseJSON = withObject "task" $ \v -> Task <$>
		(v .:: "task") <*>
		(v .:: "status") <*>
		(v .:: "type") <*>
		(v .:: "name") <*>
		(v .:: "progress")

data UpdateOptions = UpdateOptions {
	_updateTasks :: [Task],
	_updateGhcOpts :: [String],
	_updateDocs :: Bool,
	_updateInfer :: Bool }

instance Default UpdateOptions where
	def = UpdateOptions [] [] False False

makeLenses ''UpdateOptions

data UpdateState = UpdateState {
	_updateOptions :: UpdateOptions,
	_updateChan :: F.Chan (ServerM IO ()) }

makeLenses ''UpdateState

withUpdateState :: SessionMonad m => UpdateOptions -> (UpdateState -> m a) -> m a
withUpdateState uopts fn = bracket (liftIO F.newChan) (liftIO . F.closeChan) $ \ch -> do
	session <- getSession
	th <- liftIO $ Async.async $ withSession session $ Log.component "sqlite" $ transaction_ Immediate $ do
		Log.sendLog Log.Debug "updating sql database"
		cts <- liftIO $ F.readChan ch
		sequence_ $ map (handleAll logErr) cts
		Log.sendLog Log.Debug "sql database updated"
	r <- fn (UpdateState uopts ch)
	liftIO $ liftIO $ F.closeChan ch
	liftIO $ Async.wait th
	return r
	where
		logErr e = Log.sendLog Log.Error ("exception in sql database updater: {}" ~~ displayException e)

type UpdateMonad m = (CommandMonad m, MonadReader UpdateState m, MonadWriter [ModuleLocation] m)

sendUpdateAction :: UpdateMonad m => ServerM IO () -> m ()
sendUpdateAction act = do
	ch <- asks _updateChan
	sync $ \done -> liftIO $ F.putChan ch (act `finally` liftIO done)

newtype UpdateM m a = UpdateM { runUpdateM :: ReaderT UpdateState (WriterT [ModuleLocation] (ClientM m)) a }
	deriving (Applicative, Alternative, Monad, MonadPlus, MonadIO, MonadThrow, MonadCatch, MonadMask, Functor, MonadReader UpdateState, MonadWriter [ModuleLocation])

instance MonadTrans UpdateM where
	lift = UpdateM . lift . lift . lift

instance (MonadIO m, MonadMask m) => Log.MonadLog (UpdateM m) where
	askLog = UpdateM $ lift $ lift Log.askLog
	localLog fn = UpdateM . hoist (hoist (Log.localLog fn)) . runUpdateM

instance ServerMonadBase m => SessionMonad (UpdateM m) where
	getSession = UpdateM $ lift $ lift getSession
	localSession fn = UpdateM . hoist (hoist (localSession fn)) . runUpdateM

instance ServerMonadBase m => CommandMonad (UpdateM m) where
	getOptions = UpdateM $ lift $ lift getOptions

instance MonadBase b m => MonadBase b (UpdateM m) where
	liftBase = UpdateM . liftBase

instance MonadBaseControl b m => MonadBaseControl b (UpdateM m) where
	type StM (UpdateM m) a = StM (ReaderT UpdateState (WriterT [ModuleLocation] (ClientM m))) a
	liftBaseWith f = UpdateM $ liftBaseWith (\f' -> f (f' . runUpdateM))
	restoreM = UpdateM . restoreM
