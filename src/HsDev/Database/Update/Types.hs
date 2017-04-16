{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances, ConstraintKinds, FlexibleContexts, TemplateHaskell #-}

module HsDev.Database.Update.Types (
	Status(..), Progress(..), Task(..), UpdateOptions(..), UpdateM(..), UpdateMonad,
	taskName, taskStatus, taskSubjectType, taskSubjectName, taskProgress, updateTasks, updateGhcOpts, updateDocs, updateInfer,

	module HsDev.Server.Types
	) where

import Control.Applicative
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
import qualified System.Log.Simple as Log

import HsDev.Server.Types (ServerMonadBase, Session(..), CommandOptions(..), SessionMonad(..), askSession, CommandMonad(..), ClientM(..))
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

type UpdateMonad m = (CommandMonad m, MonadReader UpdateOptions m, MonadWriter [ModuleLocation] m)

newtype UpdateM m a = UpdateM { runUpdateM :: ReaderT UpdateOptions (WriterT [ModuleLocation] (ClientM m)) a }
	deriving (Applicative, Alternative, Monad, MonadPlus, MonadIO, MonadThrow, MonadCatch, MonadMask, Functor, MonadReader UpdateOptions, MonadWriter [ModuleLocation])

instance MonadTrans UpdateM where
	lift = UpdateM . lift . lift . lift

instance (MonadIO m, MonadMask m) => Log.MonadLog (UpdateM m) where
	askLog = UpdateM $ lift $ lift Log.askLog
	localLog fn = UpdateM . hoist (hoist (Log.localLog fn)) . runUpdateM

instance ServerMonadBase m => SessionMonad (UpdateM m) where
	getSession = UpdateM $ lift $ lift getSession

instance ServerMonadBase m => CommandMonad (UpdateM m) where
	getOptions = UpdateM $ lift $ lift getOptions

instance MonadBase b m => MonadBase b (UpdateM m) where
	liftBase = UpdateM . liftBase

instance MonadBaseControl b m => MonadBaseControl b (UpdateM m) where
	type StM (UpdateM m) a = StM (ReaderT UpdateOptions (WriterT [ModuleLocation] (ClientM m))) a
	liftBaseWith f = UpdateM $ liftBaseWith (\f' -> f (f' . runUpdateM))
	restoreM = UpdateM . restoreM
