{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Database.Update.Types (
	Status(..), Progress(..), Task(..), Settings(..), settings, UpdateDB(..)
	) where

import Control.Monad.Catch
import Control.Monad.CatchIO
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Aeson
import qualified System.Log.Simple as Log

import Control.Concurrent.Worker (Worker)
import HsDev.Database
import HsDev.Database.Async hiding (Event)
import HsDev.Tools.GhcMod (WorkerMap)
import HsDev.Server.Types (CommandOptions(..))
import HsDev.Server.Message (Notification(..))
import HsDev.Symbols
import HsDev.Util ((.::))
import HsDev.Watcher.Types
import System.Console.Args

data Status = StatusWorking | StatusOk | StatusError String

instance ToJSON Status where
	toJSON StatusWorking = toJSON ("working" :: String)
	toJSON StatusOk = toJSON ("ok" :: String)
	toJSON (StatusError e) = toJSON $ object ["error" .= e]

instance FromJSON Status where
	parseJSON v = msum $ map ($ v) [
		withText "status" $ \t -> guard (t == "working") *> return StatusWorking,
		withText "status" $ \t -> guard (t == "ok") *> return StatusOk,
		withObject "status" $ \obj -> StatusError <$> (obj .:: "error"),
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
	taskName :: String,
	taskStatus :: Status,
	taskParams :: Object,
	taskProgress :: Maybe Progress,
	taskChild :: Maybe Task }

instance ToJSON Task where
	toJSON t = object [
		"status" .= taskStatus t,
		"task" .= taskName t,
		"params" .= taskParams t,
		"progress" .= taskProgress t,
		"child" .= taskChild t]

instance FromJSON Task where
	parseJSON = withObject "task" $ \v -> Task <$>
		(v .:: "task") <*>
		(v .:: "status") <*>
		(v .:: "params") <*>
		(v .:: "progress") <*>
		(v .:: "child")

data Settings = Settings {
	database :: Async Database,
	databaseCacheReader :: (FilePath -> ExceptT String IO Structured) -> IO (Maybe Database),
	databaseCacheWriter :: Database -> IO (),
	onStatus :: Task -> IO (),
	ghcOptions :: [String],
	updateDocs :: Bool,
	runInferTypes :: Bool,
	settingsGhcModWorker :: Worker (ReaderT WorkerMap IO),
	settingsLogger :: Log.Log,
	settingsWatcher :: Watcher }

settings :: CommandOptions -> Opts String -> Settings
settings copts as = Settings
	(commandDatabase copts)
	(commandReadCache copts)
	(commandWriteCache copts)
	(commandNotify copts . Notification . toJSON)
	(listArg "ghc" as)
	(flagSet "docs" as)
	(flagSet "infer" as)
	(commandGhcMod copts)
	(commandLogger copts)
	(commandWatcher copts)

newtype UpdateDB m a = UpdateDB { runUpdateDB :: ReaderT Settings (WriterT [ModuleLocation] m) a }
	deriving (Applicative, Monad, MonadIO, MonadCatchIO, MonadThrow, MonadCatch, Functor, MonadReader Settings, MonadWriter [ModuleLocation])

instance MonadCatchIO m => MonadCatchIO (ExceptT e m) where
	catch act onError = ExceptT $ Control.Monad.CatchIO.catch (runExceptT act) (runExceptT . onError)
	block = ExceptT . block . runExceptT
	unblock = ExceptT . unblock . runExceptT

instance MonadCatchIO m => Log.MonadLog (UpdateDB m) where
	askLog = liftM settingsLogger ask

instance Log.MonadLog m => Log.MonadLog (ExceptT e m) where
	askLog = lift Log.askLog
