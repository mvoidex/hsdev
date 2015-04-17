{-# LANGUAGE OverloadedStrings, CPP, TypeSynonymInstances, FlexibleInstances #-}

module HsDev.Server.Types (
	CommandOptions(..), CommandError(..), commandError_, commandError,
	CommandAction, CommandM, CommandActionT,
	ResultValue(..)
	) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson hiding (Result, Error)
import Data.Aeson.Types (Pair)
import qualified Data.HashMap.Strict as HM (null)
import Data.Map (Map)
import System.Log.Simple

import HsDev.Database
import qualified HsDev.Database.Async as DB
import HsDev.Project
import HsDev.Symbols
import HsDev.Server.Message
import HsDev.Tools.GhcMod (OutputMessage, TypedRegion, WorkerMap)
import HsDev.Tools.Ghc.Worker (Worker, Ghc)

#if mingw32_HOST_OS
import System.Win32.FileMapping.NamePool (Pool)
#endif

data CommandOptions = CommandOptions {
	commandDatabase :: DB.Async Database,
	commandWriteCache :: Database -> IO (),
	commandReadCache :: (FilePath -> ExceptT String IO Structured) -> IO (Maybe Database),
	commandRoot :: FilePath,
	commandLog :: Level -> String -> IO (),
	commandLogger :: Log,
	commandListenLog :: ([String] -> IO ()) -> IO (),
	commandLogWait :: IO (),
#if mingw32_HOST_OS
	commandMmapPool :: Maybe Pool,
#endif
	commandGhc :: Worker Ghc,
	commandGhcMod :: Worker (ReaderT WorkerMap IO),
	commandNotify :: Notification -> IO (),
	commandLink :: IO (),
	commandHold :: IO (),
	commandExit :: IO () }

data CommandError = CommandError String [Pair]

instance Monoid CommandError where
	mempty = CommandError "" []
	mappend (CommandError lmsg lp) (CommandError rmsg rp) = CommandError (lmsg ++ ", " ++ rmsg) (lp ++ rp)

commandError_ :: String -> ExceptT CommandError IO a
commandError_ m = commandError m []

commandError :: String -> [Pair] -> ExceptT CommandError IO a
commandError m ps = throwError $ CommandError m ps

type CommandAction = CommandOptions -> IO Result

type CommandM a = ExceptT CommandError IO a

type CommandActionT a = CommandOptions -> CommandM a

data ResultValue =
	ResultDatabase Database |
	ResultDeclaration Declaration |
	ResultModuleDeclaration ModuleDeclaration |
	ResultModuleId ModuleId |
	ResultModule Module |
	ResultInspectedModule InspectedModule |
	ResultPackage ModulePackage |
	ResultProject Project |
	ResultTyped TypedRegion |
	ResultOutputMessage OutputMessage |
	ResultList [ResultValue] |
	ResultMap (Map String ResultValue) |
	ResultJSON Value |
	ResultString String |
	ResultNone

instance ToJSON ResultValue where
	toJSON (ResultDatabase db) = toJSON db
	toJSON (ResultDeclaration d) = toJSON d
	toJSON (ResultModuleDeclaration md) = toJSON md
	toJSON (ResultModuleId mid) = toJSON mid
	toJSON (ResultModule m) = toJSON m
	toJSON (ResultInspectedModule m) = toJSON m
	toJSON (ResultPackage p) = toJSON p
	toJSON (ResultProject p) = toJSON p
	toJSON (ResultTyped t) = toJSON t
	toJSON (ResultOutputMessage e) = toJSON e
	toJSON (ResultList l) = toJSON l
	toJSON (ResultMap m) = toJSON m
	toJSON (ResultJSON v) = toJSON v
	toJSON (ResultString s) = toJSON s
	toJSON ResultNone = toJSON $ object []

instance FromJSON ResultValue where
	parseJSON v = foldr1 (<|>) [
		do
			(Object m) <- parseJSON v
			if HM.null m then return ResultNone else mzero,
		ResultDatabase <$> parseJSON v,
		ResultDeclaration <$> parseJSON v,
		ResultModuleDeclaration <$> parseJSON v,
		ResultModuleId <$> parseJSON v,
		ResultModule <$> parseJSON v,
		ResultInspectedModule <$> parseJSON v,
		ResultPackage <$> parseJSON v,
		ResultProject <$> parseJSON v,
		ResultTyped <$> parseJSON v,
		ResultOutputMessage <$> parseJSON v,
		ResultList <$> parseJSON v,
		ResultMap <$> parseJSON v,
		pure $ ResultJSON v,
		ResultString <$> parseJSON v]
