{-# LANGUAGE OverloadedStrings, CPP #-}

module Types (
	-- * Server/client options
	serverOpts, clientOpts, serverDefCfg, clientDefCfg,
	-- * Messages and results
	ResultValue(..), Response(..),
	CommandResult(..), ok, err, errArgs, details,
	CommandCall(..), callArgs, addCallOpts, removeCallOpts, WithOpts(..),
	CommandOptions(..), CommandAction
	) where

import Control.Applicative
import Control.Monad.Error
import Data.Aeson
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import System.Console.GetOpt
import Text.Read

import HsDev.Database
import HsDev.Project
import HsDev.Symbols
import HsDev.Tools.GhcMod (TypedRegion, OutputMessage)
import qualified HsDev.Database.Async as DB
import HsDev.Util ((.::), (.::?))

import System.Console.Cmd
import Update

#if mingw32_HOST_OS
import System.Win32.FileMapping.NamePool (Pool)
#endif

-- | Server options
serverOpts :: [Opt]
serverOpts = [
	req "port" "number" `desc` "listen port",
	req "timeout" "msec" `desc` "query timeout",
	req "log" "file" `short` ['l'] `desc` "log file",
	req "cache" "path" `desc` "cache directory",
	flag "load" `desc` "force load all data from cache on startup"]

-- | Client options
clientOpts :: [Opt]
clientOpts = [
	req "port" "number" `desc` "connection port",
	flag "pretty" `desc` "pretty json output",
	req "stdin" "data" `desc` "pass data to stdin",
	req "timeout" "msec" `desc` "overwrite timeout duration"]

serverDefCfg :: Opts String
serverDefCfg = mconcat [
	"port" %-- (4567 :: Int),
	"timeout" %-- (1000 :: Int)]

clientDefCfg :: Opts String
clientDefCfg = mconcat ["port" %-- (4567 :: Int)]

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

data Response =
	ResponseStatus Task |
	ResponseMapFile String |
	Response Value

instance ToJSON Response where
	toJSON (ResponseStatus s) = toJSON s
	toJSON (ResponseMapFile s) = object ["file" .= s]
	toJSON (Response v) = v

instance FromJSON Response where
	parseJSON v = foldr1 (<|>) [
		ResponseStatus <$> parseJSON v,
		withObject "response" (\f -> (ResponseMapFile <$> (f .:: "file"))) v,
		pure $ Response v]

data CommandResult =
	ResultOk ResultValue |
	ResultError String (Map String ResultValue) |
	ResultProcess ((Task -> IO ()) -> IO ())

instance Error CommandResult where
	noMsg = ResultError noMsg mempty
	strMsg s = ResultError s mempty

ok :: CommandResult
ok = ResultOk ResultNone

err :: String -> CommandResult
err s = ResultError s M.empty

errArgs :: String -> [(String, ResultValue)] -> CommandResult
errArgs s as = ResultError s (M.fromList as)

-- | Add detailed information to error message
details :: [(String, ResultValue)] -> CommandResult -> CommandResult
details as (ResultError s cs) = ResultError s (M.union (M.fromList as) cs)
details _ r = r

data CommandCall = CommandCall {
	commandCallName :: String,
	commandCallPosArgs :: [String],
	commandCallOpts :: Opts String }

instance ToJSON CommandCall where
	toJSON (CommandCall n ps opts) = object [
		"command" .= n,
		"args" .= ps,
		"opts" .= opts]

instance FromJSON CommandCall where
	parseJSON = withObject "command call" $ \v -> CommandCall <$>
		(v .:: "command") <*>
		(fromMaybe [] <$> (v .::? "args")) <*>
		(fromMaybe mempty <$> (v .::? "opts"))

callArgs :: CommandCall -> [String]
callArgs (CommandCall n ps opts) = words n ++ toArgs (Args ps opts)

-- | Add options
--
-- >cmdCall `addCallOpts` ["foo" %-- 15]
addCallOpts :: CommandCall -> [Opts String] -> CommandCall
addCallOpts cmdCall os = cmdCall {
	commandCallOpts = mconcat (commandCallOpts cmdCall : os) }

-- | Remove specified call options
--
-- >cmdCall `removeCallOpts` ["foo"]
removeCallOpts :: CommandCall -> [String] -> CommandCall
removeCallOpts cmdCall os = cmdCall {
	commandCallOpts = Opts $ foldr (.) id (map M.delete os) $ getOpts (commandCallOpts cmdCall) }

data WithOpts a = WithOpts {
	withOptsAct :: a,
	withOptsCommand :: CommandCall }

instance Functor WithOpts where
	fmap f (WithOpts x as) = WithOpts (f x) as

data CommandOptions = CommandOptions {
	commandDatabase :: DB.Async Database,
	commandWriteCache :: Database -> IO (),
	commandReadCache :: (FilePath -> ErrorT String IO Structured) -> IO (Maybe Database),
	commandRoot :: FilePath,
	commandLog :: String -> IO (),
	commandLogWait :: IO (),
#if mingw32_HOST_OS
	commandMmapPool :: Maybe Pool,
#endif
	commandLink :: IO (),
	commandHold :: IO (),
	commandExit :: IO () }

type CommandAction = WithOpts (Int -> CommandOptions -> IO CommandResult)

