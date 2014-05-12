{-# LANGUAGE OverloadedStrings, CPP #-}

module Types (
	-- * Server options
	ServerOpts(..), serverOpts, serverOptsToArgs,
	-- * Client options
	ClientOpts(..), clientOpts,
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
import HsDev.Tools.GhcMod (TypedRegion)
import qualified HsDev.Database.Async as DB
import HsDev.Util ((.::), (.::?))

import System.Command
import Update

#if mingw32_HOST_OS
import System.Win32.FileMapping.NamePool (Pool)
#endif

-- | Server options
data ServerOpts = ServerOpts {
	serverPort :: First Int,
	serverTimeout :: First Int,
	serverLog :: First String,
	serverCache :: First FilePath,
	serverLoadCache :: Any,
	serverAsClient :: Any }

instance DefaultConfig ServerOpts where
	defaultConfig = ServerOpts
		(First $ Just 4567)
		(First $ Just 1000)
		(First Nothing)
		(First Nothing)
		mempty
		mempty

instance Monoid ServerOpts where
	mempty = ServerOpts mempty mempty mempty mempty mempty mempty
	l `mappend` r = ServerOpts
		(serverPort l `mappend` serverPort r)
		(serverTimeout l `mappend` serverTimeout r)
		(serverLog l `mappend` serverLog r)
		(serverCache l `mappend` serverCache r)
		(serverLoadCache l `mappend` serverLoadCache r)
		(serverAsClient l `mappend` serverAsClient r)

-- | Server options command opts
serverOpts :: [OptDescr ServerOpts]
serverOpts = [
	Option [] ["port"] (ReqArg (\p -> mempty { serverPort = First (readMaybe p) }) "number") "listen port",
	Option [] ["timeout"] (ReqArg (\t -> mempty { serverTimeout = First (readMaybe t) }) "msec") "query timeout",
	Option ['l'] ["log"] (ReqArg (\l -> mempty { serverLog = First (Just l) }) "file") "log file",
	Option [] ["cache"] (ReqArg (\p -> mempty { serverCache = First (Just p) }) "path") "cache directory",
	Option [] ["load"] (NoArg (mempty { serverLoadCache = Any True })) "force load all data from cache on startup",
	Option ['c'] ["as-client"] (NoArg (mempty { serverAsClient = Any True })) "make server be client and connect to port specified"]

-- | Convert 'ServerOpts' to args
serverOptsToArgs :: ServerOpts -> [String]
serverOptsToArgs sopts = concat [
	arg' "port" show $ serverPort sopts,
	arg' "timeout" show $ serverTimeout sopts,
	arg' "log" id $ serverLog sopts,
	arg' "cache" id $ serverCache sopts,
	if getAny (serverLoadCache sopts) then ["--load"] else [],
	if getAny (serverAsClient sopts) then ["--as-client"] else []]
	where
		arg' :: String -> (a -> String) -> First a -> [String]
		arg' name str = maybe [] (\v -> ["--" ++ name, str v]) . getFirst

-- | Client options
data ClientOpts = ClientOpts {
	clientPort :: First Int,
	clientPretty :: Any,
	clientData :: Any }

instance DefaultConfig ClientOpts where
	defaultConfig = ClientOpts (First $ Just 4567) mempty mempty

instance Monoid ClientOpts where
	mempty = ClientOpts mempty mempty mempty
	l `mappend` r = ClientOpts
		(clientPort l `mappend` clientPort r)
		(clientPretty l `mappend` clientPretty r)
		(clientData l `mappend` clientData r)

-- | Client options command opts
clientOpts :: [OptDescr ClientOpts]
clientOpts = [
	Option [] ["port"] (ReqArg (\p -> mempty { clientPort = First (readMaybe p) }) "number") "connection port",
	Option [] ["pretty"] (NoArg (mempty { clientPretty = Any True })) "pretty json output",
	Option [] ["stdin"] (NoArg (mempty { clientData = Any True })) "pass data to stdin"]

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
		ResultList <$> parseJSON v,
		ResultMap <$> parseJSON v,
		pure $ ResultJSON v,
		ResultString <$> parseJSON v]

data Response =
	ResponseStatus Status |
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
	ResultProcess ((Status -> IO ()) -> IO ())

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
	commandCallName :: [String],
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
callArgs (CommandCall n ps opts) = n ++ ps ++ toArgs opts

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

