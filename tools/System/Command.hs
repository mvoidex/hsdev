{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module System.Command (
	Command(..),
	Param(..), CommandError(..), CommandAction(..),
	runAction,
	(&&=),
	cmd,
	brief, help,
	parseArgs, parseCmd, parseJson,
	run,

	module System.Args
	) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Error
import Data.Aeson hiding (json, Error)
import Data.Either
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import Data.String
import qualified Data.Text as T (unpack)
import System.Args

-- | Command
data Command a = Command {
	commandName :: String,
	commandId :: a,
	commandDesc :: Maybe String,
	commandArgs :: ArgsSpec }

-- | Parameter
data Param =
	Param String |
	List [String] |
	Dictionary (Map String Param)

instance ToJSON Param where
	toJSON (Param str) = toJSON str
	toJSON (List ps) = toJSON ps
	toJSON (Dictionary d) = toJSON d

-- | Command error
data CommandError = CommandError {
	errorMsg :: String,
	errorParams :: Map String Param }

(&&=) :: CommandError -> [(String, Param)] -> CommandError
e &&= es = e { errorParams = M.fromList es `M.union` errorParams e }

instance Error CommandError where
	noMsg = CommandError noMsg M.empty
	strMsg m = CommandError m M.empty

instance ToJSON CommandError where
	toJSON (CommandError msg params) = object [
		"error" .= msg,
		"params" .= params]

-- | Command action
newtype CommandAction a = CommandAction {
	runCommand :: ReaderT Args (ErrorT CommandError IO) a }
		deriving (Functor, Monad, MonadReader Args, MonadIO, Applicative, MonadError CommandError)

-- | Run CommandAction
runAction :: CommandAction a -> Args -> (CommandError -> IO b) -> (a -> IO b) -> IO b
runAction c as onError onOk = runErrorT (runReaderT (runCommand c) as) >>= either onError onOk

-- | Make command
cmd :: String -> String -> ArgsSpec -> a -> Command a
cmd name d as cmdId = Command name cmdId (Just d) as

-- | Show brief help for command
brief :: Command a -> String
brief c = unwords $ [commandName c, head (usage (commandArgs c))] ++ maybe [] (return . ("-- " ++)) (commandDesc c)

-- | Show help for command
help :: Command a -> [String]
help c = brief c : tail (usage (commandArgs c))

-- | Parse command by args
parseArgs :: [Command a] -> [String] -> Either String (a, Args)
parseArgs _ [] = Left "No command given"
parseArgs cmds (cmdName : cmdArgs) = parse cmds args cmdName cmdArgs

-- | Parse command
parseCmd :: [Command a] -> String -> Either String (a, Args)
parseCmd _ "" = Left "No command given"
parseCmd cmds str = parse cmds args cmdName cmdArgs where
	(cmdName : cmdArgs) = split str

-- | Parse command from JSON
parseJson :: [Command a] -> String -> Either String (a, Args)
parseJson cmds i = eitherDecode (fromString i) >>= parse' where
	parse' (Object v) = do
		nameVal <- maybe (Left "No command given") Right $ HM.lookup (fromString "cmd") v
		cmdName <- case nameVal of
			String str -> Right (T.unpack str)
			_ -> Left "Can't read command name"
		parse cmds json cmdName cmdArgs
		where
			cmdArgs = Object $ HM.delete (fromString "cmd") v
	parse' _ = Left "Invalid value"

-- | Run parsed command
run :: Either String (CommandAction a, Args) -> (CommandError -> IO r) -> (a -> IO r) -> IO r
run (Left e) onError _ = onError $ strMsg e
run (Right (act, as)) onError onOk = runAction act as onError onOk

parse :: [Command a] -> (ArgsSpec -> b -> Either String Args) -> String -> b -> Either String (a, Args)
parse cmds f cmdName cmdArgs = if null oks then err else Right (head oks) where
	cmds' = filter ((== cmdName) . commandName) cmds
	(fails, oks) = partitionEithers $ map parseCmd' cmds'
	err = Left $ unlines (("Can't parse " ++ cmdName ++ ":") : map ('\t':) fails)
	parseCmd' c = liftM ((,) (commandId c)) $ f (commandArgs c) cmdArgs
