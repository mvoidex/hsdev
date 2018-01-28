module HsDev.Tools.Base (
	runTool, runTool_,
	Result, ToolM,
	runWait, runWait_,
	tool, tool_,
	matchRx, splitRx, replaceRx,
	at, at_,

	module HsDev.Tools.Types
	) where

import Control.Monad.Except
import Data.Array (assocs)
import Data.List (unfoldr, intercalate)
import Data.Maybe (fromMaybe)
import Data.String
import System.Exit
import System.Process
import Text.Regex.PCRE ((=~), MatchResult(..))

import HsDev.Error
import HsDev.Tools.Types
import HsDev.Util (liftIOErrors)

-- | Run tool, throwing HsDevError on fail
runTool :: FilePath -> [String] -> String -> IO String
runTool name args input = hsdevLiftIOWith onIOError $ do
	(code, out, err) <- readProcessWithExitCode name args input
	case code of
		ExitFailure ecode -> hsdevError $ ToolError name $
			"exited with code " ++ show ecode ++ ": " ++ err
		ExitSuccess -> return out
	where
		onIOError s = ToolError name $ unlines [
			"args: [" ++ intercalate ", " args ++ "]",
			"stdin: " ++ input,
			"error: " ++ s]

-- | Run tool with not stdin
runTool_ :: FilePath -> [String] -> IO String
runTool_ name args = runTool name args ""

type Result = Either String String
type ToolM a = ExceptT String IO a

-- | Run command and wait for result
runWait :: FilePath -> [String] -> String -> IO Result
runWait name args input = do
	(code, out, err) <- readProcessWithExitCode name args input
	return $ if code == ExitSuccess && not (null out) then Right out else Left err

-- | Run command with no input
runWait_ :: FilePath -> [String] -> IO Result
runWait_ name args = runWait name args ""

-- | Tool
tool :: FilePath -> [String] -> String -> ToolM String
tool name args input = liftIOErrors $ ExceptT $ runWait name args input

-- | Tool with no input
tool_ :: FilePath -> [String] -> ToolM String
tool_ name args = tool name args ""

matchRx :: String -> String -> Maybe (Int -> Maybe String)
matchRx pat str = if matched then Just look else Nothing where
	m :: MatchResult String
	m = str =~ pat
	matched = not $ null $ mrMatch m
	groups = filter (not . null . snd) $ assocs $ mrSubs m
	look i = lookup i groups

splitRx :: String -> String -> [String]
splitRx pat = unfoldr split' . Just where
	split' :: Maybe String -> Maybe (String, Maybe String)
	split' Nothing = Nothing
	split' (Just str) = case str =~ pat of
		(pre, "", "") -> Just (pre, Nothing)
		(pre, _, post) -> Just (pre, Just post)

replaceRx :: String -> String -> String -> String
replaceRx pat w = intercalate w . splitRx pat

at :: (Int -> Maybe a) -> Int -> a
at g i = fromMaybe (error $ "Can't find group " ++ show i) $ g i

at_ :: IsString s => (Int -> Maybe s) -> Int -> s
at_ g = fromMaybe (fromString "") . g
