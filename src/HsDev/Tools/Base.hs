module HsDev.Tools.Base (
	Result, ToolM,
	runWait, runWait_,
	tool, tool_,
	matchRx, splitRx, replaceRx,
	at, at_,
	inspect,
	-- * Read parse utils
	ReadM,
	readParse, parseReads, parseRead,

	module HsDev.Tools.Types
	) where

import Control.Lens (set)
import Control.Monad.Except
import Control.Monad.State
import Data.Array (assocs)
import Data.List (unfoldr, intercalate)
import Data.Maybe (fromMaybe, listToMaybe)
import System.Exit
import System.Process
import Text.Regex.PCRE ((=~), MatchResult(..))

import HsDev.Tools.Types
import HsDev.Symbols
import HsDev.Util (liftIOErrors)

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

at :: (Int -> Maybe String) -> Int -> String
at g i = fromMaybe (error $ "Can't find group " ++ show i) $ g i

at_ :: (Int -> Maybe String) -> Int -> String
at_ g = fromMaybe "" . g

inspect :: Monad m => ModuleLocation -> ExceptT String m Inspection -> ExceptT String m Module -> ExceptT String m InspectedModule
inspect mloc insp act = lift $ execStateT inspect' (notInspected mloc) where
	inspect' = runExceptT $ do
		i <- mapExceptT lift insp
		modify (set inspection i)
		v <- mapExceptT lift act
		modify (set inspectionResult (Right v))
		`catchError`
		\e -> modify (set inspectionResult (Left e))

type ReadM a = StateT String [] a

-- | Parse readable value
readParse :: Read a => ReadM a
readParse = StateT reads

-- | Run parser
parseReads :: String -> ReadM a -> [a]
parseReads = flip evalStateT

-- | Run parser and select first result
parseRead :: String -> ReadM a -> Maybe a
parseRead s = listToMaybe . parseReads s
