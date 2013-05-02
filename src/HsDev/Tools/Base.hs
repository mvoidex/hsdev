module HsDev.Tools.Base (
	Result,
	runWait,
	match,
	at
	) where

import Data.Maybe
import System.Exit
import System.Process
import Text.RegexPR

type Result = Either String String

-- | Run command and wait for result
runWait :: FilePath -> [String] -> String -> IO Result
runWait name args input = do
	(code, out, err) <- readProcessWithExitCode name args input
	return $ if code == ExitSuccess && not (null out) then Right out else Left err

match :: String -> String -> Maybe (Int -> Maybe String)
match pat str = do
	(_, groups) <- matchRegexPR pat str
	return $ \i -> lookup i groups

at :: (Int -> Maybe String) -> Int -> String
at g i = fromMaybe (error $ "Can't find group " ++ show i) $ g i
