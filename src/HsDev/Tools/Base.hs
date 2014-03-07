module HsDev.Tools.Base (
	Result, ToolM,
	runWait, runWait_,
	runTool, runTool_,
	match,
	at,
	inspect
	) where

import Control.Monad.Error
import Control.Monad.State
import Data.Maybe (fromMaybe)
import System.Exit
import System.Process
import Text.RegexPR (matchRegexPR)

import HsDev.Symbols
import HsDev.Util (liftIOErrors)

type Result = Either String String
type ToolM a = ErrorT String IO a

-- | Run command and wait for result
runWait :: FilePath -> [String] -> String -> IO Result
runWait name args input = do
	(code, out, err) <- readProcessWithExitCode name args input
	return $ if code == ExitSuccess && not (null out) then Right out else Left err

-- | Run command with no input
runWait_ :: FilePath -> [String] -> IO Result
runWait_ name args = runWait name args ""

-- | Run tool
runTool :: FilePath -> [String] -> String -> ToolM String
runTool name args input = liftIOErrors $ ErrorT $ runWait name args input

-- | Run tool with no input
runTool_ :: FilePath -> [String] -> ToolM String
runTool_ name args = runTool name args ""

match :: String -> String -> Maybe (Int -> Maybe String)
match pat str = do
	(_, groups) <- matchRegexPR pat str
	return $ \i -> lookup i groups

at :: (Int -> Maybe String) -> Int -> String
at g i = fromMaybe (error $ "Can't find group " ++ show i) $ g i

inspect :: Monad m => ModuleLocation -> ErrorT String m Inspection -> ErrorT String m Module -> ErrorT String m InspectedModule
inspect mloc insp act = lift $ execStateT inspect' (Inspected InspectionNone mloc (Left "not inspected")) where
	inspect' = runErrorT $ do
		i <- mapErrorT lift insp
		modify (\im -> im { inspection = i })
		v <- mapErrorT lift act
		modify (\im -> im { inspectionResult = Right v })
		`catchError`
		\e -> modify (\im -> im { inspectionResult = Left e })
