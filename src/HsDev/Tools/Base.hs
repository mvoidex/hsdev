module HsDev.Tools.Base (
	Result,
	runWait,
	match,
	at,
	inspect
	) where

import Control.Monad.Error
import Control.Monad.State
import Data.Maybe
import System.Exit
import System.Process
import Text.RegexPR

import HsDev.Symbols

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

inspect :: Monad m => ModuleLocation -> ErrorT String m Inspection -> ErrorT String m Module -> ErrorT String m InspectedModule
inspect mloc insp act = liftResult $ execStateT inspect' (Inspected InspectionNone mloc (Left "not inspected")) where
	inspect' = runErrorT $ do
		i <- mapErrorT lift insp
		modify (\im -> im { inspection = i })
		v <- mapErrorT lift act
		modify (\im -> im { inspectionResult = Right v })
		`catchError`
		\e -> modify (\im -> im { inspectionResult = Left e })
	liftResult im = do
		im' <- lift im
		ErrorT $ return $ inspectionResult im' >> return im'
