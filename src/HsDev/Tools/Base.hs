module HsDev.Tools.Base (
	Result,
	runWait
	) where

import System.Exit
import System.Process

type Result = Either String String

runWait :: FilePath -> [String] -> String -> IO Result
runWait name args input = do
	(code, out, err) <- readProcessWithExitCode name args input
	return $ if code == ExitSuccess && not (null out) then Right out else Left err
