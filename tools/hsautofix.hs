module Main (
	main
	) where

import Control.Monad (forM_)
import Data.Aeson
import Data.List (nub, sort)
import System.Directory (canonicalizePath)

import HsDev.Tools.AutoFix
import HsDev.Util (toUtf8, liftE, readFileUtf8, writeFileUtf8)

import Tool

main :: IO ()
main = toolMain "hsautofix" [
	jsonCmd "show" [] [] "show what can be auto-fixed" show',
	jsonCmd "fix" [] [] "fix selected errors" fix']
	where
		show' :: Args -> ToolM [Correction]
		show' (Args [] _) = do
			input <- liftE getContents
			msgs <- maybe (toolError "Can't parse messages") return $ decode (toUtf8 input)
			return $ corrections msgs

		fix' :: Args -> ToolM ()
		fix' (Args [] _) = do
			input <- liftE getContents
			corrs <- maybe (toolError "Can't parse messages") return $ decode (toUtf8 input)
			files <- liftE $ mapM canonicalizePath $ nub $ sort $ map correctionFile corrs
			forM_ files $ \f' -> do
				fileCts <- liftE $ readFileUtf8 f'
				liftE $ writeFileUtf8 f' $ autoFix fileCts $ filter ((== f') . correctionFile) corrs
		fix' _ = toolError "Invalid arguments"
