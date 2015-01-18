module Main (
	main
	) where

import Data.Aeson
import System.Directory (canonicalizePath)
import System.IO

import Data.Mark
import HsDev.Tools.AutoFix
import HsDev.Util (toUtf8, liftE)

import Tool

main :: IO ()
main = toolMain "hsautofix" [
	jsonCmd "show" [] [] "show what can be auto-fixed" show',
	jsonCmd "fix" ["file"] [] "fix selected errors" fix']
	where
		show' :: Args -> ToolM [Mark Correction]
		show' (Args [] _) = do
			input <- liftE getContents
			msgs <- maybe (toolError "Can't parse messages") return $ decode (toUtf8 input)
			return $ corrections msgs

		fix' :: Args -> ToolM ()
		fix' (Args [file] _) = do
			input <- liftE getContents
			corrs <- maybe (toolError "Can't parse messages") return $ decode (toUtf8 input)
			f' <- liftE $ canonicalizePath file
			fileCts <- liftE $ withFile f' ReadMode $ \h -> do
				hSetEncoding h utf8
				cts <- hGetContents h
				length cts `seq` return cts
			let
				fileCts' = autoFix fileCts (mapM_ correct corrs)
			liftE $ withFile f' WriteMode $ \h -> do
				hSetEncoding h utf8
				hPutStrLn h fileCts'
		fix' _ = toolError "Invalid arguments"
