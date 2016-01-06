{-# LANGUAGE OverloadedStrings #-}

module Tool (
	-- * Tool
	ToolM, toolMain, printExceptT, printResult,

	module Options.Applicative,
	module HsDev.Util
	) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class
import Control.Monad (liftM)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L (putStrLn)
import Options.Applicative
import System.Environment
import System.IO

import HsDev.Tools.Base (ToolM)
import HsDev.Util (cmd, parseArgs)

-- | Run tool with commands
toolMain :: String -> String -> Parser a -> (a -> IO ()) -> IO ()
toolMain name d p act = do
	hSetBuffering stdout LineBuffering
	hSetEncoding stdout utf8
	hSetEncoding stdin utf8
	res <- liftM (parseArgs name (info p (progDesc d))) getArgs
	either putStrLn act res

printExceptT :: ExceptT String IO () -> IO ()
printExceptT act = runExceptT act >>= either putStrLn return

printResult :: (ToJSON a, MonadIO m) => m a -> m ()
printResult act = act >>= liftIO . L.putStrLn . encode
