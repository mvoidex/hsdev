{-# LANGUAGE OverloadedStrings, CPP, LambdaCase, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Server.Commands (
	ServerCommand(..), ServerOpts(..), ClientOpts(..),
	Request(..),
	Msg, isLisp, msg, jsonMsg, lispMsg, encodeMessage, decodeMessage,
	sendCommand, runServerCommand,
	findPath,
	processRequest, processClient, processClientSocket,
	module HsDev.Server.Types
	) where

import Control.Concurrent.Async
import Control.Lens (set, view)
import Control.Monad
import Control.Monad.Catch (bracket, bracket_)
import Data.Aeson hiding (Result, Error)
import qualified Data.Aeson as A
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe
import Network.Socket hiding (connect)
import qualified Network.Socket as Net hiding (send)
import System.Directory
import System.Exit
import System.IO
import qualified System.Log.Simple as Log

import GHC (getSessionDynFlags)

import Text.Format ((~~), (~%))
import Text.Format.Colored (coloredLine)

import HsDev.Server.Base
import HsDev.Server.Types
import HsDev.Error
import HsDev.Util
import HsDev.Version
import HsDev.PackageDb.Types (globalDb)
import HsDev.Tools.Ghc.Worker
import qualified HsDev.Tools.Ghc.System as System

#if mingw32_HOST_OS
import Data.List
import HsDev.Tools.Base (runTool_)
import System.Environment
import System.Win32.PowerShell (escape, quote, quoteDouble)
#else
import Control.Exception (SomeException, handle)
import System.Posix.Process
import System.Posix.IO
#endif

sendCommand :: ClientOpts -> Bool -> Command -> (Notification -> IO a) -> IO Result
sendCommand copts noFile c onNotification = do
	asyncAct <- async sendReceive
	res <- waitCatch asyncAct
	case res of
		Left e -> return $ Error $ OtherError (show e)
		Right r -> return r
	where
		sendReceive = do
			curDir <- getCurrentDirectory
			input <- if clientStdin copts
				then Just <$> L.getContents
				else return $ toUtf8 <$> Nothing -- arg "data" copts
			let
				parseData :: L.ByteString -> IO Value
				parseData cts = case eitherDecode cts of
					Left err -> putStrLn ("Invalid data: " ++ err) >> exitFailure
					Right v -> return v
			_ <- traverse parseData input -- FIXME: Not used!

			s <- makeSocket (clientPort copts)
			addr' <- inet_addr "127.0.0.1"
			Net.connect s (sockAddr (clientPort copts) addr')
			bracket (socketToHandle s ReadWriteMode) hClose $ \h -> do
				L.hPutStrLn h $ encode $ Message Nothing $ Request c curDir noFile (clientTimeout copts) (clientSilent copts)
				hFlush h
				peekResponse h

		peekResponse h = do
			resp <- hGetLineBS h
			parseResponse h resp

		parseResponse h str = case eitherDecode str of
			Left e -> return $ Error $ ResponseError ("can't parse: {}" ~~ e) (fromUtf8 str)
			Right (Message _ r) -> do
				Response r' <- unMmap r
				case r' of
					Left n -> onNotification n >> peekResponse h
					Right res -> return res

runServerCommand :: ServerCommand -> IO ()
runServerCommand (Version showCompiler)
	| showCompiler = do
		w <- Log.noLog ghcWorker
		compVer <- inWorker w $ do
			workerSession SessionGhc globalDb []
			df <- getSessionDynFlags
			return $ System.formatBuildPath "{compiler}-{version}" (System.buildInfo df)
		putStrLn $ $cabalVersion ++ " " ++ compVer
		joinWorker w
	| otherwise = putStrLn $cabalVersion
runServerCommand (Start sopts) = do
#if mingw32_HOST_OS
	let
		args = "run" : serverOptsArgs sopts
	myExe <- getExecutablePath
	curDir <- getCurrentDirectory
	let
		-- one escape for start-process and other for callable process
		-- seems, that start-process just concats arguments into one string
		-- start-process foo 'bar baz' ⇒ foo bar baz -- not expected
		-- start-process foo '"bar baz"' ⇒ foo "bar baz" -- ok
		biescape = escape quote . escape quoteDouble
		script = "try {{ start-process {process} {args} -WindowStyle Hidden -WorkingDirectory {dir} }} catch {{ $_.Exception, $_.InvocationInfo.Line }}"
			~~ ("process" ~% escape quote myExe)
			~~ ("args" ~% intercalate ", " (map biescape args))
			~~ ("dir" ~% escape quote curDir)
	_ <- runTool_ "powershell" [
		"-NoProfile",
		"-Command",
		script]
	putStrLn $ "Server started at port {}" ~~ serverPort sopts
#else
	let
		forkError :: SomeException -> IO ()
		forkError e  = putStrLn $ "Failed to start server: {}" ~~ show e

		proxy :: IO ()
		proxy = do
			_ <- createSession
			_ <- forkProcess serverAction
			exitImmediately ExitSuccess

		serverAction :: IO ()
		serverAction = do
			mapM_ closeFd [stdInput, stdOutput, stdError]
			nullFd <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
			mapM_ (dupTo nullFd) [stdInput, stdOutput, stdError]
			closeFd nullFd
			runServerCommand (Run sopts)

	handle forkError $ do
		_ <- forkProcess proxy
		putStrLn $ "Server started at port {}" ~~ serverPort sopts
#endif
runServerCommand (Run sopts) = runServer sopts $ bracket_ (setupServer sopts) (shutdownServer sopts) $ return ()
runServerCommand (Stop copts) = runServerCommand (Remote copts False Exit)
runServerCommand (Connect copts) = do
	curDir <- getCurrentDirectory
	s <- makeSocket $ clientPort copts
	addr' <- inet_addr "127.0.0.1"
	Net.connect s $ sockAddr (clientPort copts) addr'
	bracket (socketToHandle s ReadWriteMode) hClose $ \h -> forM_ [(1 :: Integer)..] $ \i -> ignoreIO $ do
		input' <- hGetLineBS stdin
		case decodeMsg input' of
			Left em -> L.putStrLn $ encodeMessage $ set msg (Message Nothing $ responseError $ OtherError "invalid command") em
			Right m -> do
				L.hPutStrLn h $ encodeMessage $ set msg (Message (Just $ show i) $ Request (view msg m) curDir True (clientTimeout copts) False) m
				waitResp h
	where
		waitResp h = do
			resp <- hGetLineBS h
			parseResp h resp

		parseResp h str = case decodeMessage str of
			Left em -> putStrLn $ "Can't decode response: {}" ~~ view msg em
			Right m -> do
				Response r' <- unMmap $ view (msg . message) m
				putStrLn $ "{id}: {response}"
					~~ ("id" ~% fromMaybe "_" (view (msg . messageId) m))
					~~ ("response" ~% fromUtf8 (encodeMsg $ set msg (Response r') m))
				case unResponse (view (msg . message) m) of
					Left _ -> waitResp h
					_ -> return ()
runServerCommand (Remote copts noFile c@(Listen _)) = sendCommand copts noFile c printLog >>= noResult where
	printLog :: Notification -> IO ()
	printLog (Notification v) = case fromJSON v of
		A.Error _ -> putStrLn "incorrect notification"
		A.Success m -> coloredLine . Log.text $ m
	noResult :: Result -> IO ()
	noResult _ = return ()
runServerCommand (Remote copts noFile c) = sendCommand copts noFile c printValue >>= printResult where
	printValue :: ToJSON a => a -> IO ()
	printValue = L.putStrLn . encodeValue
	printResult :: Result -> IO ()
	printResult (Result r) = printValue r
	printResult e = printValue e
	encodeValue :: ToJSON a => a -> L.ByteString
	encodeValue = if clientPretty copts then encodePretty else encode
