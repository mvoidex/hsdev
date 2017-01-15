{-# LANGUAGE OverloadedStrings #-}

module Main (
	main
	) where

import Control.Lens
import Control.Exception (displayException)
import Data.Aeson hiding (Error)
import Data.Aeson.Lens
import Data.Default
import qualified Data.Set as S
import HsDev
import Test.Hspec
import System.FilePath
import System.Directory

send :: Server -> [String] -> IO (Maybe Value)
send srv args = do
	r <- sendServer_ srv args
	case r of
		Result v -> return $ Just v
		Error e -> do
			expectationFailure $ "command result error: " ++ displayException e
			return Nothing

exports :: Maybe Value -> S.Set String
exports v = mkSet (v ^.. traverseArray . key "exports" . traverseArray . key "id" . key "name" . _Just)

mkSet :: [String] -> S.Set String
mkSet = S.fromList

main :: IO ()
main = hspec $ do
	describe "scan project" $ do
		dir <- runIO getCurrentDirectory
		s <- runIO $ startServer (def { serverSilent = True })
		it "should load data" $ do
			void $ send s ["add", "--file", dir </> "tests/data/modules.json"]
		it "should scan project" $ do
			void $ send s ["scan", "--project", dir </> "tests/test-package"]
		it "should resolve export list" $ do
			one <- send s ["module", "ModuleOne", "--exact"]
			when (mkSet ["test", "newChan", "untypedFoo"] /= exports one) $
				expectationFailure "invalid exports of ModuleOne.hs"
			two <- send s ["module", "ModuleTwo", "--exact"]
			when (mkSet ["untypedFoo", "twice", "overloadedStrings"] /= exports two) $
				expectationFailure "invalid exports of ModuleTwo.hs"
		it "should pass extensions when checkings" $ do
			checks <- send s ["check", "--file", dir </> "tests/test-package/ModuleTwo.hs"]
			when (("error" :: String) `elem` (checks ^.. traverseArray . key "level" . _Just)) $
				expectationFailure "there should be no errors, only warnings"
		it "should return types and docs" $ do
			test' <- send s ["whois", "test", "--file", dir </> "tests/test-package/ModuleOne.hs"]
			let
				testType :: Maybe String
				testType = test' ^. traverseArray . key "info" . key "type"
			when (testType /= Just "IO ()") $
				expectationFailure "function type empty"
		it "should infer types" $ do
			ts <- send s ["types", "--file", dir </> "tests/test-package/ModuleOne.hs"]
			let
				exprs :: [(String, String)]
				exprs = do
					note' <- ts ^.. traverseArray . key "note"
					return (note' ^. key "expr" . _Just, note' ^. key "type" . _Just)
			when (lookup "untypedFoo x y = x + y" exprs /= Just "a -> a -> a") $ -- FIXME: Where's Num constraint?
				expectationFailure "invalid type of 'untypedFoo'"
		_ <- runIO $ send s ["exit"]
		return ()
