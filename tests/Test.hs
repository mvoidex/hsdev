{-# LANGUAGE OverloadedStrings #-}

module Main (
	main
	) where

import Control.Lens
import Control.Exception (displayException)
import Data.Aeson hiding (Error)
import Data.Aeson.Lens
import Data.Default
import Data.Maybe
import Data.String (fromString)
import qualified Data.Set as S
import System.FilePath
import System.Directory
import Test.Hspec

import HsDev
import HsDev.Database.SQLite

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

rgn :: Maybe Value -> Maybe Region
rgn v = Region <$> (pos (v ^. key "from")) <*> (pos (v ^. key "to")) where
	pos :: Maybe Value -> Maybe Position
	pos v' = Position <$> (v' ^. key "line") <*> (v' ^. key "column")

mkSet :: [String] -> S.Set String
mkSet = S.fromList

main :: IO ()
main = hspec $ do
	describe "scan project" $ do
		dir <- runIO getCurrentDirectory
		s <- runIO $ startServer_ (def { serverSilent = True })
		it "should load data" $ do
			inserts <- liftM lines $ readFile (dir </> "tests/data/base.sql")
			inServer s $ mapM_ (execute_ . fromString) inserts
		it "should scan project" $ do
			void $ send s ["scan", "--project", dir </> "tests/test-package"]
		it "should resolve export list" $ do
			one <- send s ["module", "ModuleOne", "--exact"]
			exports one `shouldBe` mkSet ["test", "newChan", "untypedFoo"]
			two <- send s ["module", "ModuleTwo", "--exact"]
			exports two `shouldBe` mkSet ["untypedFoo", "twice", "overloadedStrings"]
		it "should pass extensions when checks" $ do
			checks <- send s ["check", "--file", dir </> "tests/test-package/ModuleTwo.hs"]
			(checks ^.. traverseArray . key "level" . _Just) `shouldSatisfy` (("error" :: String) `notElem`)
		it "should return types and docs" $ do
			test' <- send s ["whois", "test", "--file", dir </> "tests/test-package/ModuleOne.hs"]
			let
				testType :: Maybe String
				testType = test' ^. traverseArray . key "info" . key "type"
			testType `shouldBe` Just "IO ()"
		it "should infer types" $ do
			ts <- send s ["types", "--file", dir </> "tests/test-package/ModuleOne.hs"]
			let
				untypedFooRgn = Region (Position 14 1) (Position 14 11)
				exprs :: [(Region, String)]
				exprs = do
					note' <- ts ^.. traverseArray
					rgn' <- maybeToList $ rgn (note' ^. key "region")
					return (rgn', note' ^. key "note" . key "type" . _Just)
			lookup untypedFooRgn exprs `shouldBe` Just "a -> a -> a" -- FIXME: Where's Num constraint?
		it "should return symbol under location" $ do
			_ <- send s ["infer", "--file", dir </> "tests/test-package/ModuleTwo.hs"]
			who' <- send s ["whoat", "13", "15", "--file", dir </> "tests/test-package/ModuleTwo.hs"]
			let
				whoName :: Maybe String
				whoName = who' ^. traverseArray . key "id" . key "name"
				whoType :: Maybe String
				whoType = who' ^. traverseArray . key "info" . key "type"
			whoName `shouldBe` Just "f"
			whoType `shouldBe` Just "a -> a"
		_ <- runIO $ send s ["exit"]
		return ()
