{-# LANGUAGE OverloadedStrings #-}

module Main (
	main
	) where

import Control.Lens
import Control.Exception (displayException)
import Data.Aeson hiding (Error)
import Data.Aeson.Lens
import Data.Default
import Data.List
import Data.Maybe
import Data.String (fromString)
import qualified Data.Set as S
import Data.Text (unpack)
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
rgn v = Region <$> (pos (v ^. key "from")) <*> (pos (v ^. key "to"))

pos :: Maybe Value -> Maybe Position
pos v = Position <$> (v ^. key "line") <*> (v ^. key "column")

mkSet :: [String] -> S.Set String
mkSet = S.fromList

main :: IO ()
main = hspec $ do
	describe "scan project" $ do
		dir <- runIO getCurrentDirectory
		s <- runIO $ startServer_ (def { serverSilent = True })
		it "should load data" $ do
			inserts <- fmap lines $ readFile (dir </> "tests/data/base.sql")
			inServer s $ mapM_ (execute_ . fromString) inserts
		it "should scan project" $ do
			void $ send s ["scan", "--project", dir </> "tests/test-package"]
		it "should resolve export list" $ do
			one <- send s ["module", "ModuleOne", "--exact"]
			exports one `shouldBe` mkSet ["Ctor", "ctor", "test", "newChan", "untypedFoo"]
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
				untypedFooRgn = Region (Position 15 1) (Position 15 11)
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
		it "should distinguish between constructor and data" $ do
			whoData <- send s ["whoat", "20", "9", "--file", dir </> "tests/test-package/ModuleOne.hs"]
			whoCtor <- send s ["whoat", "21", "8", "--file", dir </> "tests/test-package/ModuleOne.hs"]
			(whoData ^. traverseArray . key "info" . key "what" :: Maybe String) `shouldBe` Just "data"
			(whoCtor ^. traverseArray . key "info" . key "what" :: Maybe String) `shouldBe` Just "ctor"
		it "should use modified file contents" $ do
			modifiedContents <- fmap unpack $ readFileUtf8 $ dir </> "tests/data/ModuleTwo.modified.hs"
			void $ send s ["set-file-contents", "--file", dir </> "tests/test-package/ModuleTwo.hs", "--contents", modifiedContents]
			-- Call scan to wait until file updated
			void $ send s ["scan", "--file", dir </> "tests/test-package/ModuleTwo.hs"]
		it "should rescan module with modified contents" $ do
			two <- send s ["module", "ModuleTwo", "--exact"]
			exports two `shouldBe` mkSet ["untypedFoo", "twice", "overloadedStrings", "useUntypedFoo"]
		it "should use modified source in `check` command" $ do
			checks <- send s ["check", "--file", dir </> "tests/test-package/ModuleTwo.hs"]
			(checks ^.. traverseArray . key "note" . key "message" . _Just) `shouldSatisfy` (any ("Defined but not used" `isPrefixOf`))
		it "should get usages of symbol" $ do
			-- Note, that source was modified
			us <- send s ["usages", "2", "2", "--file", dir </> "tests/test-package/ModuleTwo.hs"]
			let
				locs :: [(String, Position)]
				locs = do
					n <- us ^.. traverseArray
					nm <- maybeToList $ n ^. key "in" . key "name"
					p <- maybeToList $ pos (n ^. key "at")
					return (nm, p)
			S.fromList locs `shouldBe` S.fromList [
				("ModuleOne", Position 4 2),
				("ModuleOne", Position 15 1),
				("ModuleTwo", Position 2 2),
				("ModuleTwo", Position 8 19),
				("ModuleTwo", Position 25 21),
				("ModuleTwo", Position 25 35)]
		it "should get usages of local symbols" $ do
			us <- send s ["usages", "14", "15", "--file", dir </> "tests/test-package/ModuleTwo.hs"]
			let
				locs :: [Position]
				locs = do
					n <- us ^.. traverseArray
					maybeToList $ pos (n ^. key "at")
			S.fromList locs `shouldBe` S.fromList [Position 14 7, Position 14 11, Position 14 15]
		_ <- runIO $ send s ["exit"]
		return ()
