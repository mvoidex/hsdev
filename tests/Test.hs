{-# LANGUAGE OverloadedStrings, TypeApplications #-}

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
import Data.Text (Text, unpack)
import qualified Data.Text as T
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

exports :: Maybe Value -> S.Set Text
exports v = mkSet (v ^.. _Just . values . key "exports" . values . key "id" . key "name" . _String)

imports :: Maybe Value -> S.Set Import
imports v = mkSet $ do
	is <- v ^.. _Just . values . key "imports" . values
	maybeToList $ Import <$> (is ^? key "pos" . _JSON) <*> (is ^? key "name" . _String) <*> (is ^? key "qualified" . _Bool) <*> pure (is ^? key "as" . _String)

mkSet :: Ord a => [a] -> S.Set a
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
			void $ send s ["scan", "project", dir </> "tests/test-package", "--cabal", "--no-deps"]

		it "should resolve export list" $ do
			one <- send s ["module", "ModuleOne", "--exact"]
			exports one `shouldBe` mkSet ["Ctor", "ctor", "test", "newChan", "untypedFoo"]
			two <- send s ["module", "ModuleTwo", "--exact"]
			exports two `shouldBe` mkSet ["untypedFoo", "twice", "overloadedStrings"]

		it "should return cached warnings on sequential checks without file modifications" $ do
			firstCheck <- send s ["check", "--file", dir </> "tests/test-package/ModuleOne.hs"]
			secondCheck <- send s ["check", "--file", dir </> "tests/test-package/ModuleOne.hs"]
			firstCheck `shouldNotSatisfy` null
			firstCheck `shouldBe` secondCheck

		it "should not lose warnings that comes while loading module for inferring types" $ do
			void $ send s ["infer", "--file", dir </> "tests/test-package/ModuleThree.hs"]
			checks <- send s ["check", "--file", dir </> "tests/test-package/ModuleThree.hs"]
			checks `shouldNotSatisfy` null

		it "should return module imports" $ do
			three <- send s ["module", "ModuleThree", "--exact"]
			imports three `shouldBe` mkSet [
				Import (Position 8 1) "Control.Concurrent.Chan" False Nothing,
				Import (Position 10 1) "ModuleOne" False Nothing,
				Import (Position 11 1) "ModuleTwo" False (Just "T")]

		it "should pass extensions when checks" $ do
			checks <- send s ["check", "--file", dir </> "tests/test-package/ModuleTwo.hs"]
			(checks ^.. _Just . values . key "level" . _String) `shouldSatisfy` (("error" :: Text) `notElem`)

		it "should return types and docs" $ do
			test' <- send s ["whois", "test", "--file", dir </> "tests/test-package/ModuleOne.hs"]
			let
				testType :: Maybe Text
				testType = test' ^? _Just . values . key "info" . key "type" . _String
			testType `shouldBe` Just "IO ()"

		it "should infer types" $ do
			ts <- send s ["types", "--file", dir </> "tests/test-package/ModuleOne.hs"]
			let
				untypedFooRgn = Region (Position 15 1) (Position 15 11)
				exprs :: [(Region, Text)]
				exprs = do
					note' <- ts ^.. _Just . values
					rgn' <- maybeToList $ note' ^? key "region" . _JSON
					ty' <- maybeToList $ note' ^? key "note" . key "type" . _String
					return (rgn', ty')
			lookup untypedFooRgn exprs `shouldBe` Just "a -> a -> a" -- FIXME: Where's Num constraint?

		it "should return symbol under location" $ do
			_ <- send s ["infer", "--file", dir </> "tests/test-package/ModuleTwo.hs"]
			who' <- send s ["whoat", "13", "15", "--file", dir </> "tests/test-package/ModuleTwo.hs"]
			let
				whoName :: Maybe Text
				whoName = who' ^? _Just . values . key "id" . key "name" . _String
				whoType :: Maybe Text
				whoType = who' ^? _Just . values . key "info" . key "type" . _String
			whoName `shouldBe` Just "f"
			whoType `shouldBe` Just "a -> a"

		it "should distinguish between constructor and data" $ do
			whoData <- send s ["whoat", "20", "9", "--file", dir </> "tests/test-package/ModuleOne.hs"]
			whoCtor <- send s ["whoat", "21", "8", "--file", dir </> "tests/test-package/ModuleOne.hs"]
			(whoData ^? _Just . values . key "info" . key "what" . _String) `shouldBe` Just "data"
			(whoCtor ^? _Just . values . key "info" . key "what" . _String) `shouldBe` Just "ctor"

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
			(checks ^.. _Just . values . key "note" . key "message" . _String) `shouldSatisfy` (any ("Defined but not used" `T.isPrefixOf`))

		it "should get usages of symbol" $ do
			-- Note, that source was modified
			us <- send s ["usages", "2", "2", "--file", dir </> "tests/test-package/ModuleTwo.hs"]
			let
				locs :: [(Text, Region)]
				locs = do
					n <- us ^.. _Just . values
					nm <- maybeToList $ n ^? key "in" . key "name" . _String
					p <- maybeToList $ n ^? key "at" . _JSON
					return (nm, p)
			S.fromList locs `shouldBe` S.fromList [
				("ModuleOne", Position 4 2 `region` Position 4 12),
				("ModuleOne", Position 15 1 `region` Position 15 11),
				("ModuleTwo", Position 2 2 `region` Position 2 12),
				("ModuleTwo", Position 8 19 `region` Position 8 29),
				("ModuleTwo", Position 25 21 `region` Position 25 31),
				("ModuleTwo", Position 25 35 `region` Position 25 45)]

		it "should get usages of local symbols" $ do
			us <- send s ["usages", "14", "15", "--file", dir </> "tests/test-package/ModuleTwo.hs"]
			let
				locs :: [Region]
				locs = do
					n <- us ^.. _Just . values
					maybeToList $ n ^? key "at" . _JSON
			S.fromList locs `shouldBe` S.fromList [
				Position 14 7 `region` Position 14 8,
				Position 14 11 `region` Position 14 12,
				Position 14 15 `region` Position 14 16]

		it "should evaluate expression in context of module" $ do
			vals <- send s ["ghc", "eval", "smth [1,2,3]", "--file", dir </> "tests/test-package/ModuleThree.hs"]
			vals ^.. _Just . values . key "ok" . _String `shouldBe` ["[3,4]"]

		it "should return type of expression in context of module" $ do
			tys <- send s ["ghc", "type", "twice twice", "--file", dir </> "tests/test-package/ModuleTwo.hs"]
			tys ^.. _Just . values . key "ok" . _String `shouldBe` ["(a -> a) -> a -> a"]

		it "shouldn't lose module info if unsaved source is broken" $ do
			brokenContents <- fmap unpack $ readFileUtf8 $ dir </> "tests/data/ModuleTwo.broken.hs"
			void $ send s ["set-file-contents", "--file", dir </> "tests/test-package/ModuleTwo.hs", "--contents", brokenContents]
			-- Call scan to wait until file updated
			void $ send s ["scan", "--file", dir </> "tests/test-package/ModuleTwo.hs"]

			two <- send s ["module", "ModuleTwo", "--exact"]
			exports two `shouldBe` mkSet ["untypedFoo", "twice", "overloadedStrings", "useUntypedFoo"]

		it "should complete qualified names" $ do
			comps <- send s ["complete", "T.o", "--file", dir </> "tests/test-package/ModuleThree.hs"]
			comps ^.. _Just . values . key "id" . key "name" . _String `shouldBe` ["overloadedStrings"]

		_ <- runIO $ send s ["exit"]
		return ()
