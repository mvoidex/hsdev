{-# LANGUAGE OverloadedStrings #-}

module Main (
	main
	) where

import Control.Lens
import Control.Exception (displayException)
import Data.Aeson hiding (Error)
import Data.Aeson.Lens
import Data.Default
import HsDev
import Test.Hspec
import System.FilePath
import System.Directory

call :: Server -> Command -> IO (Maybe Value)
call srv c = do
	r <- inServer srv def c
	case r of
		Result v -> return $ Just v
		Error e -> do
			expectationFailure $ "command result error: " ++ displayException e
			return Nothing

exports :: Maybe Value -> [String]
exports v = v ^.. key "exports" . traverseArray . each . key "name" . each . _Just

main :: IO ()
main = hspec $ do
	describe "scan project" $ do
		dir <- runIO getCurrentDirectory
		s <- runIO $ startServer (def { serverSilent = True })
		it "should scan project" $ do
			_ <- call s $ Scan [dir </> "tests/test-package"] False [] [] [] [] False False
			one <- call s $ InfoModule (SearchQuery "ModuleOne" SearchExact) [] False
			when (["test", "forkIO", "untypedFoo"] /= exports one) $
				expectationFailure "invalid exports of ModuleOne.hs"
			two <- call s $ InfoModule (SearchQuery "ModuleTwo" SearchExact) [] False
			when (["untypedFoo", "twice", "overloadedStrings"] /= exports two) $
				expectationFailure "invalid exports of ModuleTwo.hs"
		it "should pass extensions when checkings" $ do
			checks <- call s (Check [FileSource (dir </> "tests/test-package/ModuleTwo.hs") Nothing] [])
			when (("error" :: String) `elem` (checks ^.. traverseArray . key "level" . _Just)) $
				expectationFailure "there should be no errors, only warnings"
		_ <- runIO $ call s Exit
		return ()
