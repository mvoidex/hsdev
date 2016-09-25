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
		it "should scan project" $ do
			dir <- getCurrentDirectory
			s <- startServer (def { serverSilent = True })
			_ <- call s $ Scan [dir </> "tests/test-package"] False [] [] [] [] False False
			one <- call s $ InfoResolve (dir </> "tests/test-package/ModuleOne.hs") True
			when (["test", "forkIO", "f"] /= exports one) $
				expectationFailure "invalid exports of ModuleOne.hs"
			two <- call s $ InfoResolve (dir </> "tests/test-package/ModuleTwo.hs") True
			when (["f", "twice"] /= exports two) $
				expectationFailure "invalid exports of ModuleTwo.hs"
			_ <- call s Exit
			return ()
