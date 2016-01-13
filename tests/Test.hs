{-# LANGUAGE OverloadedStrings #-}

module Main (
	main
	) where

import Control.Lens
import Data.Aeson hiding (Error)
import Data.Aeson.Lens
import Data.Default
import HsDev
import Test.Hspec

call :: Server -> Command -> IO (Maybe Value)
call srv c = do
	r <- inServer srv c
	case r of
		Result v -> return $ Just v
		Error e _ -> do
			expectationFailure $ "command result error: " ++ e
			return Nothing

exports :: Maybe Value -> [String]
exports v = v ^.. key "exports" . traverseArray . each . key "name" . each . _Just

main :: IO ()
main = hspec $ do
	describe "scan project" $ do
		it "should scan project" $ do
			s <- startServer def
			_ <- call s $ Scan ["tests\\test-package"] [] [] [] [] [] False False
			one <- call s $ InfoResolve "tests\\test-package\\ModuleOne.hs" True
			when (["test", "forkIO", "f"] /= exports one) $
				expectationFailure "invalid exports of ModuleOne.hs"
			two <- call s $ InfoResolve "tests\\test-package\\ModuleTwo.hs" True
			when (["f", "twice"] /= exports two) $
				expectationFailure "invalid exports of ModuleTwo.hs"
			_ <- call s Exit
			return ()
