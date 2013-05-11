module HsDev.Database.Async (
	traceEvents,
	module Data.Async
	) where

import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S

import Data.Async

import HsDev.Database
import HsDev.Symbols
import HsDev.Symbols.Util

traceEvents :: Async Database -> IO ()
traceEvents avar = subscribeEvents avar traceEvent where
	traceEvent (Append db) = forM_ (dbModules db) $ \m ->
		putStrLn $ "appending " ++ showModule m
	traceEvent (Remove db) = forM_ (dbModules db) $ \m ->
		putStrLn $ "removing " ++ showModule m
	traceEvents Clear = putStrLn "clearing"
	traceEvents (Modify _) = putStrLn "custom modify"
	traceEvents (Action _) = putStrLn "custom action"

	dbModules :: Database -> [Symbol Module]
	dbModules db = concatMap S.toList $ M.elems $ databaseModules db

	showModule :: Symbol Module -> String
	showModule m
		| bySources m = symbolName m ++ " in " ++ maybe "" locationFile (symbolLocation m)
		| otherwise = symbolName m ++ maybe "" (\c -> if c == Cabal then "" else (" in " ++ show c)) (moduleCabal $ symbol m)
