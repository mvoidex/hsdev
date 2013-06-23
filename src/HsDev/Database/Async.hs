module HsDev.Database.Async (
	traceEvents,
	update,
	module Data.Async
	) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map as M
import qualified Data.Set as S

import Data.Async

import HsDev.Database
import HsDev.Symbols
import HsDev.Symbols.Util

traceEvents :: Async Database -> (String -> IO ()) -> IO ()
traceEvents avar logMsg = subscribeEvents avar traceEvent where
	traceEvent (Append db) = forM_ (dbModules db) $ \m ->
		logMsg $ "appending " ++ showModule m
	traceEvent (Remove db) = forM_ (dbModules db) $ \m ->
		logMsg $ "removing " ++ showModule m
	traceEvents Clear = logMsg "clearing"
	traceEvents (Modify _) = logMsg "custom modify"
	traceEvents (Action _) = logMsg "custom action"

	dbModules :: Database -> [Symbol Module]
	dbModules db = concatMap S.toList $ M.elems $ databaseModules db

	showModule :: Symbol Module -> String
	showModule m
		| bySources m = symbolName m ++ " in " ++ maybe "" locationFile (symbolLocation m)
		| otherwise = symbolName m ++ maybe "" (\c -> if c == Cabal then "" else (" in " ++ show c)) (moduleCabal $ symbol m)

update :: MonadIO m => Async Database -> m Database -> m ()
update db act = do
	db' <- act
	liftIO $ modifyAsync db (Append db')
