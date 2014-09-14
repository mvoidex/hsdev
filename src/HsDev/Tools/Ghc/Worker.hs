module HsDev.Tools.Ghc.Worker (
	ghcWorker,
	waitWork,
	evaluate,
	try,

	Ghc,

	module Control.Concurrent.Worker
	) where

import Control.Arrow (left)
import Control.Concurrent
import Control.Exception (SomeException)
import Control.Monad
import Control.Monad.Error
import Data.Dynamic
import Exception (gtry)
import GHC
import GHC.Paths
import Packages

import Control.Concurrent.Worker

ghcWorker :: IO (Worker (Ghc ()))
ghcWorker = worker_ (runGhc (Just libdir)) ghcInit try where
	ghcInit f = do
		fs <- getSessionDynFlags
		defaultCleanupHandler fs $ do
			(fs', _, _) <- parseDynamicFlags fs (map noLoc [])
			let fs'' = fs' {
				ghcMode = CompManager,
				ghcLink = LinkInMemory,
				hscTarget = HscInterpreted }
			_ <- setSessionDynFlags fs''
			_ <- liftIO $ initPackages fs''
			mapM parseImportDecl ["import " ++ m | m <- startMods] >>= setContext . map IIDecl
			f
	startMods :: [String]
	startMods = ["Prelude", "Data.List", "Control.Monad", "HsDev.Tools.Ghc.Prelude"]

waitWork :: Worker (Ghc ()) -> Ghc a -> ErrorT String IO a
waitWork w act = ErrorT $ do
	var <- newEmptyMVar
	sendWork w $ try act >>= liftIO . putMVar var
	takeMVar var

evaluate :: String -> Ghc String
evaluate expr = liftM fromDynamic (dynCompileExpr $ "show (" ++ expr ++ ")") >>=
	maybe (fail "evaluate fail") return

try :: Ghc a -> Ghc (Either String a)
try = liftM (left (show :: SomeException -> String)) . gtry
