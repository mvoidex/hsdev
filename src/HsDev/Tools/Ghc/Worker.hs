module HsDev.Tools.Ghc.Worker (
	Worker(..),
	startWorker,
	waitWork,
	evaluate,
	try,

	Ghc
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

data Worker = Worker {
	ghcSendWork :: Ghc () -> IO (),
	ghcWorkerChan :: Chan (Ghc ()) }

startWorker :: IO Worker
startWorker = do
	ch <- newChan
	void $ forkIO $ runGhc (Just libdir) $ do
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
			liftIO (getChanContents ch) >>= mapM_ try
	return $ Worker (writeChan ch) ch
	where
		startMods :: [String]
		startMods = ["Prelude", "Data.List", "Control.Monad"]

waitWork :: Worker -> Ghc a -> ErrorT String IO a
waitWork w act = ErrorT $ do
	var <- newEmptyMVar
	ghcSendWork w $ try act >>= liftIO . putMVar var
	takeMVar var

evaluate :: String -> Ghc String
evaluate expr = liftM fromDynamic (dynCompileExpr $ "show (" ++ expr ++ ")") >>=
	maybe (fail "evaluate fail") return

try :: Ghc a -> Ghc (Either String a)
try = liftM (left (show :: SomeException -> String)) . gtry
