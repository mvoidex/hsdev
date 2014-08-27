module HsDev.Tools.Ghc.Worker (
	Worker(..),
	startWorker,
	waitWork,
	evaluate,

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
			liftIO (getChanContents ch) >>= mapM_ wrapError
	return $ Worker (writeChan ch) ch
	where
		startMods :: [String]
		startMods = ["Prelude", "Data.List", "Control.Monad"]

		wrapError :: Ghc a -> Ghc (Either SomeException a)
		wrapError = gtry

waitWork :: Worker -> Ghc a -> ErrorT String IO a
waitWork w act = ErrorT $ do
	var <- newEmptyMVar
	ghcSendWork w $ gtry act >>= liftIO . putMVar var . left (show :: SomeException -> String)
	takeMVar var

evaluate :: String -> Ghc (Maybe String)
evaluate expr = liftM fromDynamic $ dynCompileExpr $ "show (" ++ expr ++ ")"
