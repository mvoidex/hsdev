{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Tools.Ghc.Worker (
	ghcWorker,
	evaluate,

	Ghc,

	module Control.Concurrent.Worker
	) where

import Control.Monad
import Control.Monad.Error
import Control.Monad.Catch
import Data.Dynamic
import GHC
import GHC.Paths
import Packages

import Control.Concurrent.Worker

ghcWorker :: IO (Worker Ghc)
ghcWorker = startWorker (runGhc (Just libdir)) ghcInit id where
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

evaluate :: String -> Ghc String
evaluate expr = liftM fromDynamic (dynCompileExpr $ "show (" ++ expr ++ ")") >>=
	maybe (fail "evaluate fail") return

instance MonadThrow Ghc where
	throwM = liftIO . throwM

instance MonadCatch Ghc where
	catch = gcatch
