{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Tools.Ghc.Worker (
	-- * Workers
	ghcWorker, ghciWorker,
	-- * Initializers and actions
	withFlags, modifyFlags, addCmdOpts,
	importModules, preludeModules,
	evaluate,
	clearTargets, makeTarget, loadTargets,

	Ghc,

	module Control.Concurrent.Worker
	) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Data.Dynamic
import Data.Time.Clock (getCurrentTime)
import GHC
import GHC.Paths
import Packages
import StringBuffer

import Control.Concurrent.Worker

-- | Ghc worker. Pass options and initializer action
ghcWorker :: [String] -> Ghc () -> IO (Worker Ghc)
ghcWorker opts initialize = startWorker (runGhc (Just libdir)) ghcInit id where
	ghcInit f = do
		fs <- getSessionDynFlags
		defaultCleanupHandler fs $ do
			(fs', _, _) <- parseDynamicFlags fs (map noLoc opts)
			let fs'' = fs' {
				ghcMode = CompManager,
				ghcLink = LinkInMemory,
				hscTarget = HscInterpreted }
			_ <- setSessionDynFlags fs''
			_ <- liftIO $ initPackages fs''
			initialize
			f

-- | Interpreter worker is worker with @preludeModules@ loaded
ghciWorker :: IO (Worker Ghc)
ghciWorker = ghcWorker [] (importModules preludeModules)

-- | Alter @DynFlags@ temporary
withFlags :: Ghc a -> Ghc a
withFlags = gbracket getSessionDynFlags (\fs -> setSessionDynFlags fs >> return ()) . const

-- | Update @DynFlags@
modifyFlags :: (DynFlags -> DynFlags) -> Ghc ()
modifyFlags f = do
	fs <- getSessionDynFlags
	let
		fs' = f fs
	_ <- setSessionDynFlags fs'
	_ <- liftIO $ initPackages fs'
	return ()

addCmdOpts :: [String] -> Ghc DynFlags
addCmdOpts opts = do
	fs <- getSessionDynFlags
	(fs', _, _) <- parseDynamicFlags fs (map noLoc opts)
	_ <- setSessionDynFlags fs'
	(fs'', _) <- liftIO $ initPackages fs'
	return fs''

-- | Import some modules
importModules :: [String] -> Ghc ()
importModules mods = mapM parseImportDecl ["import " ++ m | m <- mods] >>= setContext . map IIDecl

-- | Default interpreter modules
preludeModules :: [String]
preludeModules = ["Prelude", "Data.List", "Control.Monad", "HsDev.Tools.Ghc.Prelude"]

-- | Evaluate expression
evaluate :: String -> Ghc String
evaluate expr = liftM fromDynamic (dynCompileExpr $ "show (" ++ expr ++ ")") >>=
	maybe (fail "evaluate fail") return

-- | Clear loaded targets
clearTargets :: Ghc ()
clearTargets = loadTargets []

-- | Make target with its source code optional
makeTarget :: String -> Maybe String -> Ghc Target
makeTarget name Nothing = guessTarget name Nothing
makeTarget name (Just cts) = do
	t <- guessTarget name Nothing
	tm <- liftIO getCurrentTime
	return t { targetContents = Just (stringToStringBuffer cts, tm) }

-- | Load all targets
loadTargets :: [Target] -> Ghc ()
loadTargets ts = setTargets ts >> load LoadAllTargets >> return ()

-- TODO: Load target by @ModuleLocation@, which may cause updating @DynFlags@

instance MonadThrow Ghc where
	throwM = liftIO . throwM

instance MonadCatch Ghc where
	catch = gcatch
