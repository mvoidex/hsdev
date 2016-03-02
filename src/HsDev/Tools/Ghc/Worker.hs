{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Tools.Ghc.Worker (
	-- * Workers
	ghcWorker, ghciWorker,
	-- * Initializers and actions
	ghcRun,
	withFlags, modifyFlags, addCmdOpts,
	importModules, preludeModules,
	evaluate,
	clearTargets, makeTarget, loadTargets,
	-- * Utils
	listPackages, spanRegion,
	withCurrentDirectory,

	Ghc,

	module Control.Concurrent.Worker
	) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Data.Dynamic
import Data.Maybe
import Data.Time.Clock (getCurrentTime)
import Data.Version (showVersion)
import GHC
import GHC.Paths
import Packages
import StringBuffer
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import Text.Read

import Control.Concurrent.Worker

import HsDev.Symbols.Location (Position(..), Region(..), region, ModulePackage)

-- | Ghc worker. Pass options and initializer action
ghcWorker :: [String] -> Ghc () -> IO (Worker Ghc)
ghcWorker opts initialize = startWorker (runGhc (Just libdir)) ghcInit id where
	ghcInit f = ghcRun opts (initialize >> f)

-- | Interpreter worker is worker with @preludeModules@ loaded
ghciWorker :: IO (Worker Ghc)
ghciWorker = ghcWorker [] (importModules preludeModules)

-- | Run ghc
ghcRun :: [String] -> Ghc a -> Ghc a
ghcRun opts f = do
	fs <- getSessionDynFlags
	defaultCleanupHandler fs $ do
		(fs', _, _) <- parseDynamicFlags fs (map noLoc opts)
		let fs'' = fs' {
			ghcMode = CompManager,
			ghcLink = LinkInMemory,
			hscTarget = HscInterpreted }
		_ <- setSessionDynFlags fs''
		_ <- liftIO $ initPackages fs''
		f

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

-- | Get list of installed packages
listPackages :: Ghc [ModulePackage]
listPackages = liftM (mapMaybe readPackage . fromMaybe [] . pkgDatabase) getSessionDynFlags

readPackage :: PackageConfig -> Maybe ModulePackage
readPackage pc = readMaybe $ packageNameString pc ++ "-" ++ showVersion (packageVersion pc)

-- | Get region of @SrcSpan@
spanRegion :: SrcSpan -> Region
spanRegion (RealSrcSpan s) = Position (srcSpanStartLine s) (srcSpanStartCol s) `region` Position (srcSpanEndLine s) (srcSpanEndCol s)
spanRegion _ = Position 0 0 `region` Position 0 0

-- | Set current directory and restore it after action
withCurrentDirectory :: FilePath -> Ghc a -> Ghc a
withCurrentDirectory dir act = gbracket (liftIO getCurrentDirectory) (liftIO . setCurrentDirectory) $
	const (liftIO (setCurrentDirectory dir) >> act)

-- TODO: Load target by @ModuleLocation@, which may cause updating @DynFlags@

instance MonadThrow Ghc where
	throwM = liftIO . throwM

instance MonadCatch Ghc where
	catch = gcatch
