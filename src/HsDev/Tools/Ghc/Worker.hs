{-# LANGUAGE PatternGuards, OverloadedStrings, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Tools.Ghc.Worker (
	-- * Workers
	GhcM(..), runGhcM, liftGhc,
	ghcWorker, ghciWorker,
	-- * Initializers and actions
	ghcRun,
	withFlags, modifyFlags, addCmdOpts, setCmdOpts,
	importModules, preludeModules,
	evaluate,
	clearTargets, makeTarget, loadTargets,
	-- * Utils
	listPackages, spanRegion,
	withCurrentDirectory,
	logToChan, logToNull,

	Ghc,

	module Control.Concurrent.Worker
	) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Data.Dynamic
import Data.Maybe
import Data.Time.Clock (getCurrentTime)
import Data.Version (showVersion)
import Packages
import StringBuffer
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import qualified  System.Log.Simple as Log
import System.Log.Simple.Monad (MonadLog(..))
import Text.Read (readMaybe)
import Text.Format

import Exception (ExceptionMonad(..))
import GHC hiding (Warning, Module, moduleName, pkgDatabase)
import GhcMonad (Ghc(..))
import GHC.Paths
import DynFlags (HasDynFlags(..))
import Outputable
import FastString (unpackFS)

import Control.Concurrent.FiniteChan
import Control.Concurrent.Worker
import System.Directory.Paths
import HsDev.Symbols.Location (Position(..), Region(..), region, ModulePackage, ModuleLocation(..))
import HsDev.Tools.Types
import HsDev.Tools.Ghc.Compat

instance MonadThrow Ghc where
	throwM = liftIO . throwM

instance MonadCatch Ghc where
	catch = gcatch

instance MonadMask Ghc where
	mask f = Ghc $ \s -> mask $ \g -> unGhc (f $ q g) s where
		q :: (IO a -> IO a) -> Ghc a -> Ghc a
		q g' act = Ghc $ g' . unGhc act
	uninterruptibleMask f = Ghc $ \s -> uninterruptibleMask $ \g -> unGhc (f $ q g) s where
		q :: (IO a -> IO a) -> Ghc a -> Ghc a
		q g' act = Ghc $ g' . unGhc act

instance ExceptionMonad m => ExceptionMonad (ReaderT r m) where
	gcatch act onError = ReaderT $ \v -> gcatch (runReaderT act v) (flip runReaderT v . onError)
	gmask f = ReaderT $ \v -> gmask (\h -> flip runReaderT v (f $ \act -> ReaderT (\v' -> h (runReaderT act v'))))

instance (Monad m, GhcMonad m) => GhcMonad (ReaderT r m) where
	getSession = lift getSession
	setSession = lift . setSession

newtype GhcM a = GhcM { unGhcM :: ReaderT Log.Log Ghc a }
	deriving (Functor, Applicative, Monad, MonadIO, MonadMask, MonadThrow, MonadCatch, ExceptionMonad, HasDynFlags, MonadLog, GhcMonad)

runGhcM :: MonadLog m => Maybe FilePath -> GhcM a -> m a
runGhcM dir act = do
	l <- askLog
	liftIO $ runGhc dir (runReaderT (unGhcM act) l)

liftGhc :: Ghc a -> GhcM a
liftGhc = GhcM . lift

-- | Ghc worker. Pass options and initializer action
ghcWorker :: MonadLog m => [String] -> GhcM () -> m (Worker GhcM)
ghcWorker opts initialize = do
	l <- askLog
	liftIO $ startWorker (flip runReaderT l . runGhcM (Just libdir)) (ghcRun opts . (initialize >>)) (Log.scope "ghc")

-- | Interpreter worker is worker with @preludeModules@ loaded
ghciWorker :: MonadLog m => m (Worker GhcM)
ghciWorker = do
	l <- askLog
	liftIO $ startWorker (flip runReaderT l . runGhcM (Just libdir)) (ghcRun [] . (importModules preludeModules >>)) (Log.scope "ghc")

-- | Run ghc
ghcRun :: GhcMonad m => [String] -> m a -> m a
ghcRun opts f = do
	fs <- getSessionDynFlags
	cleanupHandler fs $ do
		(fs', _, _) <- parseDynamicFlags fs (map noLoc opts)
		let fs'' = fs' {
			ghcMode = CompManager,
			ghcLink = LinkInMemory,
			hscTarget = HscInterpreted }
			-- ghcLink = NoLink,
			-- hscTarget = HscNothing }
		void $ setSessionDynFlags fs''
		f

-- | Alter @DynFlags@ temporary
withFlags :: GhcMonad m => m a -> m a
withFlags = gbracket getSessionDynFlags (\fs -> setSessionDynFlags fs >> return ()) . const

-- | Update @DynFlags@
modifyFlags :: GhcMonad m => (DynFlags -> DynFlags) -> m ()
modifyFlags f = do
	fs <- getSessionDynFlags
	let
		fs' = f fs
	_ <- setSessionDynFlags fs'
	_ <- liftIO $ initPackages fs'
	return ()

-- | Add options without reinit session
addCmdOpts :: (MonadLog m, GhcMonad m) => [String] -> m ()
addCmdOpts opts = do
	Log.log Log.Trace $ "setting ghc options: {}" ~~ unwords opts
	fs <- getSessionDynFlags
	(fs', _, _) <- parseDynamicFlags fs (map noLoc opts)
	let fs'' = fs' {
		ghcMode = CompManager,
		ghcLink = LinkInMemory,
		hscTarget = HscInterpreted }
		-- ghcLink = NoLink,
		-- hscTarget = HscNothing }
	void $ setSessionDynFlags fs''

-- | Set options after session reinit
setCmdOpts :: (MonadLog m, GhcMonad m) => [String] -> m ()
setCmdOpts opts = do
	Log.log Log.Trace $ "restarting ghc session with: {}" ~~ unwords opts
	initGhcMonad (Just libdir)
	addCmdOpts opts
	modifyFlags $ setLogAction logToNull

-- | Import some modules
importModules :: GhcMonad m => [String] -> m ()
importModules mods = mapM parseImportDecl ["import " ++ m | m <- mods] >>= setContext . map IIDecl

-- | Default interpreter modules
preludeModules :: [String]
preludeModules = ["Prelude", "Data.List", "Control.Monad", "HsDev.Tools.Ghc.Prelude"]

-- | Evaluate expression
evaluate :: GhcMonad m => String -> m String
evaluate expr = liftM fromDynamic (dynCompileExpr $ "show ({})" ~~ expr) >>=
	maybe (fail "evaluate fail") return

-- | Clear loaded targets
clearTargets :: GhcMonad m => m ()
clearTargets = loadTargets []

-- | Make target with its source code optional
makeTarget :: GhcMonad m => String -> Maybe String -> m Target
makeTarget name Nothing = guessTarget name Nothing
makeTarget name (Just cts) = do
	t <- guessTarget name Nothing
	tm <- liftIO getCurrentTime
	return t { targetContents = Just (stringToStringBuffer cts, tm) }

-- | Load all targets
loadTargets :: GhcMonad m => [Target] -> m ()
loadTargets ts = setTargets ts >> load LoadAllTargets >> return ()

-- | Get list of installed packages
listPackages :: GhcMonad m => m [ModulePackage]
listPackages = liftM (mapMaybe readPackage . fromMaybe [] . pkgDatabase) getSessionDynFlags

readPackage :: PackageConfig -> Maybe ModulePackage
readPackage pc = readMaybe $ packageNameString pc ++ "-" ++ showVersion (packageVersion pc)

-- | Get region of @SrcSpan@
spanRegion :: SrcSpan -> Region
spanRegion (RealSrcSpan s) = Position (srcSpanStartLine s) (srcSpanStartCol s) `region` Position (srcSpanEndLine s) (srcSpanEndCol s)
spanRegion _ = Position 0 0 `region` Position 0 0

-- | Set current directory and restore it after action
withCurrentDirectory :: GhcMonad m => FilePath -> m a -> m a
withCurrentDirectory dir act = gbracket (liftIO getCurrentDirectory) (liftIO . setCurrentDirectory) $
	const (liftIO (setCurrentDirectory dir) >> act)

-- | Log  ghc warnings and errors as to chan
-- You may have to apply recalcTabs on result notes
logToChan :: Chan (Note OutputMessage) -> LogAction
logToChan ch fs sev src msg
	| Just sev' <- checkSev sev = do
		src' <- canonicalize srcMod
		putChan ch $ Note {
			_noteSource = src',
			_noteRegion = spanRegion src,
			_noteLevel = Just sev',
			_note = OutputMessage {
				_message = showSDoc fs msg,
				_messageSuggestion = Nothing } }
	| otherwise = return ()
	where
		checkSev SevWarning = Just Warning
		checkSev SevError = Just Error
		checkSev SevFatal = Just Error
		checkSev _ = Nothing
		srcMod = case src of
			RealSrcSpan s' -> FileModule (unpackFS $ srcSpanFile s') Nothing
			_ -> ModuleSource Nothing

-- | Don't log ghc warnings and errors
logToNull :: LogAction
logToNull _ _ _ _ = return ()

-- TODO: Load target by @ModuleLocation@, which may cause updating @DynFlags@
