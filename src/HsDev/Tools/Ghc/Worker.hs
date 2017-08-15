{-# LANGUAGE PatternGuards, OverloadedStrings, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Tools.Ghc.Worker (
	-- * Workers
	SessionType(..), SessionConfig(..),
	GhcM, GhcWorker, MGhcT(..), runGhcM,
	ghcWorker,
	workerSession, ghcSession, ghciSession, haddockSession, tmpSession,

	-- * Initializers and actions
	ghcRun,
	withFlags, modifyFlags, addCmdOpts, setCmdOpts,
	importModules, preludeModules,
	evaluate,
	clearTargets, makeTarget, loadTargets,
	loadInteractive, reload,
	-- * Utils
	listPackages, spanRegion,
	withCurrentDirectory,
	logToChan, logToNull,

	Ghc,
	LogT(..),

	module HsDev.Tools.Ghc.MGhc,
	module Control.Concurrent.Worker
	) where

import Control.Lens (view, over)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Catch
import Data.Dynamic
import Data.Maybe
import Data.Time.Clock (getCurrentTime)
import Data.Version (showVersion)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.FilePath
import qualified  System.Log.Simple as Log
import System.Log.Simple.Monad (MonadLog(..), LogT(..), withLog)
import Text.Read (readMaybe)
import Text.Format hiding (withFlags)

import Exception (ExceptionMonad(..))
import GHC hiding (Warning, Module, moduleName, pkgDatabase)
import GHC.Paths
import Outputable
import FastString (unpackFS)
import Packages
import StringBuffer

import Control.Concurrent.FiniteChan
import Control.Concurrent.Worker
import System.Directory.Paths
import HsDev.Symbols.Location (Position(..), Region(..), region, ModulePackage, ModuleLocation(..))
import HsDev.Tools.Types
import HsDev.Tools.Ghc.Compat hiding (setLogAction)
import qualified HsDev.Tools.Ghc.Compat as C (setLogAction)
import HsDev.Tools.Ghc.MGhc

data SessionType = SessionGhci | SessionGhc | SessionHaddock | SessionTmp deriving (Eq, Ord)
data SessionConfig = SessionConfig SessionType [String] deriving (Eq, Ord)

instance Show SessionType where
	show SessionGhci = "ghci"
	show SessionGhc = "ghc"
	show SessionHaddock = "haddock"
	show SessionTmp = "tmp"

instance Show SessionConfig where
	show (SessionConfig t opts) = unwords (show t : opts)

instance Formattable SessionConfig

type GhcM a = MGhcT SessionConfig (LogT IO) a

type GhcWorker = Worker (MGhcT SessionConfig (LogT IO))

instance (Monad m, GhcMonad m) => GhcMonad (ReaderT r m) where
	getSession = lift getSession
	setSession = lift . setSession

instance ExceptionMonad m => ExceptionMonad (LogT m) where
	gcatch act onError = LogT $ gcatch (runLogT act) (runLogT . onError)
	gmask f = LogT $ gmask f' where
		f' g' = runLogT $ f (LogT . g' . runLogT)

instance MonadThrow Ghc where
	throwM = liftIO . throwM

runGhcM :: MonadLog m => Maybe FilePath -> GhcM a -> m a
runGhcM dir act = do
	l <- Log.askLog
	liftIO $ withLog l $ runMGhcT dir act

-- | Multi-session ghc worker
ghcWorker :: MonadLog m => m GhcWorker
ghcWorker = do
	l <- Log.askLog
	liftIO $ startWorker (withLog l . runGhcM (Just libdir)) id (Log.scope "ghc")

-- | Create session with options
workerSession :: SessionType -> [String] -> GhcM ()
workerSession ty opts = do
	ms <- findSessionBy toKill
	forM_ ms $ \s' -> do
		Log.sendLog Log.Trace $ "killing session: {}" ~~ s'
		deleteSession s'
	Log.sendLog Log.Trace $ "session: {}" ~~ SessionConfig ty opts
	switchSession_ (SessionConfig ty opts) $ Just $ initialize
	where
		toKill (SessionConfig ty' opts') = or [
			(ty == ty' && opts /= opts'),
			(ty /= ty' && ty' `elem` [SessionTmp, SessionHaddock])]
		initialize = case ty of
			SessionGhci -> ghcRun opts (importModules preludeModules)
			SessionGhc -> ghcRun opts (return ())
			SessionTmp -> ghcRun opts (return ())
			SessionHaddock -> ghcRun ("-haddock" : opts) (return ())

-- | Get ghc session
ghcSession :: [String] -> GhcM ()
ghcSession = workerSession SessionGhc

-- | Get ghci session
ghciSession :: GhcM ()
ghciSession = workerSession SessionGhci []

-- | Get haddock session with flags
haddockSession :: [String] -> GhcM ()
haddockSession opts = workerSession SessionHaddock opts

-- | Get haddock session with flags
tmpSession :: [String] -> GhcM ()
tmpSession opts = workerSession SessionTmp opts

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
		modifyFlags $ C.setLogAction logToNull
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
	-- _ <- liftIO $ initPackages fs'
	return ()

-- | Add options without reinit session
addCmdOpts :: (MonadLog m, GhcMonad m) => [String] -> m ()
addCmdOpts [] = return ()
addCmdOpts opts = do
	Log.sendLog Log.Trace $ "setting ghc options: {}" ~~ unwords opts
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
	Log.sendLog Log.Trace $ "restarting ghc session with: {}" ~~ unwords opts
	initGhcMonad (Just libdir)
	addCmdOpts opts
	modifyFlags $ C.setLogAction logToNull

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
makeTarget :: GhcMonad m => Text -> Maybe Text -> m Target
makeTarget name Nothing = guessTarget (T.unpack name) Nothing
makeTarget name (Just cts) = do
	t <- guessTarget (T.unpack name) Nothing
	tm <- liftIO getCurrentTime
	return t { targetContents = Just (stringToStringBuffer $ T.unpack cts, tm) }

-- | Load all targets
loadTargets :: GhcMonad m => [Target] -> m ()
loadTargets ts = setTargets ts >> load LoadAllTargets >> return ()

-- | Load and set interactive context
loadInteractive :: GhcMonad m => Path -> Maybe Text -> m ()
loadInteractive fpath mcts = do
	fpath' <- liftIO $ canonicalize fpath
	withCurrentDirectory (view path $ takeDir fpath') $ do
		t <- makeTarget (over path takeFileName fpath') mcts
		loadTargets [t]
		g <- getModuleGraph
		setContext [IIModule (ms_mod_name m) | m <- g]

-- | Reload targets
reload :: GhcMonad m => m ()
reload = do
	ts <- getTargets
	ctx <- getContext
	setContext []
	clearTargets
	setTargets ts
	setContext ctx

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
		putChan ch Note {
			_noteSource = src',
			_noteRegion = spanRegion src,
			_noteLevel = Just sev',
			_note = OutputMessage {
				_message = fromString $ showSDoc fs msg,
				_messageSuggestion = Nothing } }
	| otherwise = return ()
	where
		checkSev SevWarning = Just Warning
		checkSev SevError = Just Error
		checkSev SevFatal = Just Error
		checkSev _ = Nothing
		srcMod = case src of
			RealSrcSpan s' -> FileModule (fromFilePath $ unpackFS $ srcSpanFile s') Nothing
			_ -> NoLocation

-- | Don't log ghc warnings and errors
logToNull :: LogAction
logToNull _ _ _ _ = return ()

-- TODO: Load target by @ModuleLocation@, which may cause updating @DynFlags@
