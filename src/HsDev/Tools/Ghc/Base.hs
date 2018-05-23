{-# LANGUAGE PatternGuards #-}

module HsDev.Tools.Ghc.Base (
	-- * Running Ghc
	ghcRun, ghcRunWith,
	-- * Commonly used DynFlags
	interpretedFlags, noLinkFlags,
	-- * Setting DynFlags
	withFlags, modifyFlags,
	-- * Loading targets
	clearTargets, makeTarget, loadTargets,
	loadInteractive, reload,
	-- * Logging messages
	collectMessages, collectMessages_,

	-- * Util
	formatType,
	spanRegion,
	withCurrentDirectory,
	logToChan, logToNull
	) where

import Control.Lens (view, over)
import Control.Monad
import Control.Monad.Except
import Data.Time.Clock (getCurrentTime)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.FilePath

import Exception (ExceptionMonad(..))
import GHC hiding (Warning, Module)
import Outputable
import FastString (unpackFS)
import StringBuffer
import Type
import qualified Pretty

import Control.Concurrent.FiniteChan
import System.Directory.Paths
import HsDev.Symbols.Location (Position(..), Region(..), region, ModuleLocation(..))
import HsDev.Tools.Types
import HsDev.Tools.Ghc.Compat
import qualified HsDev.Tools.Ghc.Compat as C (setLogAction, addLogAction, unqualStyle)

-- | Run ghc
ghcRun :: GhcMonad m => [String] -> m a -> m a
ghcRun = ghcRunWith interpretedFlags

-- | Run ghc
ghcRunWith :: GhcMonad m => (DynFlags -> DynFlags) -> [String] -> m a -> m a
ghcRunWith onFlags opts act = do
	fs <- getSessionDynFlags
	cleanupHandler fs $ do
		(fs', _, _) <- parseDynamicFlags fs (map noLoc opts)
		void $ setSessionDynFlags $ onFlags fs'
		modifyFlags $ C.setLogAction logToNull
		act

interpretedFlags :: DynFlags -> DynFlags
interpretedFlags fs = fs {
	ghcMode = CompManager,
	ghcLink = LinkInMemory,
	hscTarget = HscInterpreted }

noLinkFlags :: DynFlags -> DynFlags
noLinkFlags fs = fs {
	ghcMode = CompManager,
	ghcLink = NoLink,
	hscTarget = HscNothing }

-- | Alter @DynFlags@ temporary
withFlags :: GhcMonad m => m a -> m a
withFlags = gbracket getSessionDynFlags (void . setSessionDynFlags) . const

-- | Update @DynFlags@
modifyFlags :: GhcMonad m => (DynFlags -> DynFlags) -> m ()
modifyFlags f = do
	fs <- getSessionDynFlags
	let
		fs' = f fs
	_ <- setSessionDynFlags fs'
	-- _ <- liftIO $ initPackages fs'
	return ()

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
		setContext [IIModule (ms_mod_name m) | m <- modSummaries g]

-- | Reload targets
reload :: GhcMonad m => m ()
reload = do
	ts <- getTargets
	ctx <- getContext
	setContext []
	clearTargets
	setTargets ts
	setContext ctx

-- | Collect messages from ghc for underlying computation
collectMessages :: GhcMonad m => m a -> m (a, [Note OutputMessage])
collectMessages act = do
	ch <- liftIO newChan
	r <- gbracket (liftM log_action getSessionDynFlags) (\action' -> modifyFlags (\fs -> fs { log_action = action' })) $ \_ -> do
		modifyFlags (C.addLogAction $ logToChan ch)
		act
	notes <- liftIO $ stopChan ch
	return (r, notes)

-- | Same as @collectMessages@, but when no result except notes needed
collectMessages_ :: GhcMonad m => m () -> m [Note OutputMessage]
collectMessages_ = fmap snd . collectMessages

-- | Format type for output
formatType :: GHC.DynFlags -> GHC.Type -> String
formatType dflag t = showOutputable dflag (removeForAlls t)

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
		void $ sendChan ch Note {
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

removeForAlls :: Type -> Type
removeForAlls ty = removeForAlls' ty' tty' where
	ty'  = dropForAlls ty
	tty' = splitFunTy_maybe ty'

removeForAlls' :: Type -> Maybe (Type, Type) -> Type
removeForAlls' ty Nothing = ty
removeForAlls' ty (Just (pre, ftype))
	| isPredTy pre = mkFunTy pre (dropForAlls ftype)
	| otherwise = ty

showOutputable :: Outputable a => DynFlags -> a -> String
showOutputable dflag = unwords . lines . showUnqualifiedPage dflag . ppr

showUnqualifiedPage :: DynFlags -> SDoc -> String
showUnqualifiedPage dflag = renderStyle Pretty.LeftMode 0 . withPprStyleDoc dflag (C.unqualStyle dflag)
