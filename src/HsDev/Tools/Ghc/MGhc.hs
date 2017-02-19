{-# LANGUAGE UnicodeSyntax, GeneralizedNewtypeDeriving, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Tools.Ghc.MGhc (
	SessionState(..), sessionActive, sessionMap,
	MGhcT(..), runMGhcT, liftGhc,
	hasSession, findSession, findSessionBy, saveSession,
	initSession, newSession,
	switchSession, switchSession_,
	deleteSession, restoreSession, usingSession
	) where

import Control.Lens
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import Data.Default
import Data.IORef
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)
import System.Log.Simple

import DynFlags
import Exception hiding (catch, mask, uninterruptibleMask, bracket)
import GHC
import GhcMonad
import HscTypes
import Outputable
import SysTools

data SessionState s = SessionState {
	_sessionActive :: Maybe s,
	_sessionMap :: Map s HscEnv }

instance Default (SessionState s) where
	def = SessionState Nothing M.empty

sessionActive :: Lens' (SessionState s) (Maybe s)
sessionActive = lens g s where
	g = _sessionActive
	s st nm = st { _sessionActive = nm }

sessionMap :: Lens' (SessionState s) (Map s HscEnv)
sessionMap = lens g s where
	g = _sessionMap
	s st m = st { _sessionMap = m }

instance ExceptionMonad m => ExceptionMonad (StateT s m) where
	gcatch act onError = StateT $ \st -> gcatch (runStateT act st) (\e -> runStateT (onError e) st)
	gmask f = StateT $ gmask . f' where
		f' st' act' = runStateT (f act) st' where
			act st = StateT $ act' . runStateT st

instance ExceptionMonad m => ExceptionMonad (ReaderT r m) where
	gcatch act onError = ReaderT $ \v -> gcatch (runReaderT act v) (\e -> runReaderT (onError e) v)
	gmask f = ReaderT $ gmask . f' where
		f' v' act' = runReaderT (f act) v' where
			act v = ReaderT $ act' . runReaderT v

-- | Multi-session ghc monad
newtype MGhcT s m a = MGhcT { unMGhcT :: GhcT (ReaderT (Maybe FilePath) (StateT (SessionState s) m)) a }
	deriving (Functor, Applicative, Monad, MonadIO, ExceptionMonad, HasDynFlags, GhcMonad, MonadState (SessionState s), MonadReader (Maybe FilePath), MonadThrow, MonadCatch, MonadMask, MonadLog)

instance MonadTrans GhcT where
	lift = liftGhcT

instance MonadState st m => MonadState st (GhcT m) where
	get = lift get
	put = lift . put
	state = lift . state

instance MonadReader r m => MonadReader r (GhcT m) where
	ask = lift ask
	local f act = GhcT $ local f . unGhcT act

instance MonadThrow m => MonadThrow (GhcT m) where
	throwM = lift . throwM

instance MonadCatch m => MonadCatch (GhcT m) where
	catch act onError = GhcT $ \sess -> catch (unGhcT act sess) (flip unGhcT sess . onError)

instance MonadMask m => MonadMask (GhcT m) where
	mask f = GhcT $ \s -> mask $ \g -> unGhcT (f $ q g) s where
		q g' act = GhcT $ g' . unGhcT act
	uninterruptibleMask f = GhcT $ \s -> uninterruptibleMask $ \g -> unGhcT (f $ q g) s where
		q g' act = GhcT $ g' . unGhcT act

instance MonadLog m => MonadLog (GhcT m) where
	askLog = lift askLog

-- | Run multi-session ghc
runMGhcT :: (MonadIO m, ExceptionMonad m, Ord s) => Maybe FilePath -> MGhcT s m a -> m a
runMGhcT lib act = do
	ref <- liftIO $ newIORef (panic "empty session")
	let
		session = Session ref
	flip evalStateT def $ flip runReaderT lib $ flip unGhcT session $ unMGhcT $ act `gfinally` cleanup
	where
		cleanup :: (MonadIO m, ExceptionMonad m, Ord s) => MGhcT s m ()
		cleanup = do
			void saveSession
			sessions <- gets (M.elems . view sessionMap)
			liftIO $ mapM_ cleanupSession sessions
			modify (set sessionMap M.empty)

-- | Lift `Ghc` monad onto `MGhc`
liftGhc :: MonadIO m => Ghc a -> MGhcT s m a
liftGhc (Ghc act) = MGhcT $ GhcT $ liftIO . act

-- | Does session exist
hasSession :: (MonadIO m, Ord s) => s -> MGhcT s m Bool
hasSession key = do
	msess <- gets (preview (sessionMap . ix key))
	return $ isJust msess

-- | Find session
findSession :: (MonadIO m, Ord s) => s -> MGhcT s m (Maybe s)
findSession key = do
	mkeys <- gets (M.keys . view sessionMap)
	return $ find (== key) mkeys

-- | Find session by
findSessionBy :: MonadIO m => (s -> Bool) -> MGhcT s m [s]
findSessionBy p = do
	mkeys <- gets (M.keys . view sessionMap)
	return $ filter p mkeys

-- | Save current session
saveSession :: (MonadIO m, ExceptionMonad m, Ord s) => MGhcT s m (Maybe s)
saveSession = do
	key <- gets (view sessionActive)
	case key of
		Just key' -> do
			sess <- getSession
			modify (set (sessionMap . at key') (Just sess))
		Nothing -> return ()
	return key

-- | Initialize new session
initSession :: (MonadIO m, ExceptionMonad m, Ord s) => MGhcT s m ()
initSession = do
	lib <- ask
	initGhcMonad lib
	void saveSession

activateSession :: (MonadIO m, ExceptionMonad m, Ord s) => s -> MGhcT s m (Maybe HscEnv)
activateSession key = do
	void saveSession
	modify (set sessionActive $ Just key)
	gets (view (sessionMap . at key))

-- | Create new named session, deleting existing session
newSession :: (MonadIO m, ExceptionMonad m, Ord s) => s -> MGhcT s m ()
newSession key = do
	msess <- activateSession key
	maybe (return ()) (liftIO . cleanupSession) msess
	initSession

-- | Switch to session, creating if not exist, returns True if session was created
switchSession :: (MonadIO m, ExceptionMonad m, Ord s) => s -> MGhcT s m Bool
switchSession key = do
	msess <- activateSession key
	case msess of
		Nothing -> initSession >> return True
		Just sess -> setSession sess >> return False

-- | Switch to session, creating if not exist and initializing with passed function
switchSession_ :: (MonadIO m, ExceptionMonad m, Ord s) => s -> Maybe (MGhcT s m ()) -> MGhcT s m ()
switchSession_ key f = do
	new <- switchSession key
	when new $ fromMaybe (return ()) f

-- | Delete existing session
deleteSession :: (MonadIO m, ExceptionMonad m, Ord s) => s -> MGhcT s m ()
deleteSession key = do
	cur <- saveSession
	when (cur == Just key) $
		modify (set sessionActive Nothing)
	msess <- gets (view (sessionMap . at key))
	modify (set (sessionMap . at key) Nothing)
	case msess of
		Nothing -> return ()
		Just sess -> liftIO $ cleanupSession sess

-- | Save and restore session
restoreSession :: (MonadIO m, MonadMask m, ExceptionMonad m, Ord s) => MGhcT s m a -> MGhcT s m a
restoreSession act = bracket saveSession (maybe (return ()) (void . switchSession)) $ const act

-- | Run action using session, restoring session back
usingSession :: (MonadIO m, MonadMask m, ExceptionMonad m, Ord s) => s -> MGhcT s m a -> MGhcT s m a
usingSession key act = restoreSession $ do
	void $ switchSession key
	act

-- | Cleanup session
cleanupSession :: HscEnv -> IO ()
cleanupSession env = do
	cleanTempFiles df
	cleanTempDirs df
	where
		df = hsc_dflags env
