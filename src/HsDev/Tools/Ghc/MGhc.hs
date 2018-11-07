{-# LANGUAGE CPP, UnicodeSyntax, GeneralizedNewtypeDeriving, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Tools.Ghc.MGhc (
	Session(..), sessionKey, sessionData,
	SessionState(..), sessionActive, sessionMap,
	MGhcT(..), runMGhcT, liftGhc,
	currentSession, getSessionData, setSessionData, hasSession, findSession, findSessionBy, saveSession,
	initSession, newSession,
	switchSession, switchSession_,
	deleteSession, restoreSession, usingSession, tempSession
	) where

import Control.Lens
import Control.Monad.Fail as Fail
import Control.Monad.Morph
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import Data.Default as Def
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import System.Log.Simple.Monad (MonadLog(..))

import HsDev.Tools.Ghc.Compat (cleanTemps)

import DynFlags
import Exception hiding (catch, mask, uninterruptibleMask, bracket, finally)
import GHC
import GHCi
import GhcMonad hiding (Session(..))
import qualified GhcMonad (Session(..))
import HscTypes
import Outputable

data Session s d = Session {
	_sessionKey :: s,
	_sessionData :: d }
		deriving (Eq, Ord, Read, Show)

sessionKey :: Lens' (Session s d) s
sessionKey = lens g s where
	g = _sessionKey
	s sess k = sess { _sessionKey = k }

sessionData :: Lens' (Session s d) d
sessionData = lens g s where
	g = _sessionData
	s sess dat = sess { _sessionData = dat }

data SessionState s d = SessionState {
	_sessionActive :: Maybe (Session s d),
	_sessionMap :: Map s (HscEnv, d) }

instance Default (SessionState s d) where
	def = SessionState Nothing M.empty

sessionActive :: Lens' (SessionState s d) (Maybe (Session s d))
sessionActive = lens g s where
	g = _sessionActive
	s st nm = st { _sessionActive = nm }

sessionMap :: Lens' (SessionState s d) (Map s (HscEnv, d))
sessionMap = lens g s where
	g = _sessionMap
	s st m = st { _sessionMap = m }

instance ExceptionMonad m => ExceptionMonad (StateT s m) where
	gcatch act onErr = StateT $ \st -> gcatch (runStateT act st) (\e -> runStateT (onErr e) st)
	gmask f = StateT $ gmask . f' where
		f' st' act' = runStateT (f act) st' where
			act st = StateT $ act' . runStateT st

instance ExceptionMonad m => ExceptionMonad (ReaderT r m) where
	gcatch act onErr = ReaderT $ \v -> gcatch (runReaderT act v) (\e -> runReaderT (onErr e) v)
	gmask f = ReaderT $ gmask . f' where
		f' v' act' = runReaderT (f act) v' where
			act v = ReaderT $ act' . runReaderT v

-- | Multi-session ghc monad
newtype MGhcT s d m a = MGhcT { unMGhcT :: GhcT (ReaderT (Maybe FilePath) (StateT (SessionState s d) m)) a }
	deriving (Functor, Applicative, Monad, MonadFail, MonadIO, ExceptionMonad, HasDynFlags, GhcMonad, MonadState (SessionState s d), MonadReader (Maybe FilePath), MonadThrow, MonadCatch, MonadMask, MonadLog)

instance MonadTrans GhcT where
	lift = liftGhcT

instance MFunctor GhcT where
	hoist fn = GhcT . (fn .) . unGhcT

instance MonadFail m => MonadFail (GhcT m) where
	fail = lift . Fail.fail

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
#if MIN_VERSION_exceptions(0,10,0)
	generalBracket acq rel act = GhcT $ \r -> generalBracket
		(unGhcT acq r)
		(\res exitCase -> case exitCase of
			ExitCaseSuccess v -> unGhcT (rel res (ExitCaseSuccess v)) r
			ExitCaseException e -> unGhcT (rel res (ExitCaseException e)) r
			ExitCaseAbort -> unGhcT (rel res ExitCaseAbort) r)
		(\res -> unGhcT (act res) r)
#elif MIN_VERSION_exceptions(0,9,0)
	generalBracket acq rel clean act = GhcT $ \r -> generalBracket
		(unGhcT acq r)
		(\res -> unGhcT (rel res) r)
		(\res e -> unGhcT (clean res e) r)
		(\res -> unGhcT (act res) r)
#endif

-- | Run multi-session ghc
runMGhcT :: (MonadIO m, ExceptionMonad m, Ord s, Monoid d) => Maybe FilePath -> MGhcT s d m a -> m a
runMGhcT lib act = do
	ref <- liftIO $ newIORef (panic "empty session")
	let
		session = GhcMonad.Session ref
	flip evalStateT Def.def $ flip runReaderT lib $ flip unGhcT session $ unMGhcT $ act `gfinally` cleanup
	where
		cleanup :: (MonadIO m, ExceptionMonad m, Ord s, Monoid d) => MGhcT s d m ()
		cleanup = do
			void saveSession
			sessions <- gets (M.elems . view sessionMap)
			liftIO $ mapM_ (cleanupSession . view _1) sessions
			modify (set sessionMap M.empty)

-- | Lift `Ghc` monad onto `MGhc`
liftGhc :: MonadIO m => Ghc a -> MGhcT s d m a
liftGhc (Ghc act) = MGhcT $ GhcT $ liftIO . act

-- | Get current session
currentSession :: MonadIO m => MGhcT s d m (Maybe (Session s d))
currentSession = gets (view sessionActive)

-- | Get current session data
getSessionData :: MonadIO m => MGhcT s d m (Maybe d)
getSessionData = gets (preview (sessionActive . _Just . sessionData))

-- | Set current session data
setSessionData :: MonadIO m => d -> MGhcT s d m ()
setSessionData sdata = modify (set (sessionActive . _Just . sessionData) sdata)

-- | Does session exist
hasSession :: (MonadIO m, Ord s) => s -> MGhcT s d m Bool
hasSession key = do
	msess <- findSession key
	return $ isJust msess

-- | Find session
findSession :: (MonadIO m, Ord s) => s -> MGhcT s d m (Maybe (Session s d))
findSession key = do
	sdata <- gets (preview (sessionMap . ix key . _2))
	return $ fmap (Session key) sdata

-- | Find session by
findSessionBy :: MonadIO m => (s -> Bool) -> MGhcT s d m [Session s d]
findSessionBy p = do
	sessions <- gets (M.toList . view sessionMap)
	return [Session key sdata | (key, (_, sdata)) <- sessions, p key]

-- | Save current session
saveSession :: (MonadIO m, ExceptionMonad m, Ord s) => MGhcT s d m (Maybe (Session s d))
saveSession = do
	msess <- currentSession
	case msess of
		Just (Session key' dat') -> do
			sess <- getSession
			modify (set (sessionMap . at key') (Just (sess, dat')))
		Nothing -> return ()
	return msess

-- | Initialize new session
initSession :: (MonadIO m, ExceptionMonad m, Ord s) => MGhcT s d m ()
initSession = do
	lib <- ask
	initGhcMonad lib
	void saveSession

activateSession :: (MonadIO m, ExceptionMonad m, Ord s, Monoid d) => s -> MGhcT s d m (Maybe HscEnv)
activateSession key = do
	void saveSession
	sdata <- gets (view (sessionMap . ix key . _2))
	modify (set sessionActive $ Just (Session key sdata))
	gets (preview (sessionMap . ix key . _1))

-- | Create new named session, deleting existing session
newSession :: (MonadIO m, ExceptionMonad m, Ord s, Monoid d) => s -> MGhcT s d m ()
newSession key = do
	msess <- activateSession key
	maybe (return ()) (liftIO . cleanupSession) msess
	initSession

-- | Switch to session, creating if not exist, returns True if session was created
switchSession :: (MonadIO m, ExceptionMonad m, Ord s, Monoid d) => s -> MGhcT s d m Bool
switchSession key = do
	msess <- activateSession key
	case msess of
		Nothing -> initSession >> return True
		Just sess -> setSession sess >> return False

-- | Switch to session, creating if not exist and initializing with passed function
switchSession_ :: (MonadIO m, ExceptionMonad m, Ord s, Monoid d) => s -> Maybe (MGhcT s d m ()) -> MGhcT s d m ()
switchSession_ key f = do
	new <- switchSession key
	when new $ fromMaybe (return ()) f

-- | Delete existing session
deleteSession :: (MonadIO m, ExceptionMonad m, Ord s, Monoid d) => s -> MGhcT s d m ()
deleteSession key = do
	cur <- saveSession
	when (preview (_Just . sessionKey) cur == Just key) $
		modify (set sessionActive Nothing)
	msess <- gets (preview (sessionMap . ix key . _1))
	modify (set (sessionMap . at key) Nothing)
	case msess of
		Nothing -> return ()
		Just sess -> liftIO $ cleanupSession sess

-- | Save and restore session
restoreSession :: (MonadIO m, MonadMask m, ExceptionMonad m, Ord s, Monoid d) => MGhcT s d m a -> MGhcT s d m a
restoreSession act = bracket saveSession (maybe (return ()) (void . switchSession . view sessionKey)) $ const act

-- | Run action using session, restoring session back
usingSession :: (MonadIO m, MonadMask m, ExceptionMonad m, Ord s, Monoid d) => s -> MGhcT s d m a -> MGhcT s d m a
usingSession key act = restoreSession $ do
	void $ switchSession key
	act

-- | Run with temporary session, like @usingSession@, but deletes self session
tempSession :: (MonadIO m, MonadMask m, ExceptionMonad m, Ord s, Monoid d) => s -> MGhcT s d m a -> MGhcT s d m a
tempSession key act = do
	exist' <- hasSession key
	usingSession key act `finally` unless exist' (deleteSession key)

-- | Cleanup session
cleanupSession :: HscEnv -> IO ()
cleanupSession env = do
	cleanTemps df
	stopIServ env
	where
		df = hsc_dflags env
