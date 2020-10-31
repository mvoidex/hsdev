{-# LANGUAGE PatternGuards, OverloadedStrings, FlexibleContexts, PackageImports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Tools.Ghc.Worker (
	-- * Workers
	SessionType(..), SessionConfig(..),
	GhcM, GhcWorker, MGhcT(..), runGhcM,
	ghcWorker,
	workerSession, ghcSession, ghciSession, haddockSession, tmpSession,

	Ghc,
	LogT(..),

	module HsDev.Tools.Ghc.Base,
	module HsDev.Tools.Ghc.Repl,
	module HsDev.Tools.Ghc.MGhc,
	module Control.Concurrent.Worker
	) where

import Control.Lens (view)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Catch
import Data.Monoid
import qualified System.Log.Simple as Log
import System.Log.Simple.Monad (MonadLog(..), LogT(..), withLog)
import Text.Format hiding (withFlags)

import "ghc" Exception (ExceptionMonad(..), ghandle)
import "ghc" GHC hiding (Warning, Module)
import "ghc" Linker (initDynLinker)
import GHC.Paths

import Control.Concurrent.Worker
import HsDev.PackageDb.Types
import HsDev.Tools.Ghc.Base
import HsDev.Tools.Ghc.Repl
import HsDev.Tools.Ghc.MGhc

data SessionType = SessionGhci | SessionGhc | SessionHaddock | SessionTmp deriving (Eq, Ord)
data SessionConfig = SessionConfig SessionType PackageDbStack deriving (Eq, Ord)

instance Show SessionType where
	show SessionGhci = "ghci"
	show SessionGhc = "ghc"
	show SessionHaddock = "haddock"
	show SessionTmp = "tmp"

instance Formattable SessionType

instance Show SessionConfig where
	show (SessionConfig t pdb) = "{} {}" ~~ t ~~ pdb

instance Formattable SessionConfig

type GhcM a = MGhcT SessionConfig (First DynFlags) (LogT IO) a

type GhcWorker = Worker (MGhcT SessionConfig (First DynFlags) (LogT IO))

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
	liftIO $ startWorker (withLog l . runGhcM (Just libdir)) (Log.scope "ghc") (ghandle logErr)
	where
		logErr :: MonadLog m => SomeException -> m ()
		logErr e = Log.sendLog Log.Warning ("exception in ghc worker task: {}" ~~ displayException e)

-- | Create session with options
workerSession :: SessionType -> PackageDbStack -> [String] -> GhcM ()
workerSession ty pdbs opts = do
	ms <- findSessionBy toKill
	forM_ ms $ \s' -> do
		Log.sendLog Log.Trace $ "killing session: {}" ~~ view sessionKey s'
		deleteSession $ view sessionKey s'
	Log.sendLog Log.Trace $ "session: {}" ~~ SessionConfig ty pdbs
	switchSession_ (SessionConfig ty pdbs) $ Just initialize
	setSessionFlags
	Log.sendLog Log.Trace "session set"
	where
		toKill (SessionConfig ty' pdbs') = or [
			(ty == ty' && pdbs /= pdbs'),
			(ty /= ty' && ty' `elem` [SessionTmp, SessionHaddock] && ty /= SessionTmp)]
		initialize = do
			run
			dflags <- getSessionDynFlags
			setSessionData (First $ Just dflags)
		run = case ty of
			SessionGhci -> ghcRun pdbsOpts (importModules preludeModules)
			SessionGhc -> ghcRun pdbsOpts (getSession >>= liftIO . initDynLinker)
			SessionTmp -> ghcRun pdbsOpts (getSession >>= liftIO . initDynLinker)
			SessionHaddock -> ghcRunWith noLinkFlags ("-haddock" : pdbsOpts) (return ())
		setSessionFlags = do
			Log.sendLog Log.Trace $ "setting flags: {}" ~~ unwords opts
			mdflags <- fmap (join . fmap getFirst) getSessionData
			dflags <- maybe getSessionDynFlags return mdflags
			(df', _, _) <- parseDynamicFlags dflags (map noLoc opts)
			void $ setSessionDynFlags df'
		pdbsOpts = packageDbStackOpts pdbs

-- | Get ghc session
ghcSession :: PackageDbStack -> [String] -> GhcM ()
ghcSession = workerSession SessionGhc

-- | Get ghci session
ghciSession :: GhcM ()
ghciSession = workerSession SessionGhci userDb []

-- | Get haddock session with flags
haddockSession :: PackageDbStack -> [String] -> GhcM ()
haddockSession = workerSession SessionHaddock

-- | Get haddock session with flags
tmpSession :: PackageDbStack -> [String] -> GhcM ()
tmpSession = workerSession SessionTmp
