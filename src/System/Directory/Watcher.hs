{-# LANGUAGE PatternGuards, TemplateHaskell #-}

module System.Directory.Watcher (
	EventType(..), Event(..), eventType, eventPath, eventTime,
	Watcher(..),
	withWatcher,
	watchDir, watchDir_, unwatchDir, isWatchingDir,
	watchTree, watchTree_, unwatchTree, isWatchingTree,
	-- * Working with events
	readEvent, eventGroup, onEvents, onEvents_
	) where

import Control.Lens (makeLenses)
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Ratio ((%))
import Data.String (fromString)
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX
import System.FilePath (takeDirectory, isDrive)
import System.Directory
import qualified System.FSNotify as FS

import HsDev.Util (uniqueBy)

-- | Event type
data EventType = Added | Modified | Removed deriving (Eq, Ord, Enum, Bounded, Read, Show)

-- | Event
data Event = Event {
	_eventType :: EventType,
	_eventPath :: FilePath,
	_eventTime :: POSIXTime }
		deriving (Eq, Ord, Show)

makeLenses ''Event

-- | Directories watcher
data Watcher a = Watcher {
	-- | Map from directory to watch stopper
	watcherDirs :: MVar (Map FilePath (Bool, IO ())),
	watcherMan :: FS.WatchManager,
	watcherChan :: Chan (a, Event) }

-- | Create watcher
withWatcher :: (Watcher a -> IO b) -> IO b
withWatcher act = FS.withManager $ \man -> do
	ch <- newChan
	dirs <- newMVar M.empty
	act $ Watcher dirs man ch

-- | Watch directory
watchDir :: Watcher a -> FilePath -> (Event -> Bool) -> a -> IO ()
watchDir w f p v = do
	e <- doesDirectoryExist f
	when e $ do
		f' <- canonicalizePath f
		watching <- isWatchingDir w f'
		unless watching $ do
			stop <- FS.watchDir
				(watcherMan w)
				(fromString f')
				(p . fromEvent)
				(writeChan (watcherChan w) . (,) v . fromEvent)
			modifyMVar_ (watcherDirs w) $ return . M.insert f' (False, stop)

watchDir_ :: Watcher () -> FilePath -> (Event -> Bool) -> IO ()
watchDir_ w f p = watchDir w f p ()

-- | Unwatch directory, return @False@, if not watched
unwatchDir :: Watcher a -> FilePath -> IO Bool
unwatchDir w f = do
	f' <- canonicalizePath f
	stop <- modifyMVar (watcherDirs w) $ return . (M.delete f' &&& M.lookup f')
	maybe (return ()) snd stop
	return $ isJust stop

-- | Check if we are watching dir
isWatchingDir :: Watcher a -> FilePath -> IO Bool
isWatchingDir w f = do
	f' <- canonicalizePath f
	dirs <- readMVar (watcherDirs w)
	return $ isWatchingDir' dirs f'

-- | Watch directory tree
watchTree :: Watcher a -> FilePath -> (Event -> Bool) -> a -> IO ()
watchTree w f p v = do
	e <- doesDirectoryExist f
	when e $ do
		f' <- canonicalizePath f
		watching <- isWatchingTree w f'
		unless watching $ do
			stop <- FS.watchTree
				(watcherMan w)
				(fromString f')
				(p . fromEvent)
				(writeChan (watcherChan w) . (,) v . fromEvent)
			modifyMVar_ (watcherDirs w) $ return . M.insert f' (True, stop)

watchTree_ :: Watcher () -> FilePath -> (Event -> Bool) -> IO ()
watchTree_ w f p = watchTree w f p ()

-- | Unwatch directory tree
unwatchTree :: Watcher a -> FilePath -> IO Bool
unwatchTree w f = do
	f' <- canonicalizePath f
	stop <- modifyMVar (watcherDirs w) $ return . (M.delete f' &&& M.lookup f')
	maybe (return ()) snd stop
	return $ isJust stop

-- | Check if we are watching tree
isWatchingTree :: Watcher a -> FilePath -> IO Bool
isWatchingTree w f = do
	f' <- canonicalizePath f
	dirs <- readMVar (watcherDirs w)
	return $ isWatchingTree' dirs f'

-- | Read next event
readEvent :: Watcher a -> IO (a, Event)
readEvent = readChan . watcherChan

-- | Get event group
eventGroup :: Watcher a -> NominalDiffTime -> ([(a, Event)] -> IO ()) -> IO ()
eventGroup w tm onGroup = do
	groupVar <- newTMVarIO []
	syncVar <- newEmptyTMVarIO
	_ <- async $ forever $ do
		ev <- readChan (watcherChan w)
		_ <- atomically $ tryPutTMVar syncVar ()
		atomically $ do
			evs <- takeTMVar groupVar
			putTMVar groupVar (ev : evs)
	forever $ do
		_ <- atomically $ takeTMVar syncVar
		threadDelay $ floor (tm * 1e6)
		evs' <- atomically $ do
			evs <- takeTMVar groupVar
			putTMVar groupVar []
			_ <- tryTakeTMVar syncVar
			return $ reverse evs
		onGroup $ uniqueBy (\(_, ev') -> (_eventType ev', _eventPath ev')) evs'

-- | Process all events
onEvents :: Watcher a -> NominalDiffTime -> ([(a, Event)] -> IO ()) -> IO ()
onEvents = eventGroup

-- | Process all events
onEvents_ :: Watcher a -> ([(a, Event)] -> IO ()) -> IO ()
onEvents_ w = onEvents w (fromRational (1 % 5))

fromEvent :: FS.Event -> Event
fromEvent e = Event t (FS.eventPath e) (utcTimeToPOSIXSeconds $ FS.eventTime e) where
	t = case e of
		FS.Added{} -> Added
		FS.Modified{} -> Modified
		FS.Removed{} -> Removed

isWatchingDir' :: Map FilePath (Bool, IO ()) -> FilePath -> Bool
isWatchingDir' m dir
	| Just (_, _) <- M.lookup dir m = True
	| isDrive dir = False
	| otherwise = isWatchingTree' m (takeDirectory dir)

isWatchingTree' :: Map FilePath (Bool, IO ()) -> FilePath -> Bool
isWatchingTree' m dir
	| Just (True, _) <- M.lookup dir m = True
	| isDrive dir = False
	| otherwise = isWatchingTree' m (takeDirectory dir)
