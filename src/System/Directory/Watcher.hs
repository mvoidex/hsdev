{-# LANGUAGE PatternGuards, TemplateHaskell #-}

module System.Directory.Watcher (
	EventType(..), Event(..), eventType, eventPath, eventTime,
	Watcher(..),
	withWatcher,
	watchDir, watchDir_, unwatchDir, isWatchingDir,
	watchTree, watchTree_, unwatchTree, isWatchingTree,
	-- * Working with events
	readEvent, events, mergeEvents, onEvent, onEvent_,
	-- * Internal
	mergeConsumeEvent
	) where

import Control.Lens (makeLenses)
import Control.Arrow
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (isJust, isNothing, maybeToList)
import Data.Ratio ((%))
import Data.String (fromString)
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX
import System.FilePath (takeDirectory, isDrive)
import System.Directory
import qualified System.FSNotify as FS

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
	return $ isWatchingDir' dirs f' || isWatchingParents' dirs f'

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
	return $ isWatchingTree' dirs f' || isWatchingParents' dirs f'

-- | Read next event
readEvent :: Watcher a -> IO (a, Event)
readEvent = readChan . watcherChan

-- | Get lazy list of events
events :: Watcher a -> IO [(a, Event)]
events = getChanContents . watcherChan

-- | Merge same events with slightly different time
mergeEvents :: NominalDiffTime -> [(a, Event)] -> [(a, Event)]
mergeEvents tolerance = uncurry concat' . flip runState (M.empty, M.empty) . mapM (mergeConsumeEvent tolerance) where
	concat' :: [[(a, Event)]] -> (Map (EventType, FilePath) (POSIXTime, POSIXTime), Map POSIXTime (a, Event)) -> [(a, Event)]
	concat' evs ~(_, queueTail) = concat evs ++ M.elems queueTail

mergeConsumeEvent :: NominalDiffTime -> (a, Event) -> State (Map (EventType, FilePath) (POSIXTime, POSIXTime), Map POSIXTime (a, Event)) [(a, Event)]
mergeConsumeEvent tolerance (v, e) = do
	(events', queue') <- get
	let
		needMerge = isNothing $ do
			(pushTime', _) <- M.lookup (eventKey e) events'
			guard (_eventTime e - pushTime' >= tolerance)
		pushTime = maybe (_eventTime e) fst $ M.lookup (eventKey e) events'
		newPushTime
			| needMerge = pushTime
			| otherwise = _eventTime e

		insertEvent = M.insert (eventKey e) (newPushTime, _eventTime e)
		insertTime = M.insert newPushTime (v, e)
		(old, bound, new) = M.splitLookup (_eventTime e - tolerance) (insertTime queue')
		removeOld = flip M.restrictKeys (S.fromList $ map (eventKey . snd) $ M.elems new)
	put (insertEvent $ removeOld events', new)
	return $ M.elems old ++ maybeToList bound
	where
		eventKey ev = (_eventType ev, _eventPath ev)

-- | Process all events
onEvent :: Watcher a -> NominalDiffTime -> (a -> Event -> IO ()) -> IO ()
onEvent w tolerance act = events w >>= mapM_ (uncurry act) . mergeEvents tolerance

-- | Process all events
onEvent_ :: Watcher a -> (a -> Event -> IO ()) -> IO ()
onEvent_ w = onEvent w (fromRational (1 % 5))

fromEvent :: FS.Event -> Event
fromEvent e = Event t (FS.eventPath e) (utcTimeToPOSIXSeconds $ FS.eventTime e) where
	t = case e of
		FS.Added _ _ -> Added
		FS.Modified _ _ -> Modified
		FS.Removed _ _ -> Removed

isWatchingDir' :: Map FilePath (Bool, IO ()) -> FilePath -> Bool
isWatchingDir' m dir
	| Just (_, _) <- M.lookup dir m = True
	| isDrive dir = False
	| otherwise = isWatchingDir' m (takeDirectory dir)

isWatchingTree' :: Map FilePath (Bool, IO ()) -> FilePath -> Bool
isWatchingTree' m dir
	| Just (True, _) <- M.lookup dir m = True
	| isDrive dir = False
	| otherwise = isWatchingTree' m (takeDirectory dir)

isWatchingParents' :: Map FilePath (Bool, IO ()) -> FilePath -> Bool
isWatchingParents' m dir = or (map (isWatchingTree' m) parents) where
	parents = takeWhile (not . isDrive) $ iterate takeDirectory dir
