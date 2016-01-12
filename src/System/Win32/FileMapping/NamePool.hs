module System.Win32.FileMapping.NamePool (
	Pool(..),
	createPool, withName
	) where

import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad (liftM, liftM2)
import System.Win32.FileMapping.Memory ()

-- | Pool of names for memory mapped files
data Pool = Pool {
	poolFreeNames :: MVar [String],
	poolNewName :: IO String }

-- | Create pool of numbered names by base name
createPool :: String -> IO Pool
createPool baseName = liftM2 Pool (newMVar []) mkNewName where
	mkNewName :: IO (IO String)
	mkNewName = do
		num <- newMVar (0 :: Integer)
		return $ modifyMVar num $ \n -> do
			return (succ n, baseName ++ show n)

-- | Use free name from pool
withName :: Pool -> (String -> IO a) -> IO a
withName p = bracket getName freeName where
	getName = modifyMVar (poolFreeNames p) $ \names -> case names of
		[] -> liftM ((,) []) $ poolNewName p
		(n:ns) -> return (ns, n)
	freeName name = modifyMVar_ (poolFreeNames p) (return . (name:))
