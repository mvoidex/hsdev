module System.Win32.FileMapping.Memory (
	createMap, openMap, mapFile,
	withMapFile, readMapFile
	) where

import Control.Monad.CatchIO (bracket)
import Control.Monad.Cont
import Control.Monad.Error
import Foreign.C.String
import Foreign.Ptr
import System.Win32.File (closeHandle)
import System.Win32.FileMapping hiding (mapFile)
import System.Win32.Types
import System.Win32.Mem

createMap :: Maybe HANDLE -> ProtectFlags -> DDWORD -> Maybe String -> ContT r (ErrorT String IO) HANDLE
createMap mh pf sz mn = ContT $ bracket
	(verify iNVALID_HANDLE_VALUE "Invalid handle" $
		liftIO (createFileMapping mh pf sz mn))
	(liftIO . closeHandle)

openMap :: FileMapAccess -> Bool -> Maybe String -> ContT r (ErrorT String IO) HANDLE
openMap f i mn = ContT $ bracket
	(verify iNVALID_HANDLE_VALUE "Invalid handle" $
		liftIO (openFileMapping f i mn))
	(liftIO . closeHandle)

mapFile :: HANDLE -> FileMapAccess -> DDWORD -> SIZE_T -> ErrorT String IO (Ptr a)
mapFile h f off sz = verify nullPtr "null pointer" $ liftIO (mapViewOfFile h f off sz) 

-- | Write data to named map view of file
withMapFile :: String -> String -> IO a -> ErrorT String IO a
withMapFile name str act = flip runContT return $ do
	p <- ContT $ \f -> ErrorT (withCString str (runErrorT . f))
	h <- createMap Nothing pAGE_READWRITE (fromIntegral len) (Just name)
	ptr <- lift $ mapFile h fILE_MAP_ALL_ACCESS 0 0
	liftIO $ do
		copyMemory ptr p (fromIntegral len)
		unmapViewOfFile ptr
		act
	where
		len = length str + 1

-- | Read data from named map view of file
readMapFile :: String -> ErrorT String IO String
readMapFile name = flip runContT return $ do
	h <- openMap fILE_MAP_ALL_ACCESS True (Just name)
	ptr <- lift $ mapFile h fILE_MAP_ALL_ACCESS 0 0
	s <- liftIO $ peekCString ptr
	length s `seq` return s

verify :: (Error e, MonadError e m, Eq a) => a -> String -> m a -> m a
verify v str act = do
	x <- act
	if x == v
		then throwError (strMsg str)
		else return x
