module System.Win32.FileMapping.Memory (
	createMap, openMap, mapFile,
	withMapFile, readMapFile
	) where

import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.Cont
import Control.Monad.Except
import Data.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Foreign.Ptr
import System.Win32.File (closeHandle)
import System.Win32.FileMapping hiding (mapFile)
import System.Win32.Types
import System.Win32.Mem

createMap :: Maybe HANDLE -> ProtectFlags -> DDWORD -> Maybe String -> ExceptT String (ContT r IO) HANDLE
createMap mh pf sz mn = verify (/= iNVALID_HANDLE_VALUE) "Invalid handle" $ ContT $ bracket
	(createFileMapping mh pf sz mn)
	closeHandle

openMap :: FileMapAccess -> Bool -> Maybe String -> ExceptT String (ContT r IO) HANDLE
openMap f i mn = verify (/= iNVALID_HANDLE_VALUE) "Invalid handle" $ ContT $ bracket
	(openFileMapping f i mn)
	closeHandle

mapFile :: HANDLE -> FileMapAccess -> DDWORD -> SIZE_T -> ExceptT String IO (Ptr a)
mapFile h f off sz = verify (/= nullPtr) "null pointer" $ mapViewOfFile h f off sz

-- | Write data to named map view of file
withMapFile :: String -> ByteString -> IO a -> ExceptT String IO a
withMapFile name str act = mapExceptT (`runContT` return) $ do
	p <- lift $ ContT $ BS.useAsCString str
	h <- createMap Nothing pAGE_READWRITE (fromIntegral len) (Just name)
	ptr <- mapExceptT lift $ mapFile h fILE_MAP_ALL_ACCESS 0 0
	liftIO $ do
		copyMemory ptr p (fromIntegral len)
		unmapViewOfFile ptr
		act
	where
		len = BS.length str + 1

-- | Read data from named map view of file
readMapFile :: String -> ExceptT String IO ByteString
readMapFile name = mapExceptT (`runContT` return) $ do
	h <- openMap fILE_MAP_ALL_ACCESS True (Just name)
	ptr <- mapExceptT lift $ mapFile h fILE_MAP_ALL_ACCESS 0 0
	liftIO $ BS.packCString ptr

verify :: (Eq a, Monad m) => (a -> Bool) -> String -> m a -> ExceptT String m a
verify p str act = do
	x <- lift act
	if p x then return x else throwError str
