module System.Win32.FileMapping.Memory (
	createMap, openMap, mapFile,
	withMapFile, readMapFile
	) where

import Control.Arrow (left)
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

createMap :: Maybe HANDLE -> ProtectFlags -> DDWORD -> Maybe String -> ContT r IO HANDLE
createMap mh pf sz mn = ContT $ bracket
	(verify iNVALID_HANDLE_VALUE "Invalid handle" $
		createFileMapping mh pf sz mn)
	closeHandle

openMap :: FileMapAccess -> Bool -> Maybe String -> ContT r IO HANDLE
openMap f i mn = ContT $ bracket
	(verify iNVALID_HANDLE_VALUE "Invalid handle" $
		openFileMapping f i mn)
	closeHandle

mapFile :: HANDLE -> FileMapAccess -> DDWORD -> SIZE_T -> IO (Ptr a)
mapFile h f off sz = verify nullPtr "null pointer" $ mapViewOfFile h f off sz

-- | Write data to named map view of file
withMapFile :: String -> ByteString -> IO a -> ExceptT String IO a
withMapFile name str act = liftE $ flip runContT return $ do
	p <- ContT $ BS.useAsCString str
	h <- createMap Nothing pAGE_READWRITE (fromIntegral len) (Just name)
	ptr <- lift $ mapFile h fILE_MAP_ALL_ACCESS 0 0
	liftIO $ do
		copyMemory ptr p (fromIntegral len)
		unmapViewOfFile ptr
		act
	where
		len = BS.length str + 1

-- | Read data from named map view of file
readMapFile :: String -> ExceptT String IO ByteString
readMapFile name = liftE $ flip runContT return $ do
	h <- openMap fILE_MAP_ALL_ACCESS True (Just name)
	ptr <- lift $ mapFile h fILE_MAP_ALL_ACCESS 0 0
	liftIO $ BS.packCString ptr

verify :: Eq a => a -> String -> IO a -> IO a
verify v str act = do
	x <- act
	if x == v
		then ioError (userError str)
		else return x

liftE :: MonadCatch m => m a -> ExceptT String m a
liftE = ExceptT . liftM (left (\(SomeException e) -> show e)) . try
