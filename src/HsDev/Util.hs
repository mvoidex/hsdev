module HsDev.Util (
	directoryContents,
	traverseDirectory,
	isParent,
	haskellSource,
	cabalFile,
	-- * String utils
	tab, tabs, trim, split,
	-- * Helper
	(.::),
	-- * Exceptions
	liftException, liftExceptionM, liftIOErrors,
	eitherT,
	-- * UTF-8
	fromUtf8, toUtf8
	) where

import Control.Arrow (second)
import Control.Exception
import Control.Monad
import Control.Monad.Error
import qualified Control.Monad.CatchIO as C
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Char (isSpace)
import Data.List (isPrefixOf, unfoldr)
import qualified Data.HashMap.Strict as HM (HashMap, toList)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import System.Directory
import System.FilePath

-- | Get directory contents safely
directoryContents :: FilePath -> IO [FilePath]
directoryContents p = handle ignore $ do
	b <- doesDirectoryExist p
	if b
		then liftM (map (p </>) . filter (`notElem` [".", ".."])) (getDirectoryContents p)
		else return []
	where
		ignore :: SomeException -> IO [FilePath]
		ignore _ = return []

-- | Collect all file names in directory recursively
traverseDirectory :: FilePath -> IO [FilePath]
traverseDirectory path = handle onError $ do
	cts <- directoryContents path
	liftM concat $ forM cts $ \c -> do
		isDir <- doesDirectoryExist c
		if isDir
			then traverseDirectory c
			else return [c]
	where
		onError :: IOException -> IO [FilePath]
		onError _ = return []

-- | Is one path parent of another
isParent :: FilePath -> FilePath -> Bool
isParent dir file = norm dir `isPrefixOf` norm file where
	norm = splitDirectories . normalise

-- | Is haskell source?
haskellSource :: FilePath -> Bool
haskellSource f = takeExtension f `elem` [".hs", ".lhs"]

-- | Is cabal file?
cabalFile :: FilePath -> Bool
cabalFile f = takeExtension f == ".cabal"

-- | Add N tabs to line
tab :: Int -> String -> String
tab n s = replicate n '\t' ++ s

-- | Add N tabs to multiline
tabs :: Int -> String -> String
tabs n = unlines . map (tab n) . lines

-- | Trim string
trim :: String -> String
trim = p . p where
	p = reverse . dropWhile isSpace

-- | Split list
split :: (a -> Bool) -> [a] -> [[a]]
split p = takeWhile (not . null) . unfoldr (Just . second (drop 1) . break p)

-- | Workaround, sometimes we get HM.lookup "foo" v == Nothing, but lookup "foo" (HM.toList v) == Just smth
(.::) :: FromJSON a => HM.HashMap Text Value -> Text -> Parser a
v .:: name = maybe (fail $ "key " ++ show name ++ " not present") parseJSON $ lookup name $ HM.toList v

-- | Lift IO exception to ErrorT
liftException :: C.MonadCatchIO m => m a -> ErrorT String m a
liftException act = ErrorT $ C.catch (liftM Right act) onError where
	onError = return . Left . (show :: SomeException -> String)

-- | Lift IO exception to MonadError
liftExceptionM :: (C.MonadCatchIO m, Error e, MonadError e m) => m a -> m a
liftExceptionM act = C.catch act onError where
	onError = throwError . strMsg . (show :: SomeException -> String)

-- | Lift IO exceptions to ErrorT
liftIOErrors :: C.MonadCatchIO m => ErrorT String m a -> ErrorT String m a
liftIOErrors act = liftException (runErrorT act) >>= either throwError return

eitherT :: (Monad m, Error e, MonadError e m) => Either String a -> m a
eitherT = either (throwError . strMsg) return

fromUtf8 :: ByteString -> String
fromUtf8 = T.unpack . T.decodeUtf8

toUtf8 :: String -> ByteString
toUtf8 = T.encodeUtf8 . T.pack
