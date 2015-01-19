module HsDev.Util (
	withCurrentDirectory,
	directoryContents,
	traverseDirectory, searchPath,
	isParent,
	haskellSource,
	cabalFile,
	-- * String utils
	tab, tabs, trim, split,
	-- * Helper
	(.::), (.::?), objectUnion,
	-- * Exceptions
	liftException, liftE, liftEIO, tries, triesMap, liftExceptionM, liftIOErrors,
	eitherT,
	liftThrow,
	-- * UTF-8
	fromUtf8, toUtf8, readFileUtf8, writeFileUtf8,
	-- * IO
	hGetLineBS, logException, logIO, ignoreIO,
	-- * Task
	liftTask
	) where

import Control.Applicative
import Control.Arrow (second, left)
import Control.Exception
import Control.Monad
import Control.Monad.Error
import qualified Control.Monad.Catch as C
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Char (isSpace)
import Data.List (isPrefixOf, unfoldr)
import Data.Maybe (catMaybes)
import qualified Data.HashMap.Strict as HM (HashMap, toList, union)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Text (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import Data.Traversable (traverse)
import System.Directory
import System.FilePath
import System.IO

import Control.Concurrent.Task

-- | Run action with current directory set
withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory cur act = bracket getCurrentDirectory setCurrentDirectory $
	const (setCurrentDirectory cur >> act)

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
			then (c :) <$> traverseDirectory c
			else return [c]
	where
		onError :: IOException -> IO [FilePath]
		onError _ = return []

-- | Search something up
searchPath :: (MonadIO m, MonadPlus m) => FilePath -> (FilePath -> m a) -> m a
searchPath path f = do
	path' <- liftIO $ canonicalizePath path
	isDir <- liftIO $ doesDirectoryExist path'
	search' (if isDir then path' else takeDirectory path')
	where
		search' dir
			| isDrive dir = f dir
			| otherwise = f dir `mplus` search' (takeDirectory dir)

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

(.::?) :: FromJSON a => HM.HashMap Text Value -> Text -> Parser (Maybe a)
v .::? name = traverse parseJSON $ lookup name $ HM.toList v

-- | Union two JSON objects
objectUnion :: Value -> Value -> Value
objectUnion (Object l) (Object r) = Object $ HM.union l r
objectUnion (Object l) _ = Object l
objectUnion _ (Object r) = Object r
objectUnion _ _ = Null

-- | Lift IO exception to ErrorT
liftException :: C.MonadCatch m => m a -> ErrorT String m a
liftException = ErrorT . liftM (left $ \(SomeException e) -> show e) . C.try

-- | Same as @liftException@
liftE :: C.MonadCatch m => m a -> ErrorT String m a
liftE = liftException

-- | @liftE@ for IO
liftEIO :: (C.MonadCatch m, MonadIO m) => IO a -> ErrorT String m a
liftEIO = liftE . liftIO

-- | Run actions ignoring errors
tries :: MonadPlus m => [m a] -> m [a]
tries acts = liftM catMaybes $ sequence [liftM Just act `mplus` return Nothing | act <- acts]

triesMap :: MonadPlus m => (a -> m b) -> [a] -> m [b]
triesMap f = tries . map f

-- | Lift IO exception to MonadError
liftExceptionM :: (C.MonadCatch m, Error e, MonadError e m) => m a -> m a
liftExceptionM act = C.catch act onError where
	onError = throwError . strMsg . (\(SomeException e) -> show e)

-- | Lift IO exceptions to ErrorT
liftIOErrors :: C.MonadCatch m => ErrorT String m a -> ErrorT String m a
liftIOErrors act = liftException (runErrorT act) >>= either throwError return

eitherT :: (Monad m, Error e, MonadError e m) => Either String a -> m a
eitherT = either (throwError . strMsg) return

-- | Throw error as exception
liftThrow :: (Show e, Error e, MonadError e m, C.MonadCatch m) => m a -> m a
liftThrow act = catchError act (C.throwM . userError . show)

fromUtf8 :: ByteString -> String
fromUtf8 = T.unpack . T.decodeUtf8

toUtf8 :: String -> ByteString
toUtf8 = T.encodeUtf8 . T.pack

-- | Read file in UTF8
readFileUtf8 :: FilePath -> IO String
readFileUtf8 f = withFile f ReadMode $ \h -> do
	hSetEncoding h utf8
	cts <- hGetContents h
	length cts `seq` return cts

writeFileUtf8 :: FilePath -> String -> IO ()
writeFileUtf8 f cts = withFile f WriteMode $ \h -> do
	hSetEncoding h utf8
	hPutStr h cts

hGetLineBS :: Handle -> IO ByteString
hGetLineBS = fmap L.fromStrict . B.hGetLine

logException :: String -> (String -> IO ()) -> IO () -> IO ()
logException pre out = handle onErr where
	onErr :: SomeException -> IO ()
	onErr e = out $ pre ++ show e

logIO :: String -> (String -> IO ()) -> IO () -> IO ()
logIO pre out = handle onIO where
	onIO :: IOException -> IO ()
	onIO e = out $ pre ++ show e

ignoreIO :: IO () -> IO ()
ignoreIO = handle (const (return ()) :: IOException -> IO ())

liftTask :: (C.MonadThrow m, C.MonadCatch m, MonadIO m) => IO (Task a) -> ErrorT String m a
liftTask = liftExceptionM . ErrorT . liftIO . liftM (left show) . join . liftM taskWait
