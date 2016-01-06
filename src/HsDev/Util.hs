{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module HsDev.Util (
	withCurrentDirectory,
	directoryContents,
	traverseDirectory, searchPath,
	isParent,
	haskellSource,
	cabalFile,
	-- * String utils
	tab, tabs, trim, split,
	-- * Other utils
	ordNub, uniqueBy, mapBy,
	-- * Helper
	(.::), (.::?), objectUnion, jsonUnion,
	-- * Exceptions
	liftException, liftE, liftEIO, tries, triesMap, liftExceptionM, liftIOErrors,
	eitherT,
	liftThrow,
	-- * UTF-8
	fromUtf8, toUtf8, readFileUtf8, writeFileUtf8,
	-- * IO
	hGetLineBS, logException, logIO, ignoreIO, logAsync,
	-- * Async
	liftAsync,
	-- * Command line
	FromCmd(..),
	cmdJson, withCmd, guardCmd,
	withHelp, cmd, parseArgs,

	-- * Reexportss
	module Control.Monad.Except,
	MonadIO(..)
	) where

import Control.Arrow (second, left, (&&&))
import Control.Exception
import Control.Monad
import Control.Monad.Except
import qualified Control.Monad.Catch as C
import Data.Aeson hiding (Result(..), Error)
import qualified Data.Aeson.Types as A
import Data.Char (isSpace)
import Data.List (isPrefixOf, unfoldr)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HM (HashMap, toList, union)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Text (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import Options.Applicative
import System.Directory
import System.FilePath
import System.IO

import Control.Concurrent.Async

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

-- | nub is quadratic, https://github.com/nh2/haskell-ordnub/#ordnub
ordNub :: Ord a => [a] -> [a]
ordNub = go Set.empty where
	go _ [] = []
	go s (x:xs)
		| x `Set.member` s = go s xs
		| otherwise = x : go (Set.insert x s) xs

uniqueBy :: Ord b => (a -> b) -> [a] -> [a]
uniqueBy f = M.elems . mapBy f

mapBy :: Ord b => (a -> b) -> [a] -> M.Map b a
mapBy f = M.fromList . map (f &&& id)

-- | Workaround, sometimes we get HM.lookup "foo" v == Nothing, but lookup "foo" (HM.toList v) == Just smth
(.::) :: FromJSON a => HM.HashMap Text Value -> Text -> A.Parser a
v .:: name = maybe (fail $ "key " ++ show name ++ " not present") parseJSON $ lookup name $ HM.toList v

(.::?) :: FromJSON a => HM.HashMap Text Value -> Text -> A.Parser (Maybe a)
v .::? name = traverse parseJSON $ lookup name $ HM.toList v

-- | Union two JSON objects
objectUnion :: Value -> Value -> Value
objectUnion (Object l) (Object r) = Object $ HM.union l r
objectUnion (Object l) _ = Object l
objectUnion _ (Object r) = Object r
objectUnion _ _ = Null

-- | Union two JSON objects
jsonUnion :: (ToJSON a, ToJSON b) => a -> b -> Value
jsonUnion x y = objectUnion (toJSON x) (toJSON y)

-- | Lift IO exception to ExceptT
liftException :: C.MonadCatch m => m a -> ExceptT String m a
liftException = ExceptT . liftM (left $ \(SomeException e) -> displayException e) . C.try

-- | Same as @liftException@
liftE :: C.MonadCatch m => m a -> ExceptT String m a
liftE = liftException

-- | @liftE@ for IO
liftEIO :: (C.MonadCatch m, MonadIO m) => IO a -> ExceptT String m a
liftEIO = liftE . liftIO

-- | Run actions ignoring errors
tries :: MonadPlus m => [m a] -> m [a]
tries acts = liftM catMaybes $ sequence [liftM Just act `mplus` return Nothing | act <- acts]

triesMap :: MonadPlus m => (a -> m b) -> [a] -> m [b]
triesMap f = tries . map f

-- | Lift IO exception to MonadError
liftExceptionM :: (C.MonadCatch m, MonadError String m) => m a -> m a
liftExceptionM act = C.catch act onError where
	onError = throwError . (\(SomeException e) -> displayException e)

-- | Lift IO exceptions to ExceptT
liftIOErrors :: C.MonadCatch m => ExceptT String m a -> ExceptT String m a
liftIOErrors act = liftException (runExceptT act) >>= either throwError return

eitherT :: (Monad m, MonadError String m) => Either String a -> m a
eitherT = either throwError return

-- | Throw error as exception
liftThrow :: (Show e, MonadError e m, C.MonadCatch m) => m a -> m a
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
	onErr e = out $ pre ++ displayException e

logIO :: String -> (String -> IO ()) -> IO () -> IO ()
logIO pre out = handle onIO where
	onIO :: IOException -> IO ()
	onIO e = out $ pre ++ displayException e

logAsync :: (String -> IO ()) -> IO () -> IO ()
logAsync out = handle onAsync where
	onAsync :: AsyncException -> IO ()
	onAsync e = out (displayException e) >> throwIO e

ignoreIO :: IO () -> IO ()
ignoreIO = handle (const (return ()) :: IOException -> IO ())

liftAsync :: (C.MonadThrow m, C.MonadCatch m, MonadIO m) => IO (Async a) -> ExceptT String m a
liftAsync = liftExceptionM . ExceptT . liftIO . liftM (left displayException) . join . liftM waitCatch

class FromCmd a where
	cmdP :: Parser a

cmdJson :: String -> [A.Pair] -> Value
cmdJson nm ps = object $ ("command" .= nm) : ps

withCmd :: String -> (Object -> A.Parser a) -> Value -> A.Parser a
withCmd nm fn = withObject ("command " ++ nm) $ \v -> guardCmd nm v *> fn v

guardCmd :: String -> Object -> A.Parser ()
guardCmd nm obj = do
	cmdName <- obj .:: "command"
	guard (nm == cmdName)

-- | Add help command to parser
withHelp :: Parser a -> Parser a
withHelp = (helper' <*>) where
	helper' = abortOption ShowHelpText $ long "help" <> short '?' <> help "show help" <> hidden

-- | Subcommand
cmd :: String -> String -> Parser a -> Mod CommandFields a
cmd n d p = command n (info (withHelp p) (progDesc d))

-- | Parse arguments or return help
parseArgs :: String -> ParserInfo a -> [String] -> Either String a
parseArgs nm p = handle' . execParserPure (prefs mempty) (p { infoParser = withHelp (infoParser p) }) where
	handle' :: ParserResult a -> Either String a
	handle' (Success r) = Right r
	handle' (Failure f) = Left $ fst $ renderFailure f nm
	handle' _ = Left "error: completion invoked result"
