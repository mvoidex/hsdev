{-# LANGUAGE FlexibleContexts, OverloadedStrings, TemplateHaskell, CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Util (
	withCurrentDirectory,
	directoryContents,
	traverseDirectory, searchPath,
	haskellSource,
	cabalFile,
	-- * String utils
	tab,
	trim, split,
	-- * Other utils
	ordNub, uniqueBy, mapBy,
	-- * Helper
	(.::), (.::?), (.::?!), objectUnion, noNulls, fromJSON',
	-- * Exceptions
	liftException, liftE, tries, triesMap, liftIOErrors,
	logAll,
	-- * UTF-8
	fromUtf8, toUtf8, readFileUtf8, writeFileUtf8,
	-- * IO
	hGetLineBS, logIO, ignoreIO, logAsync,
	-- * Command line
	FromCmd(..),
	cmdJson, guardCmd,
	withHelp, cmd, parseArgs,
	-- * Version stuff
	version,
	-- * Parse
	parseDT,

	-- * Log utils
	timer,

	-- * Reexportss
	module Control.Monad.Except,
	MonadIO(..)
	) where

import Control.Arrow (second, left, (&&&))
import Control.Exception
import Control.DeepSeq
import Control.Monad
import Control.Monad.Except
import qualified Control.Monad.Catch as C
import Data.Aeson hiding (Result(..), Error)
import qualified Data.Aeson.Types as A
import Data.Char (isSpace)
import Data.List (unfoldr)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HM (HashMap, toList, union)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Text (Text)
import qualified Data.Text.IO as ST
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import Data.Time.Clock.POSIX
import Distribution.Text (simpleParse)
#if MIN_VERSION_Cabal(3,0,0)
import qualified Distribution.Parsec as DT
#else
import qualified Distribution.Text as DT
#endif
import Options.Applicative
import qualified System.Directory as Dir
import System.FilePath
import System.Log.Simple
import System.IO
import Text.Format
import Text.Read (readMaybe)

#if !MIN_VERSION_directory(1,2,6)
#if mingw32_HOST_OS
import qualified System.Win32 as Win32
import Data.Bits ((.&.))
#else
import qualified System.Posix as Posix
#endif
#endif

import HsDev.Version

-- | Run action with current directory set
withCurrentDirectory :: (MonadIO m, C.MonadMask m) => FilePath -> m a -> m a
withCurrentDirectory cur act = C.bracket (liftIO Dir.getCurrentDirectory) (liftIO . Dir.setCurrentDirectory) $
	const (liftIO (Dir.setCurrentDirectory cur) >> act)

-- | Is directory symbolic link
dirIsSymLink :: FilePath -> IO Bool
#if MIN_VERSION_directory(1,3,0)
dirIsSymLink = Dir.pathIsSymbolicLink
#elif MIN_VERSION_directory(1,2,6)
dirIsSymLink = Dir.isSymbolicLink
#else
dirIsSymLink path = do
#if mingw32_HOST_OS
	isReparsePoint <$> Win32.getFileAttributes path
	where
		fILE_ATTRIBUTE_REPARSE_POINT = 0x400
		isReparsePoint attr = attr .&. fILE_ATTRIBUTE_REPARSE_POINT /= 0
#else
	Posix.isSymbolicLink <$> Posix.getSymbolicLinkStatus path
#endif
#endif

-- | Get directory contents safely: no fail, ignoring symbolic links, also prepends paths with dir name
directoryContents :: FilePath -> IO [FilePath]
directoryContents p = handle ignore $ do
	b <- Dir.doesDirectoryExist p
	isLink <- dirIsSymLink p
	if b && (not isLink)
		then liftM (map (p </>) . filter (`notElem` [".", ".."])) (Dir.getDirectoryContents p)
		else return []
	where
		ignore :: SomeException -> IO [FilePath]
		ignore _ = return []

-- | Collect all file names in directory recursively
traverseDirectory :: FilePath -> IO [FilePath]
traverseDirectory p = handle onError $ do
	cts <- directoryContents p
	liftM concat $ forM cts $ \c -> do
		isDir <- Dir.doesDirectoryExist c
		if isDir
			then (c :) <$> traverseDirectory c
			else return [c]
	where
		onError :: IOException -> IO [FilePath]
		onError _ = return []

-- | Search something up
searchPath :: (MonadIO m, MonadPlus m) => FilePath -> (FilePath -> m a) -> m a
searchPath p f = do
	p' <- liftIO $ Dir.canonicalizePath p
	isDir <- liftIO $ Dir.doesDirectoryExist p'
	search' (if isDir then p' else takeDirectory p')
	where
		search' dir
			| isDrive dir = f dir
			| otherwise = f dir `mplus` search' (takeDirectory dir)

-- | Is haskell source?
haskellSource :: FilePath -> Bool
haskellSource f = takeExtension f `elem` [".hs", ".lhs"]

-- | Is cabal file?
cabalFile :: FilePath -> Bool
cabalFile f = takeExtension f == ".cabal"

-- | Add N tabs to line
tab :: Int -> String -> String
tab n s = replicate n '\t' ++ s

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

-- | Returns @Nothing@ when key doesn't exist or value is @Null@
(.::?) :: FromJSON a => HM.HashMap Text Value -> Text -> A.Parser (Maybe a)
v .::? name = fmap join $ traverse parseJSON $ lookup name $ HM.toList v

-- | Same as @.::?@ for list, returns empty list for non-existant key or @Null@ value
(.::?!) :: FromJSON a => HM.HashMap Text Value -> Text -> A.Parser [a]
v .::?! name = fromMaybe [] <$> (v .::? name)

-- | Union two JSON objects
objectUnion :: Value -> Value -> Value
objectUnion (Object l) (Object r) = Object $ HM.union l r
objectUnion (Object l) _ = Object l
objectUnion _ (Object r) = Object r
objectUnion _ _ = Null

-- | No Nulls in JSON object
noNulls :: [A.Pair] -> [A.Pair]
noNulls = filter (not . isNull . snd) where
	isNull Null = True
	isNull v = v == A.emptyArray || v == A.emptyObject || v == A.String ""

-- | Try convert json to value
fromJSON' :: FromJSON a => Value -> Maybe a
fromJSON' v = case fromJSON v of
	A.Success r -> Just r
	_ -> Nothing

-- | Lift IO exception to ExceptT
liftException :: C.MonadCatch m => m a -> ExceptT String m a
liftException = ExceptT . liftM (left $ \(SomeException e) -> displayException e) . C.try

-- | Same as @liftException@
liftE :: C.MonadCatch m => m a -> ExceptT String m a
liftE = liftException

-- | Run actions ignoring errors
tries :: MonadPlus m => [m a] -> m [a]
tries acts = liftM catMaybes $ sequence [liftM Just act `mplus` return Nothing | act <- acts]

triesMap :: MonadPlus m => (a -> m b) -> [a] -> m [b]
triesMap f = tries . map f

-- | Lift IO exceptions to ExceptT
liftIOErrors :: C.MonadCatch m => ExceptT String m a -> ExceptT String m a
liftIOErrors act = liftException (runExceptT act) >>= either throwError return

-- | Log exceptions and ignore
logAll :: (MonadLog m, C.MonadCatch m) => m () -> m ()
logAll = C.handleAll logExc' where
	logExc' e = sendLog Warning $ "exception: {}" ~~ displayException e

fromUtf8 :: ByteString -> String
fromUtf8 = T.unpack . T.decodeUtf8

toUtf8 :: String -> ByteString
toUtf8 = T.encodeUtf8 . T.pack

-- | Read file in UTF8
readFileUtf8 :: FilePath -> IO Text
readFileUtf8 f = withFile f ReadMode $ \h -> do
	hSetEncoding h utf8
	cts <- ST.hGetContents h
	cts `deepseq` return cts

writeFileUtf8 :: FilePath -> Text -> IO ()
writeFileUtf8 f cts = withFile f WriteMode $ \h -> do
	hSetEncoding h utf8
	ST.hPutStr h cts

hGetLineBS :: Handle -> IO ByteString
hGetLineBS = fmap L.fromStrict . B.hGetLine

logIO :: C.MonadCatch m => String -> (String -> m ()) -> m () -> m ()
logIO pre out = flip C.catch (onIO out) where
	onIO :: (String -> a) -> IOException -> a
	onIO out' e = out' $ pre ++ displayException e

logAsync :: (MonadIO m, C.MonadCatch m) => (String -> m ()) -> m () -> m ()
logAsync out = flip C.catch (onAsync out) where
	onAsync :: (MonadIO m, C.MonadThrow m) => (String -> m ()) -> AsyncException -> m ()
	onAsync out' e = out' (displayException e) >> C.throwM e

ignoreIO :: C.MonadCatch m => m () -> m ()
ignoreIO = C.handle ignore' where
	ignore' :: Monad m => IOException -> m ()
	ignore' _ = return ()

class FromCmd a where
	cmdP :: Parser a

cmdJson :: String -> [A.Pair] -> Value
cmdJson nm ps = object $ ("command" .= nm) : ps

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

-- instance Log.MonadLog m => Log.MonadLog (ExceptT e m) where
-- 	askLog = lift Log.askLog

-- | Get hsdev version as list of integers
version :: Maybe [Int]
version = mapM readMaybe $ split (== '.') $cabalVersion

-- | Parse Distribution.Text
#if MIN_VERSION_Cabal(3,0,0)
parseDT :: (Monad m, DT.Parsec a) => String -> String -> m a
#else
parseDT :: (Monad m, DT.Text a) => String -> String -> m a
#endif
parseDT typeName v = maybe err return (simpleParse v) where
	err = fail $ "Can't parse {}: {}" ~~ typeName ~~ v

-- | Measure time of action
timer :: MonadLog m => Text -> m a -> m a
timer msg act = do
	s <- liftIO getPOSIXTime
	r <- act
	e <- liftIO getPOSIXTime
	sendLog Trace $ "{}: {}" ~~ msg ~~ show (e - s)
	return r
