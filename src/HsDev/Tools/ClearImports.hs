module HsDev.Tools.ClearImports (
	dumpMinimalImports, waitImports, cleanTmpImports,
	findMinimalImports,
	groupImports, splitImport,
	clearImports
	) where

import Control.Arrow
import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad.Error
import Data.Char
import Data.List
import System.Directory
import System.FilePath
import qualified Language.Haskell.Exts as Exts

import GHC
import GHC.Paths (libdir)

-- | Dump minimal imports
dumpMinimalImports :: [String] -> FilePath -> ErrorT String IO String
dumpMinimalImports opts f = do
	cur <- liftIO getCurrentDirectory
	file <- liftIO $ canonicalizePath f

	m <- liftIO $ Exts.parseFile file
	mname <- case m of
		Exts.ParseFailed loc err -> throwError $
			"Failed to parse file at " ++
			Exts.prettyPrint loc ++ ":" ++ err
		Exts.ParseOk (Exts.Module _ (Exts.ModuleName mname) _ _ _ _ _) -> return mname

	ErrorT $ handle onError $ liftM Right $ void $ liftIO $ runGhc (Just libdir) $ do
		df <- getSessionDynFlags
		let
			df' = df {
				ghcLink = NoLink,
				hscTarget = HscNothing,
				dumpDir = Just cur,
				stubDir = Just cur,
				objectDir = Just cur,
				hiDir = Just cur }
		(df'', _, _) <- parseDynamicFlags df' (map noLoc ("-ddump-minimal-imports" : opts))
		setSessionDynFlags df''
		defaultCleanupHandler df'' $ do
			t <- guessTarget file Nothing
			setTargets [t]
			load LoadAllTargets

	length mname `seq` return mname
	where
		onError :: SomeException -> IO (Either String ())
		onError = return . Left . show

-- | Read imports from file
waitImports :: FilePath -> IO [String]
waitImports f = retry 1000 $ do
	is <- liftM lines $ readFile f
	length is `seq` return is

-- | Clean temporary files
cleanTmpImports :: FilePath -> IO ()
cleanTmpImports dir = do
	dumps <- liftM (map (dir </>) . filter ((== ".imports") . takeExtension)) $ getDirectoryContents dir
	forM_ dumps $ handle ignoreIO . removeFile
	where
		ignoreIO :: IOException -> IO ()
		ignoreIO _ = return ()

-- | Dump and read imports
findMinimalImports :: [String] -> FilePath -> ErrorT String IO [String]
findMinimalImports opts f = do
	file <- liftIO $ canonicalizePath f
	mname <- dumpMinimalImports opts file
	is <- liftIO $ waitImports (mname <.> "imports")
	liftIO $ cleanTmpImports $ takeDirectory file
	return is

-- | Groups several lines related to one import by indents
groupImports :: [String] -> [[String]]
groupImports = unfoldr getPack where
	getPack [] = Nothing
	getPack (s:ss) = Just $ first (s:) $ break (null . takeWhile isSpace) ss

-- | Split import to import and import-list
splitImport :: [String] -> (String, String)
splitImport = splitBraces . unwords . map trim where
	trim = twice $ reverse . dropWhile isSpace
	cut = twice $ reverse . drop 1
	twice f = f . f
	splitBraces = (trim *** (trim . cut)) . break (== '(')

-- | Returns minimal imports for file specified
clearImports :: [String] -> FilePath -> ErrorT String IO [(String, String)]
clearImports opts = liftM (map splitImport . groupImports) . findMinimalImports opts

-- | Retry action on fail
retry :: (MonadPlus m, MonadIO m) => Int -> m a -> m a
retry dt act = msum $ act : repeat ((liftIO (threadDelay dt) >>) act)
