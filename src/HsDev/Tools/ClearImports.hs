module HsDev.Tools.ClearImports (
	dumpMinimalImports, waitImports, cleanTmpImports,
	findMinimalImports,
	groupImports, splitImport,
	clearImports,

	module Control.Monad.Except
	) where

import Control.Arrow
import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad.Except
import Data.Char
import Data.List
import Data.Maybe (mapMaybe)
import System.Directory
import System.FilePath
import qualified Language.Haskell.Exts as Exts

import GHC
import GHC.Paths (libdir)

import HsDev.Util
import HsDev.Tools.Ghc.Compat

-- | Dump minimal imports
dumpMinimalImports :: [String] -> FilePath -> ExceptT String IO String
dumpMinimalImports opts f = do
	cur <- liftE getCurrentDirectory
	file <- liftE $ canonicalizePath f
	cts <- liftE $ readFileUtf8 file

	mname <- case Exts.parseFileContentsWithMode (pmode file) cts of
		Exts.ParseFailed loc err -> throwError $
			"Failed to parse file at " ++
			Exts.prettyPrint loc ++ ":" ++ err
		Exts.ParseOk (Exts.Module _ (Just (Exts.ModuleHead _ (Exts.ModuleName _ mname) _ _)) _ _ _) -> return mname
		_ -> throwError "Error"

	void $ liftE $ runGhc (Just libdir) $ do
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
		_ <- setSessionDynFlags df''
		cleanupHandler df'' $ do
			t <- guessTarget file Nothing
			setTargets [t]
			load LoadAllTargets

	length mname `seq` return mname
	where
		pmode :: FilePath -> Exts.ParseMode
		pmode f' = Exts.defaultParseMode {
			Exts.parseFilename = f',
			Exts.baseLanguage = Exts.Haskell2010,
			Exts.extensions = Exts.glasgowExts ++ map Exts.parseExtension exts,
			Exts.fixities = Just Exts.baseFixities }
		exts = mapMaybe (stripPrefix "-X") opts

-- | Read imports from file
waitImports :: FilePath -> IO [String]
waitImports f = retry 1000 $ do
	is <- liftM lines $ readFile f
	length is `seq` return is

-- | Clean temporary files
cleanTmpImports :: FilePath -> IO ()
cleanTmpImports dir = do
	dumps <- liftM (map (dir </>) . filter ((== ".imports") . takeExtension)) $ getDirectoryContents dir
	forM_ dumps $ handle ignoreIO' . retry 1000 . removeFile
	where
		ignoreIO' :: IOException -> IO ()
		ignoreIO' _ = return ()

-- | Dump and read imports
findMinimalImports :: [String] -> FilePath -> ExceptT String IO [String]
findMinimalImports opts f = do
	file <- liftE $ canonicalizePath f
	mname <- dumpMinimalImports opts file
	is <- liftE $ waitImports (mname <.> "imports")
	tmp <- liftE getCurrentDirectory
	liftE $ cleanTmpImports tmp
	return is

-- | Groups several lines related to one import by indents
groupImports :: [String] -> [[String]]
groupImports = unfoldr getPack where
	getPack [] = Nothing
	getPack (s:ss) = Just $ first (s:) $ break (null . takeWhile isSpace) ss

-- | Split import to import and import-list
splitImport :: [String] -> (String, String)
splitImport = splitBraces . unwords . map trim where
	cut = twice $ reverse . drop 1
	twice f = f . f
	splitBraces = (trim *** (trim . cut)) . break (== '(')

-- | Returns minimal imports for file specified
clearImports :: [String] -> FilePath -> ExceptT String IO [(String, String)]
clearImports opts = liftM (map splitImport . groupImports) . findMinimalImports opts

-- | Retry action on fail
retry :: (MonadPlus m, MonadIO m) => Int -> m a -> m a
retry dt act = msum $ act : repeat ((liftIO (threadDelay dt) >>) act)
