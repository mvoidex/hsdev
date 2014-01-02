module HsDev.Tools.ClearImports (
	dumpMinimalImports,
	groupImports, formatImport,
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
dumpMinimalImports :: [String] -> FilePath -> ErrorT String IO [String]
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

	liftIO $ retry 500000 $ do
		is <- liftM lines $ readFile $ mname <.> "imports"
		length is `seq` do
			dumps <- liftM (map (cur </>) . filter ((== ".imports") . takeExtension)) $ getDirectoryContents cur
			forM_ dumps $ handle ignoreIO . removeFile
		return is

	where
		onError :: SomeException -> IO (Either String ())
		onError = return . Left . show

		ignoreIO :: IOException -> IO ()
		ignoreIO _ = return ()

-- | Groups several lines related to one import by indents
groupImports :: [String] -> [[String]]
groupImports = unfoldr getPack where
	getPack [] = Nothing
	getPack (s:ss) = Just $ first (s:) $ break (null . takeWhile isSpace) ss

-- | Format import in one line
formatImport :: [String] -> String
formatImport = trimBraces . unwords . map trim where
	trim = twice $ reverse . dropWhile isSpace
	cut = twice $ reverse . drop 1
	twice f = f . f
	trimBraces str = trim start ++ " (" ++ importList ++ ")" where
		(start, braces) = break (== '(') str
		importList = trim $ cut braces

-- | Returns minimal imports for file specified
clearImports :: [String] -> FilePath -> ErrorT String IO [String]
clearImports opts = liftM (map formatImport . groupImports) .
	dumpMinimalImports opts

-- | Retry action on fail
retry :: (MonadPlus m, MonadIO m) => Int -> m a -> m a
retry dt act = msum $ act : repeat ((liftIO (threadDelay dt) >>) act)
