{-# LANGUAGE OverloadedStrings, ConstraintKinds, FlexibleContexts, LambdaCase, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Tools.GhcMod (
	list,
	browse, browseInspection,
	langs, flags,
	info,
	TypedRegion(..),
	typeOf,
	OutputMessage(..),
	parseOutputMessages, parseOutputMessage,
	check,
	lint,

	runGhcMod,

	locateGhcModEnv, ghcModEnvPath,
	ghcModWorker,
	WorkerMap,
	ghcModMultiWorker, dispatch,
	waitMultiGhcMod,

	GhcModT,
	module Control.Concurrent.Worker,

	module Control.Monad.Except,
	module HsDev.Tools.Types
	) where

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.DeepSeq
import Control.Exception (SomeException(..))
import Control.Lens (view, preview, _Just, over)
import Control.Monad.Except
import Control.Monad.Catch (MonadThrow(..), MonadCatch(..))
import Control.Monad.Reader
import Data.Aeson hiding (Error)
import Data.Char
import Data.List (sort)
import Data.Maybe
import qualified Data.Map as M
import Data.String (fromString)
import Exception (gcatch)
import System.Directory
import System.FilePath (normalise)
import Text.Read (readMaybe)

import Language.Haskell.GhcMod (GhcModT, withOptions)
import qualified Language.Haskell.GhcMod as GhcMod
import qualified Language.Haskell.GhcMod.Monad as GhcMod
import qualified Language.Haskell.GhcMod.Types as GhcMod

import Control.Concurrent.Worker
import HsDev.PackageDb
import HsDev.Project
import HsDev.Sandbox (searchPackageDbStack)
import HsDev.Symbols
import HsDev.Tools.Base
import HsDev.Tools.Types
import HsDev.Util ((.::), liftIOErrors, liftThrow, readFileUtf8, ordNub)

-- FIXME: Pass package-db stack options to ghc-mod
list :: [String] -> PackageDbStack -> ExceptT String IO [ModuleLocation]
list opts pdbs = runGhcMod (GhcMod.defaultOptions { GhcMod.optGhcUserOptions = opts }) $ do
	ms <- (map splitPackage . lines) <$> GhcMod.modules True
	return [InstalledModule (topPackageDb pdbs) (readMaybe p) m | (m, p) <- ms]
	where
		splitPackage :: String -> (String, String)
		splitPackage = second (drop 1) . break isSpace

-- FIXME: Pass package-db stack options to ghc-mod
browse :: [String] -> PackageDbStack -> String -> Maybe ModulePackage -> ExceptT String IO InspectedModule
browse opts pdbs mname mpackage = inspect thisLoc (return $ browseInspection opts) $ runGhcMod
	(GhcMod.defaultOptions { GhcMod.optGhcUserOptions = packageOpt mpackage ++ opts }) $ do
		ds <- (mapMaybe parseDecl . lines) <$> GhcMod.browse
			(GhcMod.defaultBrowseOpts {
				GhcMod.optBrowseDetailed = True,
				GhcMod.optBrowseQualified = True })
			mpkgname
		return Module {
			_moduleName = fromString mname,
			_moduleDocs = Nothing,
			_moduleLocation = thisLoc,
			_moduleExports = Just [ExportName Nothing (view declarationName d) ExportNothing | d <- ds],
			_moduleImports = [import_ iname |
				iname <- ordNub (mapMaybe (preview definedModule) ds),
				iname /= fromString mname],
			_moduleDeclarations = sortDeclarations ds }
	where
		mpkgname = maybe mname (\p -> view packageName p ++ ":" ++ mname) mpackage
		thisLoc = view moduleIdLocation $ mloc mname
		mloc mname' = ModuleId (fromString mname') $ InstalledModule (topPackageDb pdbs) Nothing mname'
		parseDecl s = do
			groups <- matchRx rx s
			let
				rdecl = decl (fromString $ groups `at` 3) $ case groups 5 of
					Nothing -> Function (Just $ fromString $ groups `at` 4) [] Nothing
					Just k -> declarationTypeCtor k $
						TypeInfo Nothing (maybe [] (map fromString . words) $ groups 7) Nothing []
			return $ rdecl `definedIn` mloc (init $ groups `at` 1)
		definedModule = declarationDefined . _Just . moduleIdName
		-- groups:
		-- 1: "<module>."
		-- 3: "<name>"
		-- 4: "<type>" or "<rest>"
		-- 5: "<kind>" (class, type, data or newtype)
		-- 6: "<name>"
		-- 7: " <args>"
		rx = "^((\\w+\\.)*)(\\w+)\\s+::\\s+((class|type|data|newtype)\\s+(\\w+)((\\s+\\w+)*)?|.*)$"

browseInspection :: [String] -> Inspection
browseInspection = InspectionAt 0 . sort . ordNub

langs :: ExceptT String IO [String]
langs = runGhcMod GhcMod.defaultOptions $ (lines . nullToNL) <$> GhcMod.languages

flags :: ExceptT String IO [String]
flags = runGhcMod GhcMod.defaultOptions $ (lines . nullToNL) <$> GhcMod.flags

-- FIXME: Detect actual package-db for module
info :: [String] -> PackageDbStack -> FilePath -> String -> GhcModT IO Declaration
info opts pdbs file sname = do
	fileCts <- liftIO $ readFileUtf8 file
	rs <- withOptions (\o -> o { GhcMod.optGhcUserOptions = packageDbStackOpts pdbs ++ opts }) $
		liftM nullToNL $ GhcMod.info file (GhcMod.Expression sname)
	toDecl fileCts rs
	where
		toDecl fstr s =
			liftM (recalcDeclTabs fstr) .
			maybe (throwError $ GhcMod.GMEString $ "Can't parse info: '" ++ sname ++ "'") return $
			parseData s `mplus` parseFunction s
		recalcDeclTabs :: String -> Declaration -> Declaration
		recalcDeclTabs fstr = over (declarationPosition . _Just) (recalcTabs fstr 8)
		parseFunction s = do
			groups <- matchRx (sname ++ "\\s+::\\s+(.*?)(\\s+-- Defined (at (.*)|in `(.*)'))?$") s
			return (decl (fromString sname) (Function (Just $ fromString $ groups `at` 1) [] Nothing)) {
				_declarationDefined = unnamedModuleId <$>
					((groups 4 >>= parseSrc) <|> (mkMod <$> groups 5)),
				_declarationPosition = groups 4 >>= parsePos }
		parseData s = do
			groups <- matchRx "(newtype|type|data)\\s+((.*)=>\\s+)?(\\S+)\\s+((\\w+\\s+)*)=(\\s*(.*)\\s+-- Defined (at (.*)|in `(.*)'))?" s
			let
				args = maybe [] (map fromString . words) $ groups 5
				ctx = fmap (fromString . trim) $ groups 3
				def = fmap fromString $ groups 8
			return (decl (fromString sname) (declarationTypeCtor (groups `at` 1) $ TypeInfo ctx args def [])) {
				_declarationDefined = unnamedModuleId <$>
					((groups 10 >>= parseSrc) <|> (mkMod <$> groups 11)),
				_declarationPosition = groups 10 >>= parsePos }
		parseSrc src = case splitRx ":(?=\\d)" src of
			[srcFile, _, _] -> Just $ FileModule srcFile Nothing
			_ -> Nothing
		parsePos src = case splitRx ":(?=\\d)" src of
			[_, line, column] ->  Position <$> readMaybe line <*> readMaybe column
			_ -> Nothing
		mkMod = InstalledModule (topPackageDb pdbs) Nothing
		trim = p . p where
			p = reverse . dropWhile isSpace

data TypedRegion = TypedRegion {
	typedRegion :: Region,
	typedExpr :: String,
	typedType :: String }
		deriving (Eq, Ord, Read, Show)

instance NFData TypedRegion where
	rnf (TypedRegion r e t) = rnf r `seq` rnf e `seq` rnf t

instance ToJSON TypedRegion where
	toJSON (TypedRegion r e t) = object [
		"region" .= r,
		"expr" .= e,
		"type" .= t]

instance FromJSON TypedRegion where
	parseJSON = withObject "typed region" $ \v -> TypedRegion <$>
		v .:: "region" <*>
		v .:: "expr" <*>
		v .:: "type"

typeOf :: [String] -> PackageDbStack -> FilePath -> Int -> Int -> GhcModT IO [TypedRegion]
typeOf opts pdbs file line col = withOptions (\o -> o { GhcMod.optGhcUserOptions = packageDbStackOpts pdbs ++ opts }) $ do
	fileCts <- liftIO $ readFileUtf8 file
	let
		Position line' col' = calcTabs fileCts 8 (Position line col)
	ts <- lines <$> GhcMod.types file line' col'
	return $ mapMaybe (toRegionType fileCts) ts
	where
		toRegionType :: String -> String -> Maybe TypedRegion
		toRegionType fstr s = do
			(r, tp) <- parseRead s $ (,) <$> parseRegion fstr <*> readParse
			return $ TypedRegion r (regionStr r fstr) tp
		parseRegion :: String -> ReadM Region
		parseRegion fstr = Region <$> parsePosition fstr <*> parsePosition fstr
		parsePosition :: String -> ReadM Position
		parsePosition fstr = recalcTabs fstr 8 <$> (Position <$> readParse <*> readParse)

parseOutputMessages :: String -> [Note OutputMessage]
parseOutputMessages = mapMaybe parseOutputMessage . lines

parseOutputMessage :: String -> Maybe (Note OutputMessage)
parseOutputMessage s = do
	groups <- matchRx "^(.+):(\\d+):(\\d+):(\\s*(Warning|Error):)?\\s*(.*)$" s
	l <- readMaybe (groups `at` 2)
	c <- readMaybe (groups `at` 3)
	return Note {
		_noteSource = FileModule (normalise (groups `at` 1)) Nothing,
		_noteRegion = regionAt (Position l c),
		_noteLevel = Just $ if groups 5 == Just "Warning" then Warning else Error,
		_note = outputMessage $ nullToNL (groups `at` 6) }

recalcOutputMessageTabs :: [(FilePath, String)] -> Note OutputMessage -> Note OutputMessage
recalcOutputMessageTabs fileCts n = fromMaybe n $ do
	src <- preview (noteSource . moduleFile) n
	cts <- lookup src fileCts
	return $ recalcTabs cts 8 n

-- | Replace NULL with newline
nullToNL :: String -> String
nullToNL = map $ \case
	'\0' -> '\n'
	ch -> ch

check :: [String] -> PackageDbStack -> [FilePath] -> Maybe Project -> GhcModT IO [Note OutputMessage]
check opts pdbs files _ = do
	cts <- liftIO $ mapM readFileUtf8 files
	withOptions (\o -> o { GhcMod.optGhcUserOptions = packageDbStackOpts pdbs ++ opts }) $ do
		res <- GhcMod.checkSyntax files
		return $ map (recalcOutputMessageTabs (zip files cts)) $ parseOutputMessages res

lint :: [String] -> FilePath -> GhcModT IO [Note OutputMessage]
lint opts file = do
	cts <- liftIO $ readFileUtf8 file
	res <- GhcMod.lint (GhcMod.defaultLintOpts { GhcMod.optLintHlintOpts = opts }) file
	return $ map (recalcOutputMessageTabs [(file, cts)]) $ parseOutputMessages res

gmOut :: IO GhcMod.GhcModOut
gmOut = do
	ch <- newChan
	return GhcMod.GhcModOut {
		GhcMod.gmoOptions = GhcMod.OutputOpts {
			GhcMod.ooptLogLevel = GhcMod.GmSilent,
			GhcMod.ooptStyle = GhcMod.PlainStyle,
			GhcMod.ooptLineSeparator = GhcMod.LineSeparator "\0",
			GhcMod.ooptLinePrefix = Nothing },
		GhcMod.gmoChan = ch }

runGhcMod :: (GhcMod.IOish m, MonadCatch m) => GhcMod.Options -> GhcModT m a -> ExceptT String m a
runGhcMod opts act = do
	out <- liftIO gmOut
	cur <- liftIO getCurrentDirectory
	liftIOErrors $ ExceptT $ liftM (left show . right fst . fst) $ flip runReaderT out $ GhcMod.unGmOutT $
		GhcMod.withGhcModEnv cur opts $ \(env, _) ->
			GhcMod.runGhcModT' env GhcMod.defaultGhcModState act

locateGhcModEnv :: FilePath -> IO (Either Project PackageDbStack)
locateGhcModEnv f = do
	mproj <- locateProject f
	maybe (liftM Right $ searchPackageDbStack f) (return . Left) mproj

ghcModEnvPath :: FilePath -> Either Project PackageDbStack -> FilePath
ghcModEnvPath defaultPath = either (view projectPath) (fromMaybe defaultPath . preview packageDb . topPackageDb)

-- | Create ghc-mod worker for project or for sandbox
ghcModWorker :: Either Project PackageDbStack -> IO (Worker (GhcModT IO))
ghcModWorker p = do
	home <- getHomeDirectory
	startWorker (runGhcModT'' $ ghcModEnvPath home p) id liftThrow
	where
		runGhcModT'' :: FilePath -> GhcModT IO () -> IO ()
		runGhcModT'' cur act = void $ do
			out <- gmOut
			flip runReaderT out $
				GhcMod.unGmOutT $
				GhcMod.withGhcModEnv cur GhcMod.defaultOptions $ \(env, _) ->
					GhcMod.runGhcModT' env GhcMod.defaultGhcModState (act `catchError` (void . return))

type WorkerMap = MVar (M.Map FilePath (Worker (GhcModT IO)))

-- | Manage many ghc-mod workers for each project/sandbox
ghcModMultiWorker :: IO (Worker (ReaderT WorkerMap IO))
ghcModMultiWorker = newMVar M.empty >>= \m -> startWorker (`runReaderT` m) id id

instance MonadThrow (GhcMod.GmOutT IO) where
	throwM = lift . throwM

instance MonadCatch (GhcMod.GmOutT IO) where
	catch = gcatch

instance MonadThrow (GhcModT IO) where
	throwM = lift . throwM

instance MonadCatch (GhcModT IO) where
	catch = gcatch

dispatch :: FilePath -> GhcModT IO a -> ReaderT WorkerMap IO (Async a)
dispatch file act = do
	mvar <- ask
	home <- liftIO getHomeDirectory
	env' <- liftIO $ locateGhcModEnv file
	let
		envPath' = ghcModEnvPath home env'
	liftIO $ modifyMVar mvar $ \wmap -> do
		w <- maybe (ghcModWorker env') return $ M.lookup envPath' wmap
		t <- pushTask w act
		return (M.insert envPath' w wmap, t)

waitMultiGhcMod :: Worker (ReaderT WorkerMap IO) -> FilePath -> GhcModT IO a -> ExceptT String IO a
waitMultiGhcMod w f =
	liftIO . pushTask w . dispatch f >=>
	asExceptT . waitCatch >=>
	asExceptT . waitCatch
	where
		asExceptT :: Monad m => m (Either SomeException a) -> ExceptT String m a
		asExceptT = ExceptT . liftM (left (\(SomeException e) -> show e))
