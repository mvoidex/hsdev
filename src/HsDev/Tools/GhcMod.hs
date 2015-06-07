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
	module Control.Concurrent.Worker
	) where

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.DeepSeq
import Control.Exception (SomeException(..))
import Control.Lens (view, preview, set, _Just, over)
import Control.Monad.Except
import Control.Monad.Catch (MonadThrow(..), MonadCatch(..))
import Control.Monad.Reader
import Data.Aeson
import Data.Char
import Data.List (sort)
import Data.Maybe
import qualified Data.Map as M
import Data.String (fromString)
import Exception (gcatch)
import System.Directory
import System.FilePath (normalise)
import Text.Read (readMaybe)

import Language.Haskell.GhcMod (GhcModT, runGhcModT, withOptions)
import qualified Language.Haskell.GhcMod as GhcMod
import qualified Language.Haskell.GhcMod.Internal as GhcMod

import Control.Concurrent.Worker
import HsDev.Cabal
import HsDev.Project
import HsDev.Symbols
import HsDev.Tools.Base
import HsDev.Util ((.::), liftIOErrors, liftThrow, withCurrentDirectory, readFileUtf8, ordNub)

list :: [String] -> Cabal -> ExceptT String IO [ModuleLocation]
list opts cabal = runGhcMod (GhcMod.defaultOptions { GhcMod.ghcUserOptions = opts }) $ do
	ms <- (map splitPackage . lines) <$> GhcMod.modules
	return [CabalModule cabal (readMaybe p) m | (m, p) <- ms]
	where
		splitPackage :: String -> (String, String)
		splitPackage = second (drop 1) . break isSpace

browse :: [String] -> Cabal -> String -> Maybe ModulePackage -> ExceptT String IO InspectedModule
browse opts cabal mname mpackage = inspect thisLoc (return $ browseInspection opts) $ runGhcMod
	(GhcMod.defaultOptions { GhcMod.detailed = True, GhcMod.qualified = True, GhcMod.ghcUserOptions = packageOpt mpackage ++ opts }) $ do
		ds <- (mapMaybe parseDecl . lines) <$> GhcMod.browse mpkgname
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
		mloc mname' = ModuleId (fromString mname') $ CabalModule cabal Nothing mname'
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

info :: [String] -> Cabal -> FilePath -> String -> GhcModT IO Declaration
info opts cabal file sname = do
	fileCts <- liftIO $ readFileUtf8 file
	rs <- withOptions (\o -> o { GhcMod.ghcUserOptions = cabalOpt cabal ++ opts }) $
		liftM nullToNL $ GhcMod.info file (GhcMod.Expression sname)
	toDecl fileCts rs
	where
		toDecl fstr s =
			liftM (recalcDeclTabs fstr) .
			maybe (throwError $ GhcMod.GMEString $ "Can't parse info: '" ++ sname ++ "'") return $
			parseData s `mplus` parseFunction s
		recalcDeclTabs :: String -> Declaration -> Declaration
		recalcDeclTabs fstr = over (declarationPosition . _Just) (recalcTabs fstr)
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
		mkMod = CabalModule cabal Nothing
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

typeOf :: [String] -> Cabal -> FilePath -> Int -> Int -> GhcModT IO [TypedRegion]
typeOf opts cabal file line col = withOptions (\o -> o { GhcMod.ghcUserOptions = cabalOpt cabal ++ opts }) $ do
	fileCts <- liftIO $ readFileUtf8 file
	ts <- lines <$> GhcMod.types file line col
	return $ mapMaybe (toRegionType fileCts) ts
	where
		toRegionType :: String -> String -> Maybe TypedRegion
		toRegionType fstr s = do
			(r, tp) <- parseRead s $ (,) <$> parseRegion fstr <*> readParse
			return $ TypedRegion r (regionStr r fstr) tp
		parseRegion :: String -> ReadM Region
		parseRegion fstr = Region <$> parsePosition fstr <*> parsePosition fstr
		parsePosition :: String -> ReadM Position
		parsePosition fstr = recalcTabs fstr <$> (Position <$> readParse <*> readParse)

data OutputMessageLevel = WarningMessage | ErrorMessage deriving (Eq, Ord, Bounded, Enum, Read, Show)

instance NFData OutputMessageLevel where
	rnf WarningMessage = ()
	rnf ErrorMessage = ()

instance ToJSON OutputMessageLevel where
	toJSON WarningMessage = toJSON ("warning" :: String)
	toJSON ErrorMessage = toJSON ("error" :: String)

instance FromJSON OutputMessageLevel where
	parseJSON v = do
		s <- parseJSON v
		msum [
			guard (s == ("warning" :: String)) >> return WarningMessage,
			guard (s == ("error" :: String)) >> return ErrorMessage,
			fail "Invalid output message level"]

data OutputMessage = OutputMessage {
	errorLocation :: Location,
	errorLevel :: OutputMessageLevel,
	errorMessage :: String }
		deriving (Eq, Show)

instance NFData OutputMessage where
	rnf (OutputMessage l w m) = rnf l `seq` rnf w `seq` rnf m

instance ToJSON OutputMessage where
	toJSON (OutputMessage l w m) = object [
		"location" .= l,
		"level" .= w,
		"message" .= m]

instance FromJSON OutputMessage where
	parseJSON = withObject "error message" $ \v -> OutputMessage <$>
		v .:: "location" <*>
		v .:: "level" <*>
		v .:: "message"

parseOutputMessages :: String -> [OutputMessage]
parseOutputMessages = mapMaybe parseOutputMessage . lines

parseOutputMessage :: String -> Maybe OutputMessage
parseOutputMessage s = do
	groups <- matchRx "^(.+):(\\d+):(\\d+):(\\s*(Warning|Error):)?\\s*(.*)$" s
	return OutputMessage {
		errorLocation = Location {
			_locationModule = FileModule (normalise (groups `at` 1)) Nothing,
			_locationPosition = Position <$> readMaybe (groups `at` 2) <*> readMaybe (groups `at` 3) },
		errorLevel = if groups 5 == Just "Warning" then WarningMessage else ErrorMessage,
		errorMessage = nullToNL (groups `at` 6) }

recalcOutputMessageTabs :: [(FilePath, String)] -> OutputMessage -> OutputMessage
recalcOutputMessageTabs fileCts msg = msg {
	errorLocation = recalc' (errorLocation msg) }
	where
		recalc' :: Location -> Location
		recalc' loc = fromMaybe loc $ do
			pos <- view locationPosition loc
			src <- preview (locationModule . moduleFile) loc
			cts <- lookup src fileCts
			return $ set locationPosition (Just $ recalcTabs cts pos) loc

-- | Replace NULL with newline
nullToNL :: String -> String
nullToNL = map $ \case
	'\0' -> '\n'
	ch -> ch

check :: [String] -> Cabal -> [FilePath] -> Maybe Project -> GhcModT IO [OutputMessage]
check opts cabal files _ = do
	cts <- liftIO $ mapM readFileUtf8 files
	withOptions (\o -> o { GhcMod.ghcUserOptions = cabalOpt cabal ++ opts }) $ do
		res <- GhcMod.checkSyntax files
		return $ map (recalcOutputMessageTabs (zip files cts)) $ parseOutputMessages res

lint :: [String] -> FilePath -> GhcModT IO [OutputMessage]
lint opts file = do
	cts <- liftIO $ readFileUtf8 file
	withOptions (\o -> o { GhcMod.hlintOpts = opts }) $ do
		res <- GhcMod.lint file
		return $ map (recalcOutputMessageTabs [(file, cts)]) $ parseOutputMessages res

runGhcMod :: (GhcMod.IOish m, MonadCatch m) => GhcMod.Options -> GhcModT m a -> ExceptT String m a
runGhcMod opts act = liftIOErrors $ ExceptT $ liftM (left show . fst) $ runGhcModT opts act

locateGhcModEnv :: FilePath -> IO (Either Project Cabal)
locateGhcModEnv f = do
	mproj <- locateProject f
	maybe (liftM Right $ getSandbox f) (return . Left) mproj

ghcModEnvPath :: FilePath -> Either Project Cabal -> FilePath
ghcModEnvPath defaultPath = either (view projectPath) (fromMaybe defaultPath . sandbox)

-- | Create ghc-mod worker for project or for sandbox
ghcModWorker :: Either Project Cabal -> IO (Worker (GhcModT IO))
ghcModWorker p = do
	home <- getHomeDirectory
	startWorker (runGhcModT'' $ ghcModEnvPath home p) id liftThrow
	where
		-- TODO: Uncomment comment below after ghc-mod exports neccessary functions
		functionNotExported = False
		runGhcModT'' :: FilePath -> GhcModT IO () -> IO ()
		runGhcModT'' cur act
			| functionNotExported = withCurrentDirectory cur $
				void $ runGhcModT GhcMod.defaultOptions $ act `catchError` (void . return)
			| otherwise = do
				void $ GhcMod.runGhcModT' cur GhcMod.defaultOptions $ act `catchError` (void . return)

type WorkerMap = MVar (M.Map FilePath (Worker (GhcModT IO)))

-- | Manage many ghc-mod workers for each project/sandbox
ghcModMultiWorker :: IO (Worker (ReaderT WorkerMap IO))
ghcModMultiWorker = newMVar M.empty >>= \m -> startWorker (`runReaderT` m) id id

instance MonadThrow (GhcModT IO) where
	throwM = lift . throwM

instance MonadCatch (GhcModT IO) where
	catch = gcatch

dispatch :: FilePath -> GhcModT IO a -> ReaderT WorkerMap IO (Task a)
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
	asExceptT . taskWait >=>
	asExceptT . taskWait
	where
		asExceptT :: Monad m => m (Either SomeException a) -> ExceptT String m a
		asExceptT = ExceptT . liftM (left (\(SomeException e) -> show e))
