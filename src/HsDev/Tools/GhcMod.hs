{-# LANGUAGE OverloadedStrings, ConstraintKinds, FlexibleContexts, LambdaCase #-}

module HsDev.Tools.GhcMod (
	list,
	browse, browseInspection,
	info,
	TypedRegion(..),
	typeOf,
	OutputMessage(..),
	check,
	lint,

	runGhcMod,

	locateGhcModEnv, ghcModEnvPath,
	ghcModWorker,
	ghcModMultiWorker,
	waitGhcMod,
	waitMultiGhcMod,

	GhcModT,
	module Control.Concurrent.Worker
	) where

import Control.Applicative
import Control.Arrow
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, newMVar, modifyMVar_)
import Control.DeepSeq
import Control.Exception (SomeException, bracket)
import Control.Monad.Error
import Control.Monad.CatchIO (MonadCatchIO)
import Data.Aeson
import Data.Char
import Data.Maybe
import qualified Data.Map as M
import Exception (gtry)
import GHC (getSessionDynFlags, defaultCleanupHandler)
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
import HsDev.Util ((.::), liftIOErrors)

list :: [String] -> Cabal -> ErrorT String IO [ModuleLocation]
list opts cabal = runGhcMod (GhcMod.defaultOptions { GhcMod.ghcUserOptions = opts }) $ do
	ms <- (map splitPackage . lines) <$> GhcMod.modules
	return [CabalModule cabal (readMaybe p) m | (m, p) <- ms]
	where
		splitPackage :: String -> (String, String)
		splitPackage = second (drop 1) . break isSpace

browse :: [String] -> Cabal -> String -> Maybe ModulePackage -> ErrorT String IO InspectedModule
browse opts cabal mname mpackage = inspect mloc (return $ browseInspection opts) $ runGhcMod
	(GhcMod.defaultOptions { GhcMod.detailed = True, GhcMod.ghcUserOptions = packageOpt mpackage ++ opts }) $ do
		ts <- lines <$> GhcMod.browse mpkgname
		return $ Module {
			moduleName = mname,
			moduleDocs = Nothing,
			moduleLocation = mloc,
			moduleExports = [],
			moduleImports = [],
			moduleDeclarations = decls ts }
	where
		mpkgname = maybe mname (\p -> packageName p ++ ":" ++ mname) mpackage
		mloc = CabalModule cabal mpackage mname
		decls rs = M.fromList $ map (declarationName &&& id) $ mapMaybe parseDecl rs
		parseFunction s = do
			groups <- match "(\\w+)\\s+::\\s+(.*)" s
			return $ Declaration (groups `at` 1) Nothing Nothing (Function (Just $ groups `at` 2) [])
		parseType s = do
			groups <- match "(class|type|data|newtype)\\s+(\\w+)(\\s+(\\w+(\\s+\\w+)*))?" s
			let
				args = maybe [] words $ groups 3
			return $ Declaration (groups `at` 2) Nothing Nothing (declarationTypeCtor (groups `at` 1) $ TypeInfo Nothing args Nothing)
		parseDecl s = parseFunction s `mplus` parseType s

browseInspection :: [String] -> Inspection
browseInspection = InspectionAt 0

info :: [String] -> Cabal -> FilePath -> Maybe Project -> String -> String -> GhcModT IO Declaration
info opts cabal file _ _ sname = do
	rs <- withOptions (\o -> o { GhcMod.ghcUserOptions = cabalOpt cabal ++ opts }) $
		GhcMod.info file sname
	toDecl rs
	where
		toDecl s = maybe (throwError $ strMsg $ "Can't parse info: '" ++ s ++ "'") return $ parseData s `mplus` parseFunction s
		parseFunction s = do
			groups <- match (sname ++ "\\s+::\\s+(.*?)(\\s+--(.*))?$") s
			return $ Declaration sname Nothing Nothing (Function (Just $ groups `at` 1) [])
		parseData s = do
			groups <- match "(newtype|type|data)\\s+((.*)=>\\s+)?(\\S+)\\s+((\\w+\\s+)*)=(\\s*(.*)\\s+-- Defined)?" s
			let
				args = maybe [] words $ groups 5
				ctx = fmap trim $ groups 3
				def = groups 8
			return $ Declaration sname Nothing Nothing (declarationTypeCtor (groups `at` 1) $ TypeInfo ctx args def)
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

typeOf :: [String] -> Cabal -> FilePath -> Maybe Project -> String -> Int -> Int -> GhcModT IO [TypedRegion]
typeOf opts cabal file _ _ line col = withOptions (\o -> o { GhcMod.ghcUserOptions = cabalOpt cabal ++ opts }) $ do
	fileCts <- liftIO $ readFile file
	ts <- lines <$> GhcMod.types file line col
	return $ mapMaybe (toRegionType fileCts) ts
	where
		toRegionType :: String -> String -> Maybe TypedRegion
		toRegionType fstr s = do
			(r, tp) <- parseRead s $ (,) <$> parseRegion <*> readParse
			return $ TypedRegion r (regionStr r fstr) tp
		parseRegion :: ReadM Region
		parseRegion = Region <$> parsePosition <*> parsePosition
		parsePosition :: ReadM Position
		parsePosition = Position <$> readParse <*> readParse

data OutputMessageLevel = WarningMessage | ErrorMessage deriving (Eq, Ord, Bounded, Enum, Read, Show)

instance NFData OutputMessageLevel where

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

parseOutputMessage :: String -> Maybe OutputMessage
parseOutputMessage s = do
	groups <- match "^(.+):(\\d+):(\\d+):(\\s*(Warning|Error):)?\\s*(.*)$" s
	return $ OutputMessage {
		errorLocation = Location {
			locationModule = FileModule (normalise (groups `at` 1)) Nothing,
			locationPosition = Position <$> readMaybe (groups `at` 2) <*> readMaybe (groups `at` 3) },
		errorLevel = if groups 5 == Just "Warning" then WarningMessage else ErrorMessage,
		errorMessage = map nullToNL (groups `at` 6) }
	where
		nullToNL = \case
			'\0' -> '\n'
			ch -> ch

check :: [String] -> Cabal -> [FilePath] -> Maybe Project -> GhcModT IO [OutputMessage]
check opts cabal files _ = withOptions (\o -> o { GhcMod.ghcUserOptions = cabalOpt cabal ++ opts }) $ do
	msgs <- lines <$> GhcMod.checkSyntax files
	return $ mapMaybe parseOutputMessage msgs

lint :: [String] -> FilePath -> GhcModT IO [OutputMessage]
lint opts file = withOptions (\o -> o { GhcMod.hlintOpts = opts }) $ do
	msgs <- lines <$> GhcMod.lint file
	return $ mapMaybe parseOutputMessage msgs

runGhcMod :: (GhcMod.IOish m, MonadCatchIO m) => GhcMod.Options -> GhcModT m a -> ErrorT String m a
runGhcMod opts act = liftIOErrors $ ErrorT $ liftM (left show . fst) $ runGhcModT opts act

locateGhcModEnv :: FilePath -> IO (Either Project Cabal)
locateGhcModEnv f = do
	mproj <- locateProject f
	maybe (liftM Right $ getSandbox f) (return . Left) mproj

ghcModEnvPath :: FilePath -> Either Project Cabal -> FilePath
ghcModEnvPath defaultPath = either projectPath (fromMaybe defaultPath . sandbox)

-- | Create ghc-mod worker for project or for sandbox
ghcModWorker :: Either Project Cabal -> IO (Worker (GhcModT IO ()))
ghcModWorker p = do
	home <- getHomeDirectory
	worker_ (runGhcModT'' $ ghcModEnvPath home p) id try
	where
		makeEnv :: FilePath -> IO GhcMod.GhcModEnv
		makeEnv = GhcMod.newGhcModEnv GhcMod.defaultOptions
		functionNotExported = True
		runGhcModT'' :: FilePath -> GhcModT IO () -> IO ()
		runGhcModT'' cur act
			| functionNotExported = withCurrentDirectory cur
				(void . runGhcModT GhcMod.defaultOptions $ act)
			| otherwise = do
				env' <- makeEnv cur
				void $ GhcMod.runGhcModT' env' GhcMod.defaultState $ do
					dflags <- getSessionDynFlags
					defaultCleanupHandler dflags $ do
						--GhcMod.initializeFlagsWithCradle GhcMod.defaultOptions (GhcMod.gmCradle env')
						act
		withCurrentDirectory :: FilePath -> IO a -> IO a
		withCurrentDirectory cur act = bracket getCurrentDirectory setCurrentDirectory $
			const (setCurrentDirectory cur >> act)

-- | Manage many ghc-mod workers for each project/sandbox
ghcModMultiWorker :: IO (Worker (FilePath, GhcModT IO ()))
ghcModMultiWorker = worker id initMultiGhcMod multiWork where
	initMultiGhcMod f = newMVar M.empty >>= f
	multiWork ghcMods (file, act) = do
		home <- getHomeDirectory
		env' <- locateGhcModEnv file
		let
			envPath' = ghcModEnvPath home env'
		modifyMVar_ ghcMods $ \ghcModsMap -> do
			w <- maybe (ghcModWorker env') return $ M.lookup envPath' ghcModsMap
			sendWork w act
			return $ M.insert envPath' w ghcModsMap

waitGhcMod :: Worker (GhcModT IO ()) -> GhcModT IO a -> ErrorT String IO a
waitGhcMod w act = ErrorT $ do
	var <- newEmptyMVar
	sendWork w $ try act >>= liftIO . putMVar var
	takeMVar var

waitMultiGhcMod :: Worker (FilePath, GhcModT IO ()) -> FilePath -> GhcModT IO a -> ErrorT String IO a
waitMultiGhcMod w f act = ErrorT $ do
	var <- newEmptyMVar
	sendWork w (f, try act >>= liftIO . putMVar var)
	takeMVar var

try :: GhcModT IO a -> GhcModT IO (Either String a)
try = liftM (left (show :: SomeException -> String)) . gtry
