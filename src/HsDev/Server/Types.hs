{-# LANGUAGE OverloadedStrings, CPP, TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses, TypeFamilies, ConstraintKinds, TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Server.Types (
	ServerMonadBase,
	SessionLog(..), Session(..), SessionMonad(..), askSession, ServerM(..),
	CommandOptions(..), CommandMonad(..), askOptions, ClientM(..),
	withSession, serverListen, serverSetLogLevel, serverWait, serverWaitClients,
	serverSqlDatabase, openSqlConnection, closeSqlConnection, withSqlConnection, withSqlTransaction, serverSetFileContents,
	inSessionGhc, inSessionUpdater, postSessionUpdater, serverExit, commandRoot, commandNotify, commandLink, commandHold,
	ServerCommand(..), ConnectionPort(..), ServerOpts(..), silentOpts, ClientOpts(..), serverOptsArgs, Request(..),

	Command(..),
	FileSource(..), TargetFilter(..), SearchQuery(..), SearchType(..),
	FromCmd(..),
	) where

import Control.Applicative
import Control.Concurrent.Async (Async)
import qualified Control.Concurrent.FiniteChan as F
import Control.Lens (view, set)
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Control
import Data.Aeson hiding (Result(..), Error)
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Default
import Data.Maybe (fromMaybe)
import Data.Foldable (asum)
import Data.Text (Text)
import Data.String (fromString)
import qualified Database.SQLite.Simple as SQL
import Options.Applicative
import System.Log.Simple as Log

import Control.Concurrent.Worker
import Data.LookupTable
import System.Directory.Paths
import Text.Format (Formattable(..), (~~))

import HsDev.Error (hsdevError)
import HsDev.Inspect.Types
import HsDev.Server.Message
import HsDev.Watcher.Types (Watcher)
import HsDev.PackageDb.Types
import HsDev.Project.Types
import HsDev.Tools.Ghc.Worker (GhcWorker, GhcM)
import HsDev.Tools.Types (Note, OutputMessage)
import HsDev.Tools.AutoFix (Refact)
import HsDev.Types (HsDevError(..))
import HsDev.Util

#if mingw32_HOST_OS
import System.Win32.FileMapping.NamePool (Pool)
#endif

type ServerMonadBase m = (MonadIO m, MonadMask m, MonadBaseControl IO m, Alternative m, MonadPlus m)

data SessionLog = SessionLog {
	sessionLogger :: Log,
	sessionListenLog :: IO [Log.Message],
	sessionLogWait :: IO () }

data Session = Session {
	sessionSqlDatabase :: SQL.Connection,
	sessionSqlPath :: String,
	sessionLog :: SessionLog,
	sessionWatcher :: Watcher,
	sessionFileContents :: Path -> Maybe Text -> IO (),
#if mingw32_HOST_OS
	sessionMmapPool :: Maybe Pool,
#endif
	sessionGhc :: GhcWorker,
	sessionUpdater :: Worker (ServerM IO),
	sessionResolveEnvironment :: LookupTable (Maybe Path) (Environment, FixitiesTable),
	sessionExit :: IO (),
	sessionWait :: IO (),
	sessionClients :: F.Chan (IO ()),
	sessionDefines :: [(String, String)] }

class (ServerMonadBase m, MonadLog m) => SessionMonad m where
	getSession :: m Session
	localSession :: (Session -> Session) -> m a -> m a

askSession :: SessionMonad m => (Session -> a) -> m a
askSession f = liftM f getSession

newtype ServerM m a = ServerM { runServerM :: ReaderT Session m a }
	deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO, MonadReader Session, MonadTrans, MonadThrow, MonadCatch, MonadMask)

instance (MonadIO m, MonadMask m) => MonadLog (ServerM m) where
	askLog = ServerM $ asks (sessionLogger . sessionLog)
	localLog fn = ServerM . local setLog' . runServerM where
		setLog' sess = sess { sessionLog = (sessionLog sess) { sessionLogger = fn (sessionLogger (sessionLog sess)) } }

instance ServerMonadBase m => SessionMonad (ServerM m) where
	getSession = ask
	localSession = local

instance MonadBase b m => MonadBase b (ServerM m) where
	liftBase = ServerM . liftBase

instance MonadBaseControl b m => MonadBaseControl b (ServerM m) where
	type StM (ServerM m) a = StM (ReaderT Session m) a
	liftBaseWith f = ServerM $ liftBaseWith (\f' -> f (f' . runServerM))
	restoreM = ServerM . restoreM

instance MFunctor ServerM where
	hoist fn = ServerM . hoist fn . runServerM

instance SessionMonad m => SessionMonad (ReaderT r m) where
	getSession = lift getSession
	localSession = mapReaderT . localSession

instance (SessionMonad m, Monoid w) => SessionMonad (WriterT w m) where
	getSession = lift getSession
	localSession = mapWriterT . localSession

instance SessionMonad m => SessionMonad (StateT s m) where
	getSession = lift getSession
	localSession = mapStateT . localSession

data CommandOptions = CommandOptions {
	commandOptionsRoot :: FilePath,
	commandOptionsNotify :: Notification -> IO (),
	commandOptionsLink :: IO (),
	commandOptionsHold :: IO () }

instance Default CommandOptions where
	def = CommandOptions "." (const $ return ()) (return ()) (return ())

class (SessionMonad m, MonadPlus m) => CommandMonad m where
	getOptions :: m CommandOptions

askOptions :: CommandMonad m => (CommandOptions -> a) -> m a
askOptions f = liftM f getOptions

newtype ClientM m a = ClientM { runClientM :: ServerM (ReaderT CommandOptions m) a }
	deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance MonadTrans ClientM where
	lift = ClientM . lift . lift

instance (MonadIO m, MonadMask m) => MonadLog (ClientM m) where
	askLog = ClientM askLog
	localLog fn = ClientM . localLog fn . runClientM

instance ServerMonadBase m => SessionMonad (ClientM m) where
	getSession = ClientM getSession
	localSession fn = ClientM . localSession fn . runClientM

instance ServerMonadBase m => CommandMonad (ClientM m) where
	getOptions = ClientM $ lift ask

instance MonadBase b m => MonadBase b (ClientM m) where
	liftBase = ClientM . liftBase

instance MonadBaseControl b m => MonadBaseControl b (ClientM m) where
	type StM (ClientM m) a = StM (ServerM (ReaderT CommandOptions m)) a
	liftBaseWith f = ClientM $ liftBaseWith (\f' -> f (f' . runClientM))
	restoreM = ClientM . restoreM

instance MFunctor ClientM where
	hoist fn = ClientM . hoist (hoist fn) . runClientM

instance CommandMonad m => CommandMonad (ReaderT r m) where
	getOptions = lift getOptions

instance (CommandMonad m, Monoid w) => CommandMonad (WriterT w m) where
	getOptions = lift getOptions

instance CommandMonad m => CommandMonad (StateT s m) where
	getOptions = lift getOptions

-- | Run action on session
withSession :: Session -> ServerM m a -> m a
withSession s act = runReaderT (runServerM act) s

-- | Listen server's log
serverListen :: SessionMonad m => m [Log.Message]
serverListen = join . liftM liftIO $ askSession (sessionListenLog . sessionLog)

-- | Set server's log config
serverSetLogLevel :: SessionMonad m => Level -> m Level
serverSetLogLevel lev = do
	l <- askSession (sessionLogger . sessionLog)
	cfg <- updateLogConfig l (set (componentCfg "") (Just lev))
	return $ fromMaybe def $ view (componentCfg "") cfg

-- | Wait for server
serverWait :: SessionMonad m => m ()
serverWait = join . liftM liftIO $ askSession sessionWait

-- | Wait while clients disconnects
serverWaitClients :: SessionMonad m => m ()
serverWaitClients = do
	clientChan <- askSession sessionClients
	liftIO (F.stopChan clientChan) >>= sequence_ . map liftIO

-- | Get sql connection
serverSqlDatabase :: SessionMonad m => m SQL.Connection
serverSqlDatabase = askSession sessionSqlDatabase

-- | Open new sql connection
openSqlConnection :: SessionMonad m => m SQL.Connection
openSqlConnection = do
	p <- askSession sessionSqlPath
	-- FIXME: There's `new` function in HsDev's SQLite module
	liftIO $ do
		conn <- SQL.open p
		SQL.execute_ conn "pragma case_sensitive_like = true;"
		SQL.execute_ conn "pragma synchronous = off;"
		SQL.execute_ conn "pragma journal_mode = memory;"
		return conn

-- | Close sql connection
closeSqlConnection :: SessionMonad m => SQL.Connection -> m ()
closeSqlConnection = liftIO . SQL.close

-- | Locally opens new connection, updating @Session@
withSqlConnection :: SessionMonad m => m a -> m a
withSqlConnection act = bracket openSqlConnection closeSqlConnection $ \conn ->
	localSession (\sess -> sess { sessionSqlDatabase = conn }) act

-- | With sql transaction
withSqlTransaction :: SessionMonad m => ServerM IO a -> m a
withSqlTransaction fn = do
	conn <- serverSqlDatabase
	sess <- getSession
	liftIO $ SQL.withTransaction conn $ withSession sess fn

-- | Set custom file contents
serverSetFileContents :: SessionMonad m => Path -> Maybe Text -> m ()
serverSetFileContents fpath mcts = do
	setCts <- askSession sessionFileContents
	liftIO $ setCts fpath mcts

-- | In ghc session
inSessionGhc :: SessionMonad m => GhcM a -> m a
inSessionGhc act = do
	ghcw <- askSession sessionGhc
	inWorkerWith (hsdevError . GhcError . displayException) ghcw act

-- | In updater
inSessionUpdater :: SessionMonad m => ServerM IO a -> m a
inSessionUpdater act = do
	uw <- askSession sessionUpdater
	inWorkerWith (hsdevError . OtherError . displayException) uw act

-- | Post to updater and return
postSessionUpdater :: SessionMonad m => ServerM IO a -> m (Async a)
postSessionUpdater act = do
	uw <- askSession sessionUpdater
	liftIO $ sendTask uw act

-- | Exit session
serverExit :: SessionMonad m => m ()
serverExit = join . liftM liftIO $ askSession sessionExit

commandRoot :: CommandMonad m => m FilePath
commandRoot = askOptions commandOptionsRoot

commandNotify :: CommandMonad m => Notification -> m ()
commandNotify n = join . liftM liftIO $ askOptions commandOptionsNotify <*> pure n

commandLink :: CommandMonad m => m ()
commandLink = join . liftM liftIO $ askOptions commandOptionsLink

commandHold :: CommandMonad m => m ()
commandHold = join . liftM liftIO $ askOptions commandOptionsHold

-- | Server control command
data ServerCommand =
	Version |
	Start ServerOpts |
	Run ServerOpts |
	Stop ClientOpts |
	Connect ClientOpts |
	Remote ClientOpts Bool Command
		deriving (Show)

data ConnectionPort = NetworkPort Int | UnixPort String deriving (Eq, Read)

instance Default ConnectionPort where
	def = NetworkPort 4567

instance Show ConnectionPort where
	show (NetworkPort p) = show p
	show (UnixPort s) = "unix " ++ s

instance Formattable ConnectionPort

-- | Server options
data ServerOpts = ServerOpts {
	serverPort :: ConnectionPort,
	serverTimeout :: Int,
	serverLog :: Maybe FilePath,
	serverLogLevel :: String,
	serverLogNoColor :: Bool,
	serverDbFile :: Maybe FilePath,
	serverSilent :: Bool }
		deriving (Show)

instance Default ServerOpts where
	def = ServerOpts def 0 Nothing "info" False Nothing False

-- | Silent server with no connection, useful for ghci
silentOpts :: ServerOpts
silentOpts = def { serverSilent = True }

-- | Client options
data ClientOpts = ClientOpts {
	clientPort :: ConnectionPort,
	clientPretty :: Bool,
	clientStdin :: Bool,
	clientTimeout :: Int,
	clientSilent :: Bool }
		deriving (Show)

instance Default ClientOpts where
	def = ClientOpts def False False 0 False

instance FromCmd ServerCommand where
	cmdP = serv <|> remote where
		serv = subparser $ mconcat [
			cmd "version" "hsdev version" (pure Version),
			cmd "start" "start remote server" (Start <$> cmdP),
			cmd "run" "run server" (Run <$> cmdP),
			cmd "stop" "stop remote server" (Stop <$> cmdP),
			cmd "connect" "connect to send commands directly" (Connect <$> cmdP)]
		remote = Remote <$> cmdP <*> noFileFlag <*> cmdP

instance FromCmd ServerOpts where
	cmdP = ServerOpts <$>
		(connectionArg <|> pure (serverPort def)) <*>
		(timeoutArg <|> pure (serverTimeout def)) <*>
		optional logArg <*>
		(logLevelArg <|> pure (serverLogLevel def)) <*>
		noColorFlag <*>
		optional dbFileArg <*>
		serverSilentFlag

instance FromCmd ClientOpts where
	cmdP = ClientOpts <$>
		(connectionArg <|> pure (clientPort def)) <*>
		prettyFlag <*>
		stdinFlag <*>
		(timeoutArg <|> pure (clientTimeout def)) <*>
		silentFlag

portArg :: Parser ConnectionPort
connectionArg :: Parser ConnectionPort
timeoutArg :: Parser Int
logArg :: Parser FilePath
logLevelArg :: Parser String
noColorFlag :: Parser Bool
noFileFlag :: Parser Bool
prettyFlag :: Parser Bool
serverSilentFlag :: Parser Bool
stdinFlag :: Parser Bool
silentFlag :: Parser Bool
dbFileArg :: Parser FilePath

portArg = NetworkPort <$> option auto (long "port" <> metavar "number" <> help "connection port")
#if mingw32_HOST_OS
connectionArg = portArg
#else
unixArg :: Parser ConnectionPort
unixArg = UnixPort <$> strOption (long "unix" <> metavar "name" <> help "unix connection port")
connectionArg = portArg <|> unixArg
#endif
timeoutArg = option auto (long "timeout" <> metavar "msec" <> help "query timeout")
logArg = strOption (long "log" <> short 'l' <> metavar "file" <> help "log file")
logLevelArg = strOption (long "log-level" <> metavar "level" <> help "log level: trace/debug/info/warning/error/fatal")
noColorFlag = switch (long "no-color" <> help "don't use colorized log output")
noFileFlag = switch (long "no-file" <> help "don't use mmap files")
prettyFlag = switch (long "pretty" <> help "pretty json output")
serverSilentFlag = switch (long "silent" <> help "no stdout/stderr")
stdinFlag = switch (long "stdin" <> help "pass data to stdin")
silentFlag = switch (long "silent" <> help "supress notifications")
dbFileArg = strOption (long "db" <> metavar "path" <> help "path to sql database")

serverOptsArgs :: ServerOpts -> [String]
serverOptsArgs sopts = concat [
	portArgs (serverPort sopts),
	["--timeout", show $ serverTimeout sopts],
	marg "--log" (serverLog sopts),
	["--log-level", serverLogLevel sopts],
	marg "--db" (serverDbFile sopts),
	["--silent" | serverSilent sopts]]
	where
		marg :: String -> Maybe String -> [String]
		marg n (Just v) = [n, v]
		marg _ _ = []
		portArgs :: ConnectionPort -> [String]
		portArgs (NetworkPort n) = ["--port", show n]
		portArgs (UnixPort s) = ["--unix", s]

data Request = Request {
	requestCommand :: Command,
	requestDirectory :: FilePath,
	requestNoFile :: Bool,
	requestTimeout :: Int,
	requestSilent :: Bool }
		deriving (Show)

instance ToJSON Request where
	toJSON (Request c dir f tm s) = object ["current-directory" .= dir, "no-file" .= f, "timeout" .= tm, "silent" .= s] `objectUnion` toJSON c

instance FromJSON Request where
	parseJSON = withObject "request" $ \v -> Request <$>
		parseJSON (Object v) <*>
		((v .:: "current-directory") <|> pure ".") <*>
		((v .:: "no-file") <|> pure False) <*>
		((v .:: "timeout") <|> pure 0) <*>
		((v .:: "silent") <|> pure False)

-- | Command from client
data Command =
	Ping |
	Listen (Maybe String) |
	SetLogLevel String |
	Scan {
		scanProjects :: [Path],
		scanCabal :: Bool,
		scanSandboxes :: [Path],
		scanFiles :: [FileSource],
		scanPaths :: [Path],
		scanBuildTool :: BuildTool,
		scanGhcOpts :: [String],
		scanDocs :: Bool,
		scanInferTypes :: Bool } |
	ScanProject {
		scanProjectPath :: Path,
		scanProjectBuildTool :: BuildTool,
		scanProjectDeps :: Bool } |
	ScanFile {
		scanFilePath :: Path,
		scanFileBuildTool :: BuildTool,
		scanFileProject :: Bool,
		scanFileDeps :: Bool } |
	ScanPackageDbs {
		scanPackageDbStack :: PackageDbStack } |
	SetFileContents Path (Maybe Text) |
	RefineDocs {
		docsProjects :: [Path],
		docsFiles :: [Path] } |
	InferTypes {
		inferProjects :: [Path],
		inferFiles :: [Path] } |
	Remove {
		removeProjects :: [Path],
		removeCabal :: Bool,
		removeSandboxes :: [Path],
		removeFiles :: [Path] } |
	RemoveAll |
	InfoPackages |
	InfoProjects |
	InfoSandboxes |
	InfoSymbol SearchQuery [TargetFilter] Bool Bool |
	InfoModule SearchQuery [TargetFilter] Bool Bool |
	InfoProject (Either Text Path) |
	InfoSandbox Path |
	Lookup Text Path |
	Whois Text Path |
	Whoat Int Int Path |
	ResolveScopeModules SearchQuery Path |
	ResolveScope SearchQuery Path |
	FindUsages Int Int Path |
	Complete Text Bool Path |
	Hayoo {
		hayooQuery :: String,
		hayooPage :: Int,
		hayooPages :: Int } |
	CabalList { cabalListPackages :: [Text] } |
	UnresolvedSymbols {
		unresolvedFiles :: [Path] } |
	Lint {
		lintFiles :: [FileSource],
		lintHlintOpts :: [String]
	} |
	Check {
		checkFiles :: [FileSource],
		checkGhcOpts :: [String],
		checkClear :: Bool } |
	CheckLint {
		checkLintFiles :: [FileSource],
		checkLintGhcOpts :: [String],
		checkLintOpts :: [String],
		checkLinkClear :: Bool } |
	Types {
		typesFiles :: [FileSource],
		typesGhcOpts :: [String],
		typesClear :: Bool } |
	AutoFix [Note OutputMessage] |
	Refactor [Note Refact] [Note Refact] Bool |
	Rename Text Text Path |
	GhcEval { ghcEvalExpressions :: [String], ghcEvalSource :: Maybe FileSource } |
	GhcType { ghcTypeExpressions :: [String], ghcTypeSource :: Maybe FileSource } |
	Langs |
	Flags |
	Link { linkHold :: Bool } |
	StopGhc |
	Exit
		deriving (Show)

data FileSource = FileSource { fileSource :: Path, fileContents :: Maybe Text } deriving (Show)
data TargetFilter =
	TargetProject Text |
	TargetFile Path |
	TargetModule Text |
	TargetPackage Text |
	TargetInstalled |
	TargetSourced |
	TargetStandalone
		deriving (Eq, Show)
data SearchQuery = SearchQuery Text SearchType deriving (Show)
data SearchType = SearchExact | SearchPrefix | SearchInfix | SearchSuffix deriving (Show)

instance Paths Command where
	paths f (Scan projs c cs fs ps btool ghcs docs infer) = Scan <$>
		traverse (paths f) projs <*>
		pure c <*>
		traverse (paths f) cs <*>
		traverse (paths f) fs <*>
		traverse (paths f) ps <*>
		pure btool <*>
		pure ghcs <*>
		pure docs <*>
		pure infer
	paths f (ScanProject proj tool deps) = ScanProject <$> paths f proj <*> pure tool <*> pure deps
	paths f (ScanFile file' tool scanProj deps) = ScanFile <$> paths f file' <*> pure tool <*> pure scanProj <*> pure deps
	paths f (ScanPackageDbs pdbs) = ScanPackageDbs <$> paths f pdbs
	paths f (SetFileContents p cts) = SetFileContents <$> paths f p <*> pure cts
	paths f (RefineDocs projs fs) = RefineDocs <$> traverse (paths f) projs <*> traverse (paths f) fs
	paths f (InferTypes projs fs) = InferTypes <$> traverse (paths f) projs <*> traverse (paths f) fs
	paths f (Remove projs c cs fs) = Remove <$> traverse (paths f) projs <*> pure c <*> traverse (paths f) cs <*> traverse (paths f) fs
	paths _ RemoveAll = pure RemoveAll
	paths f (InfoSymbol q t h l) = InfoSymbol <$> pure q <*> traverse (paths f) t <*> pure h <*> pure l
	paths f (InfoModule q t h i) = InfoModule <$> pure q <*> traverse (paths f) t <*> pure h <*> pure i
	paths f (InfoProject (Right proj)) = InfoProject <$> (Right <$> paths f proj)
	paths f (InfoSandbox fpath) = InfoSandbox <$> paths f fpath
	paths f (Lookup n fpath) = Lookup <$> pure n <*> paths f fpath
	paths f (Whois n fpath) = Whois <$> pure n <*> paths f fpath
	paths f (Whoat l c fpath) = Whoat <$> pure l <*> pure c <*> paths f fpath
	paths f (ResolveScopeModules q fpath) = ResolveScopeModules q <$> paths f fpath
	paths f (ResolveScope q fpath) = ResolveScope q <$> paths f fpath
	paths f (FindUsages l c fpath) = FindUsages <$> pure l <*> pure c <*> paths f fpath
	paths f (Complete n g fpath) = Complete n g <$> paths f fpath
	paths f (UnresolvedSymbols fs) = UnresolvedSymbols <$> traverse (paths f) fs
	paths f (Lint fs lints) = Lint <$> traverse (paths f) fs <*> pure lints
	paths f (Check fs ghcs c) = Check <$> traverse (paths f) fs <*> pure ghcs <*> pure c
	paths f (CheckLint fs ghcs lints c) = CheckLint <$> traverse (paths f) fs <*> pure ghcs <*> pure lints <*> pure c
	paths f (Types fs ghcs c) = Types <$> traverse (paths f) fs <*> pure ghcs <*> pure c
	paths f (GhcEval e mf) = GhcEval e <$> traverse (paths f) mf
	paths f (GhcType e mf) = GhcType e <$> traverse (paths f) mf
	paths _ c = pure c

instance Paths FileSource where
	paths f (FileSource fpath mcts) = FileSource <$> paths f fpath <*> pure mcts

instance Paths TargetFilter where
	paths f (TargetFile fpath) = TargetFile <$> paths f fpath
	paths _ t = pure t

instance FromCmd Command where
	cmdP = subparser $ mconcat [
		cmd "ping" "ping server" (pure Ping),
		cmd "listen" "listen server log" (Listen <$> optional logLevelArg),
		cmd "set-log" "set log level" (SetLogLevel <$> strArgument idm),
		cmd "scan" "scan sources" (
			subparser (cmd "project" "scan project" (ScanProject <$> textArgument idm <*> toolArg <*> depsArg)) <|>
			subparser (cmd "file" "scan file" (ScanFile <$> textArgument idm <*> (toolArg <|> pure CabalTool) <*> depProjArg <*> depsArg)) <|>
			subparser (cmd "package-dbs" "scan package-dbs; note, that order of package-dbs matters - dependent package-dbs should go first" (ScanPackageDbs <$> (mkPackageDbStack <$> many packageDbArg))) <|>
			(Scan <$>
				many projectArg <*>
				cabalFlag <*>
				many sandboxArg <*>
				many cmdP <*>
				many (pathArg $ help "path") <*>
				(toolArg' <|> pure CabalTool) <*>
				ghcOpts <*>
				docsFlag <*>
				inferFlag)),
		cmd "set-file-contents" "set edited file contents, which will be used instead of contents in file until it updated" $
			SetFileContents <$> fileArg <*> optional contentsArg,
		cmd "docs" "scan docs" $ RefineDocs <$> many projectArg <*> many fileArg,
		cmd "infer" "infer types" $ InferTypes <$> many projectArg <*> many fileArg,
		cmd "remove" "remove modules info" $ Remove <$>
			many projectArg <*>
			cabalFlag <*>
			many sandboxArg <*>
			many fileArg,
		cmd "remove-all" "remove all data" (pure RemoveAll),
		cmd "packages" "list packages" (pure InfoPackages),
		cmd "projects" "list projects" (pure InfoProjects),
		cmd "sandboxes" "list sandboxes" (pure InfoSandboxes),
		cmd "symbol" "get symbol info" (InfoSymbol <$> cmdP <*> many cmdP <*> headerFlag <*> localsFlag),
		cmd "module" "get module info" (InfoModule <$> cmdP <*> many cmdP <*> headerFlag <*> inspectionFlag),
		cmd "project" "get project info" (InfoProject <$> ((Left <$> projectArg) <|> (Right <$> pathArg idm))),
		cmd "sandbox" "get sandbox info" (InfoSandbox <$> pathArg (help "locate sandbox in parent of this path")),
		cmd "lookup" "lookup for symbol" (Lookup <$> textArgument idm <*> ctx),
		cmd "whois" "get info for symbol" (Whois <$> textArgument idm <*> ctx),
		cmd "whoat" "get info for symbol under cursor" (Whoat <$> argument auto (metavar "line") <*> argument auto (metavar "column") <*> ctx),
		cmd "scope" "get declarations accessible from module or within a project" (
			subparser (cmd "modules" "get modules accessible from module or within a project" (ResolveScopeModules <$> cmdP <*> ctx)) <|>
			ResolveScope <$> cmdP <*> ctx),
		cmd "usages" "find usages of symbol within project/module" (FindUsages <$> argument auto (metavar "line") <*> argument auto (metavar "column") <*> ctx),
		cmd "complete" "show completions for input" (Complete <$> textArgument idm <*> wideFlag <*> ctx),
		cmd "hayoo" "find declarations online via Hayoo" (Hayoo <$> strArgument idm <*> hayooPageArg <*> hayooPagesArg),
		cmd "cabal" "cabal commands" (subparser $ cmd "list" "list cabal packages" (CabalList <$> many (textArgument idm))),
		cmd "unresolveds" "list unresolved symbols in source file" (UnresolvedSymbols <$> many fileArg),
		cmd "lint" "lint source files or file contents" (Lint <$> many cmdP <*> lintOpts),
		cmd "check" "check source files or file contents" (Check <$> many cmdP <*> ghcOpts <*> clearFlag),
		cmd "check-lint" "check and lint source files or file contents" (CheckLint <$> many cmdP <*> ghcOpts <*> lintOpts <*> clearFlag),
		cmd "types" "get types for file expressions" (Types <$> many cmdP <*> ghcOpts <*> clearFlag),
		cmd "autofixes" "get autofixes by output messages" (AutoFix <$> option readJSON (long "data" <> metavar "message" <> help "messages to make fixes for")),
		cmd "refactor" "apply some refactors and get rest updated" (Refactor <$>
			option readJSON (long "data" <> metavar "message" <> help "messages to fix") <*>
			option readJSON (long "rest" <> metavar "correction" <> short 'r' <> help "update corrections") <*>
			pureFlag),
		cmd "rename" "get rename refactors" (Rename <$> textArgument idm <*> textArgument idm <*> ctx),
		cmd "ghc" "ghc commands" (
				subparser (cmd "eval" "evaluate expression" (GhcEval <$> many (strArgument idm) <*> optional cmdP)) <|>
				subparser (cmd "type" "expression type" (GhcType <$> many (strArgument idm) <*> optional cmdP))),
		cmd "langs" "ghc language options" (pure Langs),
		cmd "flags" "ghc flags" (pure Flags),
		cmd "link" "link to server" (Link <$> holdFlag),
		cmd "stop-ghc" "stop ghc sessions" (pure StopGhc),
		cmd "exit" "exit" (pure Exit)]

instance FromCmd FileSource where
	cmdP = option readJSON (long "contents") <|> (FileSource <$> fileArg <*> pure Nothing)

instance FromCmd TargetFilter where
	cmdP = asum [
		TargetProject <$> projectArg,
		TargetFile <$> fileArg,
		TargetModule <$> moduleArg,
		TargetPackage <$> packageArg,
		flag' TargetInstalled (long "installed"),
		flag' TargetSourced (long "src"),
		flag' TargetStandalone (long "stand")]

instance FromCmd SearchQuery where
	cmdP = SearchQuery <$> (textArgument idm <|> pure "") <*> asum [
		flag' SearchExact (long "exact"),
		flag' SearchInfix (long "infix"),
		flag' SearchSuffix (long "suffix"),
		pure SearchPrefix <* switch (long "prefix")]

readJSON :: FromJSON a => ReadM a
readJSON = str >>= maybe (readerError "Can't parse JSON argument") return . decode . L.pack

textOption :: Mod OptionFields String -> Parser Text
textOption = fmap fromString . strOption

textArgument :: Mod ArgumentFields String -> Parser Text
textArgument = fmap fromString . strArgument

cabalFlag :: Parser Bool
clearFlag :: Parser Bool
contentsArg :: Parser Text
ctx :: Parser Path
depProjArg :: Parser Bool
depsArg :: Parser Bool
docsFlag :: Parser Bool
fileArg :: Parser Path
ghcOpts :: Parser [String]
hayooPageArg :: Parser Int
hayooPagesArg :: Parser Int
headerFlag :: Parser Bool
holdFlag :: Parser Bool
inferFlag :: Parser Bool
inspectionFlag :: Parser Bool
lintOpts :: Parser [String]
localsFlag :: Parser Bool
moduleArg :: Parser Text
packageArg :: Parser Text
packageDbArg :: Parser PackageDb
pathArg :: Mod OptionFields String -> Parser Path
projectArg :: Parser Path
pureFlag :: Parser Bool
sandboxArg :: Parser Path
toolArg :: Parser BuildTool
toolArg' :: Parser BuildTool
wideFlag :: Parser Bool

cabalFlag = switch (long "cabal")
clearFlag = switch (long "clear" <> short 'c' <> help "clear run, drop previous state")
contentsArg = textOption (long "contents" <> help "text contents")
ctx = fileArg
depProjArg = fmap not $ switch (long "no-project" <> help "don't scan related project")
depsArg = fmap not $ switch (long "no-deps" <> help "don't scan dependent package-dbs")
docsFlag = switch (long "docs" <> help "scan source file docs")
fileArg = textOption (long "file" <> metavar "path" <> short 'f')
ghcOpts = many (strOption (long "ghc" <> metavar "option" <> short 'g' <> help "options to pass to GHC"))
hayooPageArg = option auto (long "page" <> metavar "n" <> short 'p' <> help "page number (0 by default)" <> value 0)
hayooPagesArg = option auto (long "pages" <> metavar "count" <> short 'n' <> help "pages count (1 by default)" <> value 1)
headerFlag = switch (long "header" <> short 'h' <> help "show only header of module")
holdFlag = switch (long "hold" <> short 'h' <> help "don't return any response")
inferFlag = switch (long "infer" <> help "infer types")
inspectionFlag = switch (long "inspection" <> short 'i' <> help "return inspection data")
lintOpts = many (strOption (long "lint" <> metavar "option" <> short 'l' <> help "options for hlint"))
localsFlag = switch (long "locals" <> short 'l' <> help "look in local declarations")
moduleArg = textOption (long "module" <> metavar "name" <> short 'm' <> help "module name")
packageArg = textOption (long "package" <> metavar "name" <> help "module package")
packageDbArg =
	flag' GlobalDb (long "global-db" <> help "global package-db") <|>
	flag' UserDb (long "user-db" <> help "per-user package-db") <|>
	fmap PackageDb (textOption (long "package-db" <> metavar "path" <> help "custom package-db"))
pathArg f = textOption (long "path" <> metavar "path" <> short 'p' <> f)
projectArg = textOption (long "project" <> long "proj" <> metavar "project")
pureFlag = switch (long "pure" <> help "don't modify actual file, just return result")
sandboxArg = textOption (long "sandbox" <> metavar "path" <> help "path to cabal sandbox")
toolArg =
	flag' CabalTool (long "cabal" <> help "use cabal as build tool") <|>
	flag' StackTool (long "stack" <> help "use stack as build tool") <|>
	toolArg'
toolArg' = option readTool (long "tool" <> help "specify build tool, `cabal` or `stack`") where
	readTool :: ReadM BuildTool
	readTool = do
		s <- str @String
		msum [
			guard (s == "cabal") >> return CabalTool,
			guard (s == "stack") >> return StackTool,
			readerError ("unknown build tool: {}" ~~ s)]

wideFlag = switch (long "wide" <> short 'w' <> help "wide mode - complete as if there were no import lists")

instance ToJSON Command where
	toJSON Ping = cmdJson "ping" []
	toJSON (Listen lev) = cmdJson "listen" ["level" .= lev]
	toJSON (SetLogLevel lev) = cmdJson "set-log" ["level" .= lev]
	toJSON (Scan projs cabal sboxes fs ps btool ghcs docs' infer') = cmdJson "scan" [
		"projects" .= projs,
		"cabal" .= cabal,
		"sandboxes" .= sboxes,
		"files" .= fs,
		"paths" .= ps,
		"build-tool" .= btool,
		"ghc-opts" .= ghcs,
		"docs" .= docs',
		"infer" .= infer']
	toJSON (ScanProject proj tool deps) = cmdJson "scan project" [
		"project" .= proj,
		"build-tool" .= tool,
		"scan-deps" .= deps]
	toJSON (ScanFile file' tool scanProj deps) = cmdJson "scan file" [
		"file" .= file',
		"build-tool" .= tool,
		"scan-project" .= scanProj,
		"scan-deps" .= deps]
	toJSON (ScanPackageDbs pdbs) = cmdJson "scan package-dbs" [
		"package-db-stack" .= pdbs]
	toJSON (SetFileContents f cts) = cmdJson "set-file-contents" ["file" .= f, "contents" .= cts]
	toJSON (RefineDocs projs fs) = cmdJson "docs" ["projects" .= projs, "files" .= fs]
	toJSON (InferTypes projs fs) = cmdJson "infer" ["projects" .= projs, "files" .= fs]
	toJSON (Remove projs cabal sboxes fs) = cmdJson "remove" ["projects" .= projs, "cabal" .= cabal, "sandboxes" .= sboxes, "files" .= fs]
	toJSON RemoveAll = cmdJson "remove-all" []
	toJSON InfoPackages = cmdJson "packages" []
	toJSON InfoProjects = cmdJson "projects" []
	toJSON InfoSandboxes = cmdJson "sandboxes" []
	toJSON (InfoSymbol q tf h l) = cmdJson "symbol" ["query" .= q, "filters" .= tf, "header" .= h, "locals" .= l]
	toJSON (InfoModule q tf h i) = cmdJson "module" ["query" .= q, "filters" .= tf, "header" .= h, "inspection" .= i]
	toJSON (InfoProject p) = cmdJson "project" $ either (\pname -> ["name" .= pname]) (\ppath -> ["path" .= ppath]) p
	toJSON (InfoSandbox p) = cmdJson "sandbox" ["path" .= p]
	toJSON (Lookup n f) = cmdJson "lookup" ["name" .= n, "file" .= f]
	toJSON (Whois n f) = cmdJson "whois" ["name" .= n, "file" .= f]
	toJSON (Whoat l c f) = cmdJson "whoat" ["line" .= l, "column" .= c, "file" .= f]
	toJSON (ResolveScopeModules q f) = cmdJson "scope modules" ["query" .= q, "file" .= f]
	toJSON (ResolveScope q f) = cmdJson "scope" ["query" .= q, "file" .= f]
	toJSON (FindUsages l c f) = cmdJson "usages" ["line" .= l, "column" .= c, "file" .= f]
	toJSON (Complete q w f) = cmdJson "complete" ["prefix" .= q, "wide" .= w, "file" .= f]
	toJSON (Hayoo q p ps) = cmdJson "hayoo" ["query" .= q, "page" .= p, "pages" .= ps]
	toJSON (CabalList ps) = cmdJson "cabal list" ["packages" .= ps]
	toJSON (UnresolvedSymbols fs) = cmdJson "unresolveds" ["files" .= fs]
	toJSON (Lint fs lints) = cmdJson "lint" ["files" .= fs, "lint-opts" .= lints]
	toJSON (Check fs ghcs c) = cmdJson "check" ["files" .= fs, "ghc-opts" .= ghcs, "clear" .= c]
	toJSON (CheckLint fs ghcs lints c) = cmdJson "check-lint" ["files" .= fs, "ghc-opts" .= ghcs, "lint-opts" .= lints, "clear" .= c]
	toJSON (Types fs ghcs c) = cmdJson "types" ["files" .= fs, "ghc-opts" .= ghcs, "clear" .= c]
	toJSON (AutoFix ns) = cmdJson "autofixes" ["messages" .= ns]
	toJSON (Refactor ns rests pure') = cmdJson "refactor" ["messages" .= ns, "rest" .= rests, "pure" .= pure']
	toJSON (Rename n n' f) = cmdJson "rename" ["name" .= n, "new-name" .= n', "file" .= f]
	toJSON (GhcEval exprs f) = cmdJson "ghc eval" ["exprs" .= exprs, "file" .= f]
	toJSON (GhcType exprs f) = cmdJson "ghc type" ["exprs" .= exprs, "file" .= f]
	toJSON Langs = cmdJson "langs" []
	toJSON Flags = cmdJson "flags" []
	toJSON (Link h) = cmdJson "link" ["hold" .= h]
	toJSON StopGhc = cmdJson "stop-ghc" []
	toJSON Exit = cmdJson "exit" []

instance FromJSON Command where
	parseJSON = withObject "command" $ \v -> asum [
		guardCmd "ping" v *> pure Ping,
		guardCmd "listen" v *> (Listen <$> v .::? "level"),
		guardCmd "set-log" v *> (SetLogLevel <$> v .:: "level"),
		guardCmd "scan" v *> (Scan <$>
			v .::?! "projects" <*>
			(v .:: "cabal" <|> pure False) <*>
			v .::?! "sandboxes" <*>
			v .::?! "files" <*>
			v .::?! "paths" <*>
			(v .:: "build-tool" <|> pure CabalTool) <*>
			v .::?! "ghc-opts" <*>
			(v .:: "docs" <|> pure False) <*>
			(v .:: "infer" <|> pure False)),
		guardCmd "scan project" v *> (ScanProject <$>
			v .:: "project" <*>
			v .:: "build-tool" <*>
			(v .:: "scan-deps" <|> pure True)),
		guardCmd "scan file" v *> (ScanFile <$>
			v .:: "file" <*>
			(v .:: "build-tool" <|> pure CabalTool) <*>
			(v .:: "scan-project" <|> pure True) <*>
			(v .:: "scan-deps" <|> pure True)),
		guardCmd "scan package-dbs" v *> (ScanPackageDbs <$> v .:: "package-db-stack"),
		guardCmd "set-file-contents" v *> (SetFileContents <$> v .:: "file" <*> v .:: "contents"),
		guardCmd "docs" v *> (RefineDocs <$> v .::?! "projects" <*> v .::?! "files"),
		guardCmd "infer" v *> (InferTypes <$> v .::?! "projects" <*> v .::?! "files"),
		guardCmd "remove" v *> (Remove <$>
			v .::?! "projects" <*>
			(v .:: "cabal" <|> pure False) <*>
			v .::?! "sandboxes" <*>
			v .::?! "files"),
		guardCmd "remove-all" v *> pure RemoveAll,
		guardCmd "packages" v *> pure InfoPackages,
		guardCmd "projects" v *> pure InfoProjects,
		guardCmd "sandboxes" v *> pure InfoSandboxes,
		guardCmd "symbol" v *> (InfoSymbol <$> v .:: "query" <*> v .::?! "filters" <*> v .:: "header" <*> (v .:: "locals" <|> pure False)),
		guardCmd "module" v *> (InfoModule <$> v .:: "query" <*> v .::?! "filters" <*> v .:: "header" <*> v .:: "inspection"),
		guardCmd "project" v *> (InfoProject <$> asum [Left <$> v .:: "name", Right <$> v .:: "path"]),
		guardCmd "sandbox" v *> (InfoSandbox <$> v .:: "path"),
		guardCmd "lookup" v *> (Lookup <$> v .:: "name" <*> v .:: "file"),
		guardCmd "whois" v *> (Whois <$> v .:: "name" <*> v .:: "file"),
		guardCmd "whoat" v *> (Whoat <$> v .:: "line" <*> v .:: "column" <*> v .:: "file"),
		guardCmd "scope modules" v *> (ResolveScopeModules <$> v .:: "query" <*> v .:: "file"),
		guardCmd "scope" v *> (ResolveScope <$> v .:: "query" <*> v .:: "file"),
		guardCmd "usages" v *> (FindUsages <$> v .:: "line" <*> v .:: "column" <*> v .:: "file"),
		guardCmd "complete" v *> (Complete <$> v .:: "prefix" <*> (v .:: "wide" <|> pure False) <*> v .:: "file"),
		guardCmd "hayoo" v *> (Hayoo <$> v .:: "query" <*> (v .:: "page" <|> pure 0) <*> (v .:: "pages" <|> pure 1)),
		guardCmd "cabal list" v *> (CabalList <$> v .::?! "packages"),
		guardCmd "unresolveds" v *> (UnresolvedSymbols <$> v .::?! "files"),
		guardCmd "lint" v *> (Lint <$> v .::?! "files" <*> v .::?! "lint-opts"),
		guardCmd "check" v *> (Check <$> v .::?! "files" <*> v .::?! "ghc-opts" <*> (v .:: "clear" <|> pure False)),
		guardCmd "check-lint" v *> (CheckLint <$> v .::?! "files" <*> v .::?! "ghc-opts" <*> v .::?! "lint-opts" <*> (v .:: "clear" <|> pure False)),
		guardCmd "types" v *> (Types <$> v .::?! "files" <*> v .::?! "ghc-opts" <*> (v .:: "clear" <|> pure False)),
		guardCmd "autofixes" v *> (AutoFix <$> v .:: "messages"),
		guardCmd "refactor" v *> (Refactor <$> v .:: "messages" <*> v .::?! "rest" <*> (v .:: "pure" <|> pure True)),
		guardCmd "rename" v *> (Rename <$> v .:: "name" <*> v .:: "new-name" <*> v .:: "file"),
		guardCmd "ghc eval" v *> (GhcEval <$> v .::?! "exprs" <*> v .::? "file"),
		guardCmd "ghc type" v *> (GhcType <$> v .::?! "exprs" <*> v .::? "file"),
		guardCmd "langs" v *> pure Langs,
		guardCmd "flags" v *> pure Flags,
		guardCmd "link" v *> (Link <$> (v .:: "hold" <|> pure False)),
		guardCmd "stop-ghc" v *> pure StopGhc,
		guardCmd "exit" v *> pure Exit]

instance ToJSON FileSource where
	toJSON (FileSource fpath mcts) = object ["file" .= fpath, "contents" .= mcts]

instance FromJSON FileSource where
	parseJSON = withObject "file-contents" $ \v -> FileSource <$> v .:: "file" <*> v .::? "contents"

instance ToJSON TargetFilter where
	toJSON (TargetProject pname) = object ["project" .= pname]
	toJSON (TargetFile fpath) = object ["file" .= fpath]
	toJSON (TargetModule mname) = object ["module" .= mname]
	toJSON (TargetPackage pkg) = object ["package" .= pkg]
	toJSON TargetInstalled = toJSON ("installed" :: String)
	toJSON TargetSourced = toJSON ("sourced" :: String)
	toJSON TargetStandalone = toJSON ("standalone" :: String)

instance FromJSON TargetFilter where
	parseJSON j = obj j <|> str' where
		obj = withObject "target-filter" $ \v -> asum [
			TargetProject <$> v .:: "project",
			TargetFile <$> v .:: "file",
			TargetModule <$> v .:: "module",
			TargetPackage <$> v .:: "package"]
		str' = do
			s <- parseJSON j :: A.Parser String
			case s of
				"installed" -> return TargetInstalled
				"sourced" -> return TargetSourced
				"standalone" -> return TargetStandalone
				_ -> empty

instance ToJSON SearchQuery where
	toJSON (SearchQuery q st) = object ["input" .= q, "type" .= st]

instance FromJSON SearchQuery where
	parseJSON = withObject "search-query" $ \v -> SearchQuery <$> (v .:: "input" <|> pure "") <*> (v .:: "type" <|> pure SearchPrefix)

instance ToJSON SearchType where
	toJSON SearchExact = toJSON ("exact" :: String)
	toJSON SearchPrefix = toJSON ("prefix" :: String)
	toJSON SearchInfix = toJSON ("infix" :: String)
	toJSON SearchSuffix = toJSON ("suffix" :: String)

instance FromJSON SearchType where
	parseJSON v = do
		str' <- parseJSON v :: A.Parser String
		case str' of
			"exact" -> return SearchExact
			"prefix" -> return SearchPrefix
			"infix" -> return SearchInfix
			"suffix" -> return SearchInfix
			_ -> empty
