{-# LANGUAGE OverloadedStrings, CPP, TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses, TypeFamilies, ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Server.Types (
	ServerMonadBase,
	SessionLog(..), Session(..), SessionMonad(..), askSession, ServerM(..),
	CommandOptions(..), CommandMonad(..), askOptions, ClientM(..),
	withSession, serverListen, serverSetLogRules, serverWait, serverUpdateDB, serverWriteCache, serverReadCache, inSessionGhc, serverExit, commandRoot, commandNotify, commandLink, commandHold,
	ServerCommand(..), ConnectionPort(..), ServerOpts(..), silentOpts, ClientOpts(..), serverOptsArgs, Request(..),

	Command(..),
	AutoFixCommand(..),
	FileSource(..), TargetFilter(..), SearchQuery(..), SearchType(..),
	FromCmd(..),
	) where

import Control.Applicative
import Control.Concurrent.MVar (MVar, swapMVar)
import Control.Concurrent.Worker
import Control.Lens (each)
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Control
import Data.Aeson hiding (Result(..), Error)
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Default
import Data.Foldable (asum)
import Options.Applicative
import System.Log.Simple hiding (Command)

import System.Directory.Paths
import Text.Format (FormatBuild(..))

import HsDev.Database
import qualified HsDev.Database.Async as DB
import HsDev.Error (hsdevError)
import HsDev.Symbols
import HsDev.Server.Message
import HsDev.Watcher.Types (Watcher)
import HsDev.Tools.Ghc.Worker (GhcWorker, GhcM)
import HsDev.Tools.Types (Note, OutputMessage)
import HsDev.Tools.AutoFix (Correction)
import HsDev.Types (HsDevError(..))
import HsDev.Util

#if mingw32_HOST_OS
import System.Win32.FileMapping.NamePool (Pool)
#endif

type ServerMonadBase m = (MonadIO m, MonadMask m, MonadBaseControl IO m, Alternative m, MonadPlus m)

data SessionLog = SessionLog {
	sessionLogger :: Log,
	sessionLogRules :: MVar [String],
	sessionListenLog :: IO [String],
	sessionLogWait :: IO () }

data Session = Session {
	sessionDatabase :: DB.Async Database,
	sessionWriteCache :: Database -> ServerM IO (),
	sessionReadCache :: (FilePath -> ExceptT String IO Structured) -> ServerM IO (Maybe Database),
	sessionLog :: SessionLog,
	sessionWatcher :: Watcher,
#if mingw32_HOST_OS
	sessionMmapPool :: Maybe Pool,
#endif
	sessionGhc :: GhcWorker,
	sessionExit :: IO (),
	sessionWait :: IO (),
	sessionDefines :: [(String, String)] }

class (ServerMonadBase m, MonadLog m) => SessionMonad m where
	getSession :: m Session

askSession :: SessionMonad m => (Session -> a) -> m a
askSession f = liftM f getSession

newtype ServerM m a = ServerM { runServerM :: ReaderT Session m a }
	deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO, MonadReader Session, MonadTrans, MonadThrow, MonadCatch, MonadMask)

instance (MonadIO m, MonadMask m) => MonadLog (ServerM m) where
	askLog = ServerM $ asks (sessionLogger . sessionLog)

instance ServerMonadBase m => SessionMonad (ServerM m) where
	getSession = ask

instance MonadBase b m => MonadBase b (ServerM m) where
	liftBase = ServerM . liftBase

instance MonadBaseControl b m => MonadBaseControl b (ServerM m) where
	type StM (ServerM m) a = StM (ReaderT Session m) a
	liftBaseWith f = ServerM $ liftBaseWith (\f' -> f (f' . runServerM))
	restoreM = ServerM . restoreM

instance SessionMonad m => SessionMonad (ReaderT r m) where
	getSession = lift getSession

instance (SessionMonad m, Monoid w) => SessionMonad (WriterT w m) where
	getSession = lift getSession

instance SessionMonad m => SessionMonad (StateT s m) where
	getSession = lift getSession

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

instance ServerMonadBase m => SessionMonad (ClientM m) where
	getSession = ClientM getSession

instance ServerMonadBase m => CommandMonad (ClientM m) where
	getOptions = ClientM $ lift ask

instance MonadBase b m => MonadBase b (ClientM m) where
	liftBase = ClientM . liftBase

instance MonadBaseControl b m => MonadBaseControl b (ClientM m) where
	type StM (ClientM m) a = StM (ServerM (ReaderT CommandOptions m)) a
	liftBaseWith f = ClientM $ liftBaseWith (\f' -> f (f' . runClientM))
	restoreM = ClientM . restoreM

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
serverListen :: SessionMonad m => m [String]
serverListen = join . liftM liftIO $ askSession (sessionListenLog . sessionLog)

-- | Set server's log config
serverSetLogRules :: SessionMonad m => [String] -> m [String]
serverSetLogRules rs = do
	rvar <- askSession (sessionLogRules . sessionLog)
	liftIO $ swapMVar rvar rs

-- | Wait for server
serverWait :: SessionMonad m => m ()
serverWait = join . liftM liftIO $ askSession sessionWait

-- | Update database
serverUpdateDB :: SessionMonad m => Database -> m ()
serverUpdateDB db = askSession sessionDatabase >>= (`DB.update` return db)

-- | Server write cache
serverWriteCache :: SessionMonad m => Database -> m ()
serverWriteCache db = do
	s <- getSession
	write' <- askSession sessionWriteCache
	liftIO $ withSession s $ write' db

-- | Server read cache
serverReadCache :: SessionMonad m => (FilePath -> ExceptT String IO Structured) -> m (Maybe Database)
serverReadCache act = do
	s <- getSession
	read' <- askSession sessionReadCache
	liftIO $ withSession s $ read' act

-- | In ghc session
inSessionGhc :: SessionMonad m => GhcM a -> m a
inSessionGhc act = do
	ghcw <- askSession sessionGhc
	inWorkerWith (hsdevError . GhcError . displayException) ghcw act

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

instance FormatBuild ConnectionPort

-- | Server options
data ServerOpts = ServerOpts {
	serverPort :: ConnectionPort,
	serverTimeout :: Int,
	serverLog :: Maybe FilePath,
	serverLogConfig :: String,
	serverCache :: Maybe FilePath,
	serverLoad :: Bool,
	serverSilent :: Bool }
		deriving (Show)

instance Default ServerOpts where
	def = ServerOpts def 0 Nothing "use default" Nothing False False

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
		(logConfigArg <|> pure (serverLogConfig def)) <*>
		optional cacheArg <*>
		loadFlag <*>
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
logConfigArg :: Parser String
cacheArg :: Parser FilePath
noFileFlag :: Parser Bool
loadFlag :: Parser Bool
prettyFlag :: Parser Bool
serverSilentFlag :: Parser Bool
stdinFlag :: Parser Bool
silentFlag :: Parser Bool

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
logConfigArg = strOption (long "log-config" <> metavar "rule" <> help "log config: low [low], high [high], set [low] [high], use [default/debug/trace/silent/supress]")
cacheArg = strOption (long "cache" <> metavar "path" <> help "cache directory")
noFileFlag = switch (long "no-file" <> help "don't use mmap files")
loadFlag = switch (long "load" <> help "force load all data from cache on startup")
prettyFlag = switch (long "pretty" <> help "pretty json output")
serverSilentFlag = switch (long "silent" <> help "no stdout/stderr")
stdinFlag = switch (long "stdin" <> help "pass data to stdin")
silentFlag = switch (long "silent" <> help "supress notifications")

serverOptsArgs :: ServerOpts -> [String]
serverOptsArgs sopts = concat [
	portArgs (serverPort sopts),
	["--timeout", show $ serverTimeout sopts],
	marg "--log" (serverLog sopts),
	["--log-config", serverLogConfig sopts],
	marg "--cache" (serverCache sopts),
	["--load" | serverLoad sopts],
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
	SetLogConfig [String] |
	AddData { addedData :: [FilePath] } |
	Scan {
		scanProjects :: [FilePath],
		scanCabal :: Bool,
		scanSandboxes :: [FilePath],
		scanFiles :: [FileSource],
		scanPaths :: [FilePath],
		scanGhcOpts :: [String],
		scanDocs :: Bool,
		scanInferTypes :: Bool } |
	RefineDocs {
		docsProjects :: [FilePath],
		docsFiles :: [FilePath],
		docsModules :: [String] } |
	InferTypes {
		inferProjects :: [FilePath],
		inferFiles :: [FilePath],
		inferModules :: [String] } |
	Remove {
		removeProjects :: [FilePath],
		removeCabal :: Bool,
		removeSandboxes :: [FilePath],
		removeFiles :: [FilePath] } |
	RemoveAll |
	InfoPackages |
	InfoProjects |
	InfoSandboxes |
	InfoSymbol SearchQuery [TargetFilter] Bool |
	InfoModule SearchQuery [TargetFilter] Bool Bool |
	InfoProject (Either String FilePath) |
	InfoSandbox FilePath |
	Lookup String FilePath |
	Whois String FilePath |
	ResolveScopeModules SearchQuery FilePath |
	ResolveScope SearchQuery FilePath |
	Complete String Bool FilePath |
	Hayoo {
		hayooQuery :: String,
		hayooPage :: Int,
		hayooPages :: Int } |
	CabalList { cabalListPackages :: [String] } |
	Lint {
		lintFiles :: [FileSource] } |
	Check {
		checkFiles :: [FileSource],
		checkGhcOpts :: [String] } |
	CheckLint {
		checkLintFiles :: [FileSource],
		checkLintGhcOpts :: [String] } |
	Types {
		typesFiles :: [FileSource],
		typesGhcOpts :: [String] } |
	AutoFix { autoFixCommand :: AutoFixCommand } |
	GhcEval { ghcEvalExpressions :: [String], ghcEvalSource :: Maybe FileSource } |
	Langs |
	Flags |
	Link { linkHold :: Bool } |
	Exit
		deriving (Show)

data AutoFixCommand =
	AutoFixShow [Note OutputMessage] |
	AutoFixFix [Note Correction] [Note Correction] Bool
		deriving (Show)

data FileSource = FileSource { fileSource :: FilePath, fileContents :: Maybe String } deriving (Show)
data TargetFilter =
	TargetProject String |
	TargetFile FilePath |
	TargetModule String |
	TargetDepsOf String |
	TargetPackageDb PackageDb |
	TargetCabal |
	TargetSandbox FilePath |
	TargetPackage String |
	TargetSourced |
	TargetStandalone
		deriving (Eq, Show)
data SearchQuery = SearchQuery String SearchType deriving (Show)
data SearchType = SearchExact | SearchPrefix | SearchInfix | SearchSuffix | SearchRegex deriving (Show)

instance Paths Command where
	paths f (Scan projs c cs fs ps ghcs docs infer) = Scan <$>
		each f projs <*>
		pure c <*>
		(each . paths) f cs <*>
		(each . paths) f fs <*>
		each f ps <*>
		pure ghcs <*>
		pure docs <*>
		pure infer
	paths f (RefineDocs projs fs ms) = RefineDocs <$> each f projs <*> each f fs <*> pure ms
	paths f (InferTypes projs fs ms) = InferTypes <$> each f projs <*> each f fs <*> pure ms
	paths f (Remove projs c cs fs) = Remove <$> each f projs <*> pure c <*> (each . paths) f cs <*> each f fs
	paths _ RemoveAll = pure RemoveAll
	paths f (InfoSymbol q t l) = InfoSymbol <$> pure q <*> paths f t <*> pure l
	paths f (InfoModule q t h i) = InfoModule <$> pure q <*> paths f t <*> pure h <*> pure i
	paths f (InfoProject (Right proj)) = InfoProject <$> (Right <$> f proj)
	paths f (InfoSandbox fpath) = InfoSandbox <$> f fpath
	paths f (Lookup n fpath) = Lookup <$> pure n <*> f fpath
	paths f (Whois n fpath) = Whois <$> pure n <*> f fpath
	paths f (ResolveScopeModules q fpath) = ResolveScopeModules q <$> f fpath
	paths f (ResolveScope q fpath) = ResolveScope q <$> f fpath
	paths f (Complete n g fpath) = Complete n g <$> f fpath
	paths f (Lint fs) = Lint <$> (each . paths) f fs
	paths f (Check fs ghcs) = Check <$> (each . paths) f fs <*> pure ghcs
	paths f (CheckLint fs ghcs) = CheckLint <$> (each . paths) f fs <*> pure ghcs
	paths f (Types fs ghcs) = Types <$> (each . paths) f fs <*> pure ghcs
	paths f (GhcEval e mf) = GhcEval e <$> traverse (paths f) mf
	paths _ c = pure c

instance Paths FileSource where
	paths f (FileSource fpath mcts) = FileSource <$> f fpath <*> pure mcts

instance Paths TargetFilter where
	paths f (TargetFile fpath) = TargetFile <$> f fpath
	paths f (TargetPackageDb pdb) = TargetPackageDb <$> paths f pdb
	paths f (TargetSandbox c) = TargetSandbox <$> paths f c
	paths _ t = pure t

instance Paths [TargetFilter] where
	paths = each . paths

instance FromCmd Command where
	cmdP = subparser $ mconcat [
		cmd "ping" "ping server" (pure Ping),
		cmd "listen" "listen server log" (Listen <$> optional ruleArg),
		cmd "set-log" "set log config rules" (SetLogConfig <$> many (strArgument idm)),
		cmd "add" "add info to database" (AddData <$> many fileArg),
		cmd "scan" "scan sources" $ Scan <$>
			many projectArg <*>
			cabalFlag <*>
			many sandboxArg <*>
			many cmdP <*>
			many (pathArg $ help "path") <*>
			ghcOpts <*>
			docsFlag <*>
			inferFlag,
		cmd "docs" "scan docs" $ RefineDocs <$> many projectArg <*> many fileArg <*> many moduleArg,
		cmd "infer" "infer types" $ InferTypes <$> many projectArg <*> many fileArg <*> many moduleArg,
		cmd "remove" "remove modules info" $ Remove <$>
			many projectArg <*>
			cabalFlag <*>
			many sandboxArg <*>
			many fileArg,
		cmd "remove-all" "remove all data" (pure RemoveAll),
		cmd "packages" "list packages" (pure InfoPackages),
		cmd "projects" "list projects" (pure InfoProjects),
		cmd "sandboxes" "list sandboxes" (pure InfoSandboxes),
		cmd "symbol" "get symbol info" (InfoSymbol <$> cmdP <*> many cmdP <*> localsFlag),
		cmd "module" "get module info" (InfoModule <$> cmdP <*> many cmdP <*> headerFlag <*> inspectionFlag),
		cmd "project" "get project info" (InfoProject <$> ((Left <$> projectArg) <|> (Right <$> pathArg idm))),
		cmd "sandbox" "get sandbox info" (InfoSandbox <$> pathArg (help "locate sandbox in parent of this path")),
		cmd "lookup" "lookup for symbol" (Lookup <$> strArgument idm <*> ctx),
		cmd "whois" "get info for symbol" (Whois <$> strArgument idm <*> ctx),
		cmd "scope" "get declarations accessible from module or within a project" (
			subparser (cmd "modules" "get modules accessible from module or within a project" (ResolveScopeModules <$> cmdP <*> ctx)) <|>
			ResolveScope <$> cmdP <*> ctx),
		cmd "complete" "show completions for input" (Complete <$> strArgument idm <*> wideFlag <*> ctx),
		cmd "hayoo" "find declarations online via Hayoo" (Hayoo <$> strArgument idm <*> hayooPageArg <*> hayooPagesArg),
		cmd "cabal" "cabal commands" (subparser $ cmd "list" "list cabal packages" (CabalList <$> many (strArgument idm))),
		cmd "lint" "lint source files or file contents" (Lint <$> many cmdP),
		cmd "check" "check source files or file contents" (Check <$> many cmdP <*> ghcOpts),
		cmd "check-lint" "check and lint source files or file contents" (CheckLint <$> many cmdP <*> ghcOpts),
		cmd "types" "get types for file expressions" (Types <$> many cmdP <*> ghcOpts),
		cmd "autofix" "autofix commands" (AutoFix <$> cmdP),
		cmd "ghc" "ghc commands" (subparser $ cmd "eval" "evaluate expression" (GhcEval <$> many (strArgument idm) <*> optional cmdP)),
		cmd "langs" "ghc language options" (pure Langs),
		cmd "flags" "ghc flags" (pure Flags),
		cmd "link" "link to server" (Link <$> holdFlag),
		cmd "exit" "exit" (pure Exit)]

instance FromCmd AutoFixCommand where
	cmdP = subparser $ mconcat [
		cmd "show" "generate corrections for check & lint messages" (AutoFixShow <$> option readJSON (long "data" <> metavar "message" <> help "messages to make fixes for")),
		cmd "fix" "fix errors and return rest corrections with updated regions" (AutoFixFix <$>
			option readJSON (long "data" <> metavar "message" <> help "messages to fix") <*>
			option readJSON (long "rest" <> metavar "correction" <> short 'r' <> help "update corrections") <*>
			pureFlag)]

instance FromCmd FileSource where
	cmdP = option readJSON (long "contents") <|> (FileSource <$> fileArg <*> pure Nothing)

instance FromCmd TargetFilter where
	cmdP = asum [
		TargetProject <$> projectArg,
		TargetFile <$> fileArg,
		TargetModule <$> moduleArg,
		TargetDepsOf <$> depsArg,
		TargetPackageDb <$> packageDbArg,
		flag' TargetCabal (long "cabal"),
		TargetSandbox <$> sandboxArg,
		TargetPackage <$> packageArg,
		flag' TargetSourced (long "src"),
		flag' TargetStandalone (long "stand")]

instance FromCmd SearchQuery where
	cmdP = SearchQuery <$> (strArgument idm <|> pure "") <*> asum [
		flag' SearchExact (long "exact"),
		flag' SearchRegex (long "regex"),
		flag' SearchInfix (long "infix"),
		flag' SearchSuffix (long "suffix"),
		pure SearchPrefix <* switch (long "prefix")]

readJSON :: FromJSON a => ReadM a
readJSON = str >>= maybe (readerError "Can't parse JSON argument") return . decode . L.pack

cabalFlag :: Parser Bool
ctx :: Parser FilePath
depsArg :: Parser String
docsFlag :: Parser Bool
fileArg :: Parser FilePath
ghcOpts :: Parser [String]
hayooPageArg :: Parser Int
hayooPagesArg :: Parser Int
headerFlag :: Parser Bool
holdFlag :: Parser Bool
inferFlag :: Parser Bool
inspectionFlag :: Parser Bool
localsFlag :: Parser Bool
moduleArg :: Parser String
packageDbArg :: Parser PackageDb
packageArg :: Parser String
pathArg :: Mod OptionFields String -> Parser FilePath
projectArg :: Parser String
pureFlag :: Parser Bool
ruleArg :: Parser String
sandboxArg :: Parser FilePath
wideFlag :: Parser Bool

cabalFlag = switch (long "cabal")
ctx = fileArg
depsArg = strOption (long "deps" <> metavar "object" <> help "filter to such that in dependency of specified object (file or project)")
docsFlag = switch (long "docs" <> help "scan source file docs")
fileArg = strOption (long "file" <> metavar "path" <> short 'f')
ghcOpts = many (strOption (long "ghc" <> metavar "option" <> short 'g' <> help "options to pass to GHC"))
hayooPageArg = option auto (long "page" <> metavar "n" <> short 'p' <> help "page number (0 by default)" <> value 0)
hayooPagesArg = option auto (long "pages" <> metavar "count" <> short 'n' <> help "pages count (1 by default)" <> value 1)
headerFlag = switch (long "header" <> short 'h' <> help "show only header of module")
holdFlag = switch (long "hold" <> short 'h' <> help "don't return any response")
inferFlag = switch (long "infer" <> help "infer types")
inspectionFlag = switch (long "inspection" <> short 'i' <> help "return inspection data")
localsFlag = switch (long "locals" <> short 'l' <> help "look in local declarations")
moduleArg = strOption (long "module" <> metavar "name" <> short 'm' <> help "module name")
packageArg = strOption (long "package" <> metavar "name" <> help "module package")
packageDbArg =
	flag' GlobalDb (long "global-db" <> help "global package-db") <|>
	flag' UserDb (long "user-db" <> help "user package-db") <|>
	(PackageDb <$> strOption (long "package-db" <> metavar "path" <> help "custom package-db"))
pathArg f = strOption (long "path" <> metavar "path" <> short 'p' <> f)
projectArg = strOption (long "project" <> long "proj" <> metavar "project")
pureFlag = switch (long "pure" <> help "don't modify actual file, just return result")
ruleArg = strOption (long "config" <> metavar "rule" <> help "set new log rules while in listen command")
sandboxArg = strOption (long "sandbox" <> metavar "path" <> help "path to cabal sandbox")
wideFlag = switch (long "wide" <> short 'w' <> help "wide mode - complete as if there were no import lists")

instance ToJSON Command where
	toJSON Ping = cmdJson "ping" []
	toJSON (Listen r) = cmdJson "listen" ["rule" .= r]
	toJSON (SetLogConfig rs) = cmdJson "set-log" ["rules" .= rs]
	toJSON (AddData fs) = cmdJson "add" ["files" .= fs]
	toJSON (Scan projs cabal sboxes fs ps ghcs docs' infer') = cmdJson "scan" [
		"projects" .= projs,
		"cabal" .= cabal,
		"sandboxes" .= sboxes,
		"files" .= fs,
		"paths" .= ps,
		"ghc-opts" .= ghcs,
		"docs" .= docs',
		"infer" .= infer']
	toJSON (RefineDocs projs fs ms) = cmdJson "docs" ["projects" .= projs, "files" .= fs, "modules" .= ms]
	toJSON (InferTypes projs fs ms) = cmdJson "infer" ["projects" .= projs, "files" .= fs, "modules" .= ms]
	toJSON (Remove projs cabal sboxes fs) = cmdJson "remove" ["projects" .= projs, "cabal" .= cabal, "sandboxes" .= sboxes, "files" .= fs]
	toJSON RemoveAll = cmdJson "remove-all" []
	toJSON InfoPackages = cmdJson "packages" []
	toJSON InfoProjects = cmdJson "projects" []
	toJSON InfoSandboxes = cmdJson "sandboxes" []
	toJSON (InfoSymbol q tf l) = cmdJson "symbol" ["query" .= q, "filters" .= tf, "locals" .= l]
	toJSON (InfoModule q tf h i) = cmdJson "module" ["query" .= q, "filters" .= tf, "header" .= h, "inspection" .= i]
	toJSON (InfoProject p) = cmdJson "project" $ either (\pname -> ["name" .= pname]) (\ppath -> ["path" .= ppath]) p
	toJSON (InfoSandbox p) = cmdJson "sandbox" ["path" .= p]
	toJSON (Lookup n f) = cmdJson "lookup" ["name" .= n, "file" .= f]
	toJSON (Whois n f) = cmdJson "whois" ["name" .= n, "file" .= f]
	toJSON (ResolveScopeModules q f) = cmdJson "scope modules" ["query" .= q, "file" .= f]
	toJSON (ResolveScope q f) = cmdJson "scope" ["query" .= q, "file" .= f]
	toJSON (Complete q w f) = cmdJson "complete" ["prefix" .= q, "wide" .= w, "file" .= f]
	toJSON (Hayoo q p ps) = cmdJson "hayoo" ["query" .= q, "page" .= p, "pages" .= ps]
	toJSON (CabalList ps) = cmdJson "cabal list" ["packages" .= ps]
	toJSON (Lint fs) = cmdJson "lint" ["files" .= fs]
	toJSON (Check fs ghcs) = cmdJson "check" ["files" .= fs, "ghc-opts" .= ghcs]
	toJSON (CheckLint fs ghcs) = cmdJson "check-lint" ["files" .= fs, "ghc-opts" .= ghcs]
	toJSON (Types fs ghcs) = cmdJson "types" ["files" .= fs, "ghc-opts" .= ghcs]
	toJSON (AutoFix acmd) = toJSON acmd
	toJSON (GhcEval exprs f) = cmdJson "ghc eval" ["exprs" .= exprs, "file" .= f]
	toJSON Langs = cmdJson "langs" []
	toJSON Flags = cmdJson "flags" []
	toJSON (Link h) = cmdJson "link" ["hold" .= h]
	toJSON Exit = cmdJson "exit" []

instance FromJSON Command where
	parseJSON = withObject "command" $ \v -> asum [
		guardCmd "ping" v *> pure Ping,
		guardCmd "listen" v *> (Listen <$> v .::? "rule"),
		guardCmd "set-log" v *> (SetLogConfig <$> v .:: "rules"),
		guardCmd "add" v *> (AddData <$> v .:: "files"),
		guardCmd "scan" v *> (Scan <$>
			v .::?! "projects" <*>
			(v .:: "cabal" <|> pure False) <*>
			v .::?! "sandboxes" <*>
			v .::?! "files" <*>
			v .::?! "paths" <*>
			v .::?! "ghc-opts" <*>
			(v .:: "docs" <|> pure False) <*>
			(v .:: "infer" <|> pure False)),
		guardCmd "docs" v *> (RefineDocs <$> v .::?! "projects" <*> v .::?! "files" <*> v .::?! "modules"),
		guardCmd "infer" v *> (InferTypes <$> v .::?! "projects" <*> v .::?! "files" <*> v .::?! "modules"),
		guardCmd "remove" v *> (Remove <$>
			v .::?! "projects" <*>
			(v .:: "cabal" <|> pure False) <*>
			v .::?! "sandboxes" <*>
			v .::?! "files"),
		guardCmd "remove-all" v *> pure RemoveAll,
		guardCmd "packages" v *> pure InfoPackages,
		guardCmd "projects" v *> pure InfoProjects,
		guardCmd "sandboxes" v *> pure InfoSandboxes,
		guardCmd "symbol" v *> (InfoSymbol <$> v .:: "query" <*> v .::?! "filters" <*> (v .:: "locals" <|> pure False)),
		guardCmd "module" v *> (InfoModule <$> v .:: "query" <*> v .::?! "filters" <*> v .:: "header" <*> v .:: "inspection"),
		guardCmd "project" v *> (InfoProject <$> asum [Left <$> v .:: "name", Right <$> v .:: "path"]),
		guardCmd "sandbox" v *> (InfoSandbox <$> v .:: "path"),
		guardCmd "lookup" v *> (Lookup <$> v .:: "name" <*> v .:: "file"),
		guardCmd "whois" v *> (Whois <$> v .:: "name" <*> v .:: "file"),
		guardCmd "scope modules" v *> (ResolveScopeModules <$> v .:: "query" <*> v .:: "file"),
		guardCmd "scope" v *> (ResolveScope <$> v .:: "query" <*> v .:: "file"),
		guardCmd "complete" v *> (Complete <$> v .:: "prefix" <*> (v .:: "wide" <|> pure False) <*> v .:: "file"),
		guardCmd "hayoo" v *> (Hayoo <$> v .:: "query" <*> (v .:: "page" <|> pure 0) <*> (v .:: "pages" <|> pure 1)),
		guardCmd "cabal list" v *> (CabalList <$> v .::?! "packages"),
		guardCmd "lint" v *> (Lint <$> v .::?! "files"),
		guardCmd "check" v *> (Check <$> v .::?! "files" <*> v .::?! "ghc-opts"),
		guardCmd "check-lint" v *> (CheckLint <$> v .::?! "files" <*> v .::?! "ghc-opts"),
		guardCmd "types" v *> (Types <$> v .::?! "files" <*> v .::?! "ghc-opts"),
		AutoFix <$> parseJSON (Object v),
		guardCmd "ghc eval" v *> (GhcEval <$> v .::?! "exprs" <*> v .::? "file"),
		guardCmd "langs" v *> pure Langs,
		guardCmd "flags" v *> pure Flags,
		guardCmd "link" v *> (Link <$> (v .:: "hold" <|> pure False)),
		guardCmd "exit" v *> pure Exit]

instance ToJSON AutoFixCommand where
	toJSON (AutoFixShow ns) = cmdJson "autofix show" ["messages" .= ns]
	toJSON (AutoFixFix ns rests pure') = cmdJson "autofix fix" ["messages" .= ns, "rest" .= rests, "pure" .= pure']

instance FromJSON AutoFixCommand where
	parseJSON = withObject "auto-fix-command" $ \v -> asum [
		guardCmd "autofix show" v *> (AutoFixShow <$> v .:: "messages"),
		guardCmd "autofix fix" v *> (AutoFixFix <$> v .:: "messages" <*> v .::?! "rest" <*> (v .:: "pure" <|> pure True))]

instance ToJSON FileSource where
	toJSON (FileSource fpath mcts) = object ["file" .= fpath, "contents" .= mcts]

instance FromJSON FileSource where
	parseJSON = withObject "file-contents" $ \v -> FileSource <$> v .:: "file" <*> v .::? "contents"

instance ToJSON TargetFilter where
	toJSON (TargetProject pname) = object ["project" .= pname]
	toJSON (TargetFile fpath) = object ["file" .= fpath]
	toJSON (TargetModule mname) = object ["module" .= mname]
	toJSON (TargetDepsOf dep) = object ["deps" .= dep]
	toJSON (TargetPackageDb pdb) = object ["db" .= pdb]
	toJSON TargetCabal = toJSON ("cabal" :: String)
	toJSON (TargetSandbox sbox) = object ["sandbox" .= sbox]
	toJSON (TargetPackage pname) = object ["package" .= pname]
	toJSON TargetSourced = toJSON ("sourced" :: String)
	toJSON TargetStandalone = toJSON ("standalone" :: String)

instance FromJSON TargetFilter where
	parseJSON j = obj j <|> str' where
		obj = withObject "target-filter" $ \v -> asum [
			TargetProject <$> v .:: "project",
			TargetFile <$> v .:: "file",
			TargetModule <$> v .:: "module",
			TargetDepsOf <$> v .:: "deps",
			TargetPackageDb <$> v .:: "db",
			TargetSandbox <$> v .:: "sandbox",
			TargetPackage <$> v .:: "package"]
		str' = do
			s <- parseJSON j :: A.Parser String
			case s of
				"cabal" -> return TargetCabal
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
	toJSON SearchRegex = toJSON ("regex" :: String)

instance FromJSON SearchType where
	parseJSON v = do
		str' <- parseJSON v :: A.Parser String
		case str' of
			"exact" -> return SearchExact
			"prefix" -> return SearchPrefix
			"infix" -> return SearchInfix
			"suffix" -> return SearchInfix
			"regex" -> return SearchRegex
			_ -> empty
