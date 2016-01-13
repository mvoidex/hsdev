{-# LANGUAGE OverloadedStrings, CPP, TypeSynonymInstances, FlexibleInstances #-}

module HsDev.Server.Types (
	CommandOptions(..), CommandError(..), commandError_, commandError,
	CommandM,
	ServerCommand(..), ServerOpts(..), ClientOpts(..), serverOptsArgs, Request(..),

	Command(..), AddedContents(..),
	GhcModCommand(..),
	AutoFixCommand(..),
	FileContents(..), TargetFilter(..), SearchQuery(..), SearchType(..),
	FromCmd(..),
	) where

import Control.Applicative
import Control.Lens (each)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson hiding (Result(..), Error)
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Default
import Data.Foldable (asum)
import Options.Applicative
import System.Log.Simple hiding (Command)

import System.Directory.Paths

import HsDev.Database
import qualified HsDev.Database.Async as DB
import HsDev.Project
import HsDev.Symbols
import HsDev.Server.Message
import HsDev.Watcher.Types (Watcher)
import HsDev.Tools.GhcMod (OutputMessage, WorkerMap)
import HsDev.Tools.Ghc.Worker (Worker, Ghc)
import HsDev.Tools.Types (Note)
import HsDev.Tools.AutoFix (Correction)
import HsDev.Util

#if mingw32_HOST_OS
import System.Win32.FileMapping.NamePool (Pool)
#endif

data CommandOptions = CommandOptions {
	commandDatabase :: DB.Async Database,
	commandWriteCache :: Database -> IO (),
	commandReadCache :: (FilePath -> ExceptT String IO Structured) -> IO (Maybe Database),
	commandRoot :: FilePath,
	commandLog :: Level -> String -> IO (),
	commandLogger :: Log,
	commandListenLog :: ([String] -> IO ()) -> IO (),
	commandLogWait :: IO (),
	commandWatcher :: Watcher,
#if mingw32_HOST_OS
	commandMmapPool :: Maybe Pool,
#endif
	commandGhc :: Worker Ghc,
	commandGhci :: Worker Ghc,
	commandGhcMod :: Worker (ReaderT WorkerMap IO),
	commandNotify :: Notification -> IO (),
	commandLink :: IO (),
	commandHold :: IO (),
	commandExit :: IO (),
	commandDefines :: [(String, String)] }

data CommandError = CommandError String [A.Pair]

instance Monoid CommandError where
	mempty = CommandError "" []
	mappend (CommandError lmsg lp) (CommandError rmsg rp) = CommandError (lmsg ++ ", " ++ rmsg) (lp ++ rp)

commandError_ :: String -> ExceptT CommandError IO a
commandError_ m = commandError m []

commandError :: String -> [A.Pair] -> ExceptT CommandError IO a
commandError m ps = throwError $ CommandError m ps

type CommandM a = ExceptT CommandError IO a

-- | Server control command
data ServerCommand =
	Version |
	Start ServerOpts |
	Run ServerOpts |
	Stop ClientOpts |
	Connect ClientOpts |
	Remote ClientOpts Bool Command
		deriving (Show)

-- | Server options
data ServerOpts = ServerOpts {
	serverPort :: Int,
	serverTimeout :: Int,
	serverLog :: Maybe FilePath,
	serverLogConfig :: String,
	serverCache :: Maybe FilePath,
	serverLoad :: Bool }
		deriving (Show)

instance Default ServerOpts where
	def = ServerOpts 4567 0 Nothing "use default" Nothing False

-- | Client options
data ClientOpts = ClientOpts {
	clientPort :: Int,
	clientPretty :: Bool,
	clientStdin :: Bool,
	clientTimeout :: Int,
	clientSilent :: Bool }
		deriving (Show)

instance Default ClientOpts where
	def = ClientOpts 4567 False False 0 False

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
		(portArg <|> pure (serverPort def)) <*>
		(timeoutArg <|> pure (serverTimeout def)) <*>
		optional logArg <*>
		(logConfigArg <|> pure (serverLogConfig def)) <*>
		optional cacheArg <*>
		loadFlag

instance FromCmd ClientOpts where
	cmdP = ClientOpts <$>
		(portArg <|> pure (clientPort def)) <*>
		prettyFlag <*>
		stdinFlag <*>
		(timeoutArg <|> pure (clientTimeout def)) <*>
		silentFlag

portArg = option auto (long "port" <> metavar "number" <> help "connection port")
timeoutArg = option auto (long "timeout" <> metavar "msec" <> help "query timeout")
logArg = strOption (long "log" <> short 'l' <> metavar "file" <> help "log file")
logConfigArg = strOption (long "log-config" <> metavar "rule" <> help "log config: low [low], high [high], set [low] [high], use [default/debug/trace/silent/supress]")
cacheArg = strOption (long "cache" <> metavar "path" <> help "cache directory")
noFileFlag = switch (long "no-file" <> help "don't use mmap files")
loadFlag = switch (long "load" <> help "force load all data from cache on startup")
prettyFlag = switch (long "pretty" <> help "pretty json output")
stdinFlag = switch (long "stdin" <> help "pass data to stdin")
silentFlag = switch (long "silent" <> help "supress notifications")

serverOptsArgs :: ServerOpts -> [String]
serverOptsArgs sopts = concat [
	["--port", show $ serverPort sopts],
	["--timeout", show $ serverTimeout sopts],
	marg "--log" (serverLog sopts),
	["--log-config", serverLogConfig sopts],
	marg "--cache" (serverCache sopts),
	["--load" | serverLoad sopts]]
	where
		marg :: String -> Maybe String -> [String]
		marg n (Just v) = [n, v]
		marg _ _ = []

data Request = Request {
	requestCommand :: Command,
	requestDirectory :: FilePath,
	requestNoFile :: Bool,
	requestTimeout :: Int,
	requestSilent :: Bool }

instance ToJSON Request where
	toJSON (Request c dir f tm s) = object ["current-directory" .= dir, "no-file" .= f, "timeout" .= tm, "silent" .= s] `objectUnion` toJSON c

instance FromJSON Request where
	parseJSON = withObject "request" $ \v -> Request <$>
		parseJSON (Object v) <*>
		((v .:: "current-directory") <|> pure ".") <*>
		((v .:: "no-file") <|> pure False) <*>
		((v .:: "timeout") <|> pure 0) <*>
		((v .:: "silent") <|> pure False)

-- FIXME: Why so much commands in options?
-- | Command from client
data Command =
	Ping |
	Listen |
	AddData { addedContents :: [AddedContents] } |
	Scan {
		scanProjects :: [FilePath],
		scanSandboxes :: [Cabal],
		scanFiles :: [FilePath],
		scanPaths :: [FilePath],
		scanContents :: [FileContents],
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
		removePackages :: [String],
		removeSandboxes :: [Cabal],
		removeFiles :: [FilePath] } |
	InfoModules TargetFilter |
	InfoPackages |
	InfoProjects |
	InfoSandboxes |
	InfoSymbol SearchQuery TargetFilter |
	InfoModule SearchQuery TargetFilter |
	InfoResolve FilePath Bool |
	InfoProject (Either String FilePath) |
	InfoSandbox FilePath |
	Lookup String FilePath |
	Whois String FilePath |
	ResolveScopeModules FilePath |
	ResolveScope SearchQuery Bool FilePath |
	Complete String Bool FilePath |
	Hayoo {
		hayooQuery :: String,
		hayooPage :: Int,
		hayooPages :: Int } |
	CabalList { cabalListPackages :: [String] } |
	Lint {
		lintFiles :: [FilePath],
		lintContents :: [FileContents] } |
	Check {
		checkFiles :: [FilePath],
		checkContents :: [FileContents],
		checkGhcOpts :: [String] } |
	CheckLint {
		checkLintFiles :: [FilePath],
		checkLintContents :: [FileContents],
		checkLintGhcOpts :: [String] } |
	Types {
		typesFiles :: [FilePath],
		typesContents :: [FileContents],
		typesGhcOpts :: [String] } |
	GhcMod { ghcModCommand :: GhcModCommand } |
	AutoFix { autoFixCommand :: AutoFixCommand } |
	GhcEval { ghcEvalExpressions :: [String] } |
	Link { linkHold :: Bool } |
	Exit
		deriving (Show)

data AddedContents =
	AddedDatabase Database |
	AddedModule InspectedModule |
	AddedProject Project

instance Show AddedContents where
	show = L.unpack . encode

data GhcModCommand =
	GhcModLang |
	GhcModFlags |
	GhcModType Position FilePath [String] |
	GhcModLint [FilePath] [String] |
	GhcModCheck [FilePath] [String] |
	GhcModCheckLint [FilePath] [String] [String]
		deriving (Show)

data AutoFixCommand =
	AutoFixShow [Note OutputMessage] |
	AutoFixFix [Note Correction] [Note Correction] Bool
		deriving (Show)

data FileContents = FileContents FilePath String deriving (Show)
-- TODO: Why deps is just string?
data TargetFilter =
	TargetProject String |
	TargetFile FilePath |
	TargetModule String |
	TargetDepsOf String |
	TargetCabal Cabal |
	TargetPackage String |
	TargetSourced |
	TargetStandalone |
	TargetAny
		deriving (Eq, Show)
data SearchQuery = SearchQuery String SearchType deriving (Show)
data SearchType = SearchExact | SearchPrefix | SearchInfix | SearchSuffix | SearchRegex deriving (Show)

instance Paths Command where
	paths f (Scan projs cs fs ps fcts ghcs docs infer) = Scan <$>
		each f projs <*>
		(each . paths) f cs <*>
		each f fs <*>
		each f ps <*>
		(each . paths) f fcts <*>
		pure ghcs <*>
		pure docs <*>
		pure infer
	paths f (RefineDocs projs fs ms) = RefineDocs <$> each f projs <*> each f fs <*> pure ms
	paths f (InferTypes projs fs ms) = InferTypes <$> each f projs <*> each f fs <*> pure ms
	paths f (Remove projs ps cs fs) = Remove <$> each f projs <*> pure ps <*> (each . paths) f cs <*> each f fs
	paths f (InfoModules t) = InfoModules <$> paths f t
	paths f (InfoSymbol q t) = InfoSymbol <$> pure q <*> paths f t
	paths f (InfoModule q t) = InfoModule <$> pure q <*> paths f t
	paths f (InfoResolve fpath es) = InfoResolve <$> f fpath <*> pure es
	paths f (InfoProject (Right proj)) = InfoProject <$> (Right <$> f proj)
	paths f (InfoSandbox fpath) = InfoSandbox <$> f fpath
	paths f (Lookup n fpath) = Lookup <$> pure n <*> f fpath
	paths f (Whois n fpath) = Whois <$> pure n <*> f fpath
	paths f (ResolveScopeModules fpath) = ResolveScopeModules <$> f fpath
	paths f (ResolveScope q g fpath) = ResolveScope q g <$> f fpath
	paths f (Complete n g fpath) = Complete n g <$> f fpath
	paths f (Lint fs fcts) = Lint <$> each f fs <*> (each . paths) f fcts
	paths f (Check fs fcts ghcs) = Check <$> each f fs <*> (each . paths) f fcts <*> pure ghcs
	paths f (CheckLint fs fcts ghcs) = CheckLint <$> each f fs <*> (each . paths) f fcts <*> pure ghcs
	paths f (Types fs fcts ghcs) = Types <$> each f fs <*> (each . paths) f fcts <*> pure ghcs
	paths f (GhcMod g) = GhcMod <$> paths f g
	paths _ c = pure c

instance Paths GhcModCommand where
	paths f (GhcModType pos fpath opts) = GhcModType <$> pure pos <*> f fpath <*> pure opts
	paths f (GhcModLint fs hlints) = GhcModLint <$> traverse f fs <*> pure hlints
	paths f (GhcModCheck fs ghcs) = GhcModCheck <$> traverse f fs <*> pure ghcs
	paths f (GhcModCheckLint fs ghcs hlints) = GhcModCheckLint <$> traverse f fs <*> pure ghcs <*> pure hlints
	paths _ g = pure g

instance Paths FileContents where
	paths f (FileContents fpath cts) = FileContents <$> f fpath <*> pure cts

instance Paths TargetFilter where
	paths f (TargetFile fpath) = TargetFile <$> f fpath
	paths f (TargetCabal c) = TargetCabal <$> paths f c
	paths _ t = pure t

instance FromCmd Command where
	cmdP = subparser $ mconcat [
		cmd "ping" "ping server" (pure Ping),
		cmd "listen" "listen server log" (pure Listen),
		cmd "add" "add info to database" (AddData <$> option readJSON idm),
		cmd "scan" "scan sources" $ Scan <$>
			many projectArg <*>
			many cabalArg <*>
			many fileArg <*>
			many (pathArg $ help "path") <*>
			many cmdP <*>
			ghcOpts <*>
			docsFlag <*>
			inferFlag,
		cmd "docs" "scan docs" $ RefineDocs <$> many projectArg <*> many fileArg <*> many moduleArg,
		cmd "infer" "infer types" $ InferTypes <$> many projectArg <*> many fileArg <*> many moduleArg,
		cmd "remove" "remove modules info" $ Remove <$>
			many projectArg <*>
			many packageArg <*>
			many cabalArg <*>
			many fileArg,
		cmd "modules" "list modules" (InfoModules <$> cmdP),
		cmd "packages" "list packages" (pure InfoPackages),
		cmd "projects" "list projects" (pure InfoProjects),
		cmd "sandboxes" "list sandboxes" (pure InfoSandboxes),
		cmd "symbol" "get symbol info" (InfoSymbol <$> cmdP <*> cmdP),
		cmd "module" "get module info" (InfoModule <$> cmdP <*> cmdP),
		cmd "resolve" "resolve module scope (or exports)" (InfoResolve <$> fileArg <*> exportsFlag),
		cmd "project" "get project info" (InfoProject <$> ((Left <$> projectArg) <|> (Right <$> pathArg idm))),
		cmd "sandbox" "get sandbox info" (InfoSandbox <$> (pathArg $ help "locate sandbox in parent of this path")),
		cmd "lookup" "lookup for symbol" (Lookup <$> strArgument idm <*> ctx),
		cmd "whois" "get info for symbol" (Whois <$> strArgument idm <*> ctx),
		cmd "scope" "get declarations accessible from module or within a project" (
			subparser (cmd "modules" "get modules accessible from module or within a project" (ResolveScopeModules <$> ctx)) <|>
			ResolveScope <$> cmdP <*> globalFlag <*> ctx),
		cmd "complete" "show completions for input" (Complete <$> strArgument idm <*> wideFlag <*> ctx),
		cmd "hayoo" "find declarations online via Hayoo" (Hayoo <$> strArgument idm <*> hayooPageArg <*> hayooPagesArg),
		cmd "cabal" "cabal commands" (subparser $ cmd "list" "list cabal packages" (CabalList <$> many (strArgument idm))),
		cmd "lint" "lint source files or file contents" (Lint <$> many fileArg <*> many cmdP),
		cmd "check" "check source files or file contents" (Check <$> many fileArg <*> many cmdP <*> ghcOpts),
		cmd "check-lint" "check and lint source files or file contents" (CheckLint <$> many fileArg <*> many cmdP <*> ghcOpts),
		cmd "types" "get types for file expressions" (Types <$> many fileArg <*> many cmdP <*> ghcOpts),
		cmd "ghc-mod" "ghc-mod commands" (GhcMod <$> cmdP),
		cmd "autofix" "autofix commands" (AutoFix <$> cmdP),
		cmd "ghc" "ghc commands" (subparser $ cmd "eval" "evaluate expression" (GhcEval <$> many (strArgument idm))),
		cmd "link" "link to server" (Link <$> holdFlag),
		cmd "exit" "exit" (pure Exit)]

instance FromCmd GhcModCommand where
	cmdP = subparser $ mconcat [
		cmd "lang" "get LANGUAGE pragmas" (pure GhcModLang),
		cmd "flags" "get OPTIONS_GHC pragmas" (pure GhcModFlags),
		cmd "type" "infer type with 'ghc-mod type'" (GhcModType <$> (Position <$> argument auto idm <*> argument auto idm) <*> fileArg <*> ghcOpts),
		cmd "lint" "lint source files" (GhcModLint <$> many (strArgument idm) <*> hlintOpts),
		cmd "check" "check source files" (GhcModCheck <$> many (strArgument idm) <*> ghcOpts),
		cmd "check-lint" "check & lint source files" (GhcModCheckLint <$> many (strArgument idm) <*> ghcOpts <*> hlintOpts)]

instance FromCmd AutoFixCommand where
	cmdP = subparser $ mconcat [
		cmd "show" "generate corrections for check & lint messages" (AutoFixShow <$> option readJSON (long "data" <> metavar "message" <> help "messages to make fixes for")),
		cmd "fix" "fix errors and return rest corrections with updated regions" (AutoFixFix <$>
			option readJSON (long "data" <> metavar "message" <> help "messages to fix") <*>
			option readJSON (long "rest" <> metavar "correction" <> short 'r' <> help "update corrections") <*>
			pureFlag)]

instance FromCmd FileContents where
	cmdP = option readJSON (long "contents")

instance FromCmd TargetFilter where
	cmdP = asum [TargetProject <$> projectArg, TargetFile <$> fileArg, TargetModule <$> moduleArg, TargetDepsOf <$> depsArg, TargetCabal <$> cabalArg, TargetPackage <$> packageArg, flag' TargetSourced (long "src"), flag' TargetStandalone (long "stand"), pure TargetAny]

instance FromCmd SearchQuery where
	cmdP = SearchQuery <$> (strArgument idm <|> pure "") <*> (asum [
		flag' SearchExact (long "exact"),
		flag' SearchRegex (long "regex"),
		flag' SearchInfix (long "infix"),
		flag' SearchSuffix (long "suffix"),
		pure SearchPrefix <* switch (long "prefix")])

readJSON :: FromJSON a => ReadM a
readJSON = str >>= maybe (readerError "Can't parse JSON argument") return . decode . L.pack

cabalArg = flag' Cabal (long "cabal") <|> (Sandbox <$> sandboxArg)
ctx = fileArg
depsArg = strOption (long "deps" <> metavar "object" <> help "filter to such that in dependency of specified object (file or project)")
docsFlag = switch (long "docs" <> help "scan source file docs")
exportsFlag = switch (long "exports" <> short 'e' <> help "resolve module exports")
fileArg = strOption (long "file" <> metavar "path" <> short 'f')
ghcOpts = many (strOption (long "ghc" <> metavar "option" <> short 'g' <> help "options to pass to GHC"))
globalFlag = switch (long "global" <> help "scope of project")
hayooPageArg = option auto (long "page" <> metavar "n" <> short 'p' <> help "page number (0 by default)" <> value 0)
hayooPagesArg = option auto (long "pages" <> metavar "count" <> short 'n' <> help "pages count (1 by default)" <> value 1)
hlintOpts = many (strOption (long "hlint" <> metavar "option" <> short 'h' <> help "options to pass to hlint"))
holdFlag = switch (long "hold" <> short 'h' <> help "don't return any response")
inferFlag = switch (long "infer" <> help "infer types")
-- localsArg = switch (long "locals" <> short 'l' <> help "look in local declarations")
moduleArg = strOption (long "module" <> metavar "name" <> short 'm' <> help "module name")
packageArg = strOption (long "package" <> metavar "name" <> help "module package")
pathArg f = strOption (long "path" <> metavar "path" <> short 'p' <> f)
projectArg = strOption (long "project" <> long "proj" <> metavar "project")
pureFlag = switch (long "pure" <> help "don't modify actual file, just return result")
sandboxArg = strOption (long "sandbox" <> metavar "path" <> help "path to cabal sandbox")
wideFlag = switch (long "wide" <> short 'w' <> help "wide mode - complete as if there were no import lists")

instance ToJSON Command where
	toJSON Ping = cmdJson "ping" []
	toJSON Listen = cmdJson "listen" []
	toJSON (AddData cts) = cmdJson "add" ["data" .= cts]
	toJSON (Scan projs cabals fs ps contents ghcs docs' infer') = cmdJson "scan" [
		"projects" .= projs,
		"sandboxes" .= cabals,
		"files" .= fs,
		"paths" .= ps,
		"contents" .= contents,
		"ghc-opts" .= ghcs,
		"docs" .= docs',
		"infer" .= infer']
	toJSON (RefineDocs projs fs ms) = cmdJson "docs" ["projects" .= projs, "files" .= fs, "modules" .= ms]
	toJSON (InferTypes projs fs ms) = cmdJson "infer" ["projects" .= projs, "files" .= fs, "modules" .= ms]
	toJSON (Remove projs packages cabals fs) = cmdJson "remove" ["projects" .= projs, "packages" .= packages, "sandboxes" .= cabals, "files" .= fs]
	toJSON (InfoModules tf) = cmdJson "modules" ["filter" .= tf]
	toJSON InfoPackages = cmdJson "packages" []
	toJSON InfoProjects = cmdJson "projects" []
	toJSON InfoSandboxes = cmdJson "sandboxes" []
	toJSON (InfoSymbol q tf) = cmdJson "symbol" ["query" .= q, "filter" .= tf]
	toJSON (InfoModule q tf) = cmdJson "module" ["query" .= q, "filter" .= tf]
	toJSON (InfoResolve f es) = cmdJson "resolve" ["file" .= f, "exports" .= es]
	toJSON (InfoProject p) = cmdJson "project" $ either (\pname -> ["name" .= pname]) (\ppath -> ["path" .= ppath]) p
	toJSON (InfoSandbox p) = cmdJson "sandbox" ["path" .= p]
	toJSON (Lookup n f) = cmdJson "lookup" ["name" .= n, "file" .= f]
	toJSON (Whois n f) = cmdJson "whois" ["name" .= n, "file" .= f]
	toJSON (ResolveScopeModules f) = cmdJson "scope modules" ["file" .= f]
	toJSON (ResolveScope q g f) = cmdJson "scope" ["query" .= q, "global" .= g, "file" .= f]
	toJSON (Complete q w f) = cmdJson "complete" ["prefix" .= q, "wide" .= w, "file" .= f]
	toJSON (Hayoo q p ps) = cmdJson "hayoo" ["query" .= q, "page" .= p, "pages" .= ps]
	toJSON (CabalList ps) = cmdJson "cabal list" ["packages" .= ps]
	toJSON (Lint fs cs) = cmdJson "lint" ["files" .= fs, "contents" .= cs]
	toJSON (Check fs cs ghcs) = cmdJson "check" ["files" .= fs, "contents" .= cs, "ghc-opts" .= ghcs]
	toJSON (CheckLint fs cs ghcs) = cmdJson "check-lint" ["files" .= fs, "contents" .= cs, "ghc-opts" .= ghcs]
	toJSON (Types fs cs ghcs) = cmdJson "types" ["files" .= fs, "contents" .= cs, "ghc-opts" .= ghcs]
	toJSON (GhcMod gcmd) = toJSON gcmd
	toJSON (AutoFix acmd) = toJSON acmd
	toJSON (GhcEval exprs) = cmdJson "ghc eval" ["exprs" .= exprs]
	toJSON (Link h) = cmdJson "link" ["hold" .= h]
	toJSON Exit = cmdJson "exit" []

instance FromJSON Command where
	parseJSON = withObject "command" $ \v -> asum [
		guardCmd "ping" v *> pure Ping,
		guardCmd "listen" v *> pure Listen,
		guardCmd "add" v *> (AddData <$> v .:: "data"),
		guardCmd "scan" v *> (Scan <$>
			v .:: "projects" <*>
			v .:: "sandboxes" <*>
			v .:: "files" <*>
			v .:: "paths" <*>
			v .:: "contents" <*>
			v .:: "ghc-opts" <*>
			v .:: "docs" <*>
			v .:: "infer"),
		guardCmd "docs" v *> (RefineDocs <$> v .:: "projects" <*> v .:: "files" <*> v .:: "modules"),
		guardCmd "infer" v *> (InferTypes <$> v .:: "projects" <*> v .:: "files" <*> v .:: "modules"),
		guardCmd "remove" v *> (Remove <$>
			v .:: "projects" <*>
			v .:: "packages" <*>
			v .:: "sandboxes" <*>
			v .:: "files"),
		guardCmd "modules" v *> (InfoModules <$> v .:: "filter"),
		guardCmd "packages" v *> pure InfoPackages,
		guardCmd "projects" v *> pure InfoProjects,
		guardCmd "sandboxes" v *> pure InfoSandboxes,
		guardCmd "symbol" v *> (InfoSymbol <$> v .:: "query" <*> v .:: "filter"),
		guardCmd "module" v *> (InfoModule <$> v .:: "query" <*> v .:: "filter"),
		guardCmd "resolve" v *> (InfoResolve <$> v .:: "file" <*> v .:: "exports"),
		guardCmd "project" v *> (InfoProject <$> asum [Left <$> v .:: "name", Right <$> v .:: "path"]),
		guardCmd "sandbox" v *> (InfoSandbox <$> v .:: "path"),
		guardCmd "lookup" v *> (Lookup <$> v .:: "name" <*> v .:: "file"),
		guardCmd "whois" v *> (Whois <$> v .:: "name" <*> v .:: "file"),
		guardCmd "scope modules" v *> (ResolveScopeModules <$> v .:: "file"),
		guardCmd "scope" v *> (ResolveScope <$> v .:: "query" <*> v .:: "global" <*> v .:: "file"),
		guardCmd "complete" v *> (Complete <$> v .:: "prefix" <*> v .:: "wide" <*> v .:: "file"),
		guardCmd "hayoo" v *> (Hayoo <$> v .:: "query" <*> v .:: "page" <*> v .:: "pages"),
		guardCmd "cabal list" v *> (CabalList <$> v .:: "packages"),
		guardCmd "lint" v *> (Lint <$> v .:: "files" <*> v .:: "contents"),
		guardCmd "check" v *> (Check <$> v .:: "files" <*> v .:: "contents" <*> v .:: "ghc-opts"),
		guardCmd "check-lint" v *> (CheckLint <$> v .:: "files" <*> v .:: "contents" <*> v .:: "ghc-opts"),
		guardCmd "types" v *> (Types <$> v .:: "files" <*> v .:: "contents" <*> v .:: "ghc-opts"),
		GhcMod <$> parseJSON (Object v),
		AutoFix <$> parseJSON (Object v),
		guardCmd "ghc eval" v *> (GhcEval <$> v .:: "exprs"),
		guardCmd "link" v *> (Link <$> v .:: "hold"),
		guardCmd "exit" v *> pure Exit]

instance ToJSON AddedContents where
	toJSON (AddedDatabase db) = object ["database" .= db]
	toJSON (AddedModule im) = object ["module" .= im]
	toJSON (AddedProject p) = object ["project" .= p]

instance FromJSON AddedContents where
	parseJSON = withObject "added-contents" $ \v -> asum [
		AddedDatabase <$> v .:: "database",
		AddedModule <$> v .:: "module",
		AddedProject <$> v .:: "project"]

instance ToJSON GhcModCommand where
	toJSON GhcModLang = cmdJson "ghc-mod lang" []
	toJSON GhcModFlags = cmdJson "ghc-mod flags" []
	toJSON (GhcModType pos f ghcs) = cmdJson "ghc-mod type" ["position" .= pos, "file" .= f, "ghc-opts" .= ghcs]
	toJSON (GhcModLint fs lints) = cmdJson "ghc-mod lint" ["files" .= fs, "hlint-opts" .= lints]
	toJSON (GhcModCheck fs ghcs) = cmdJson "ghc-mod check" ["files" .= fs, "ghc-opts" .= ghcs]
	toJSON (GhcModCheckLint fs ghcs lints) = cmdJson "ghc-mod check-lint" ["files" .= fs, "ghc-opts" .= ghcs, "hlint-opts" .= lints]

instance FromJSON GhcModCommand where
	parseJSON = withObject "ghc-mod-command" $ \v -> asum [
		guardCmd "ghc-mod lang" v *> pure GhcModLang,
		guardCmd "ghc-mod flags" v *> pure GhcModFlags,
		guardCmd "ghc-mod type" v *> (GhcModType <$> v .:: "position" <*> v .:: "file" <*> v .:: "ghc-opts"),
		guardCmd "ghc-mod lint" v *> (GhcModLint <$> v .:: "files" <*> v .:: "hlint-opts"),
		guardCmd "ghc-mod check" v *> (GhcModCheck <$> v .:: "files" <*> v .:: "ghc-opts"),
		guardCmd "ghc-mod check-lint" v *> (GhcModCheckLint <$> v .:: "files" <*> v .:: "ghc-opts" <*> v .:: "hlint-opts")]

instance ToJSON AutoFixCommand where
	toJSON (AutoFixShow ns) = cmdJson "autofix show" ["messages" .= ns]
	toJSON (AutoFixFix ns rests pure') = cmdJson "autofix fix" ["messages" .= ns, "rest" .= rests, "pure" .= pure']

instance FromJSON AutoFixCommand where
	parseJSON = withObject "auto-fix-command" $ \v -> asum [
		guardCmd "autofix show" v *> (AutoFixShow <$> v .:: "messages"),
		guardCmd "autofix fix" v *> (AutoFixFix <$> v .:: "messages" <*> v .:: "rest" <*> v .:: "pure")]

instance ToJSON FileContents where
	toJSON (FileContents fpath cts) = object ["file" .= fpath, "contents" .= cts]

instance FromJSON FileContents where
	parseJSON = withObject "file-contents" $ \v -> FileContents <$> v .:: "file" <*> v .:: "contents"

instance ToJSON TargetFilter where
	toJSON (TargetProject pname) = object ["project" .= pname]
	toJSON (TargetFile fpath) = object ["file" .= fpath]
	toJSON (TargetModule mname) = object ["module" .= mname]
	toJSON (TargetDepsOf dep) = object ["deps" .= dep]
	toJSON (TargetCabal cabal) = object ["cabal" .= cabal]
	toJSON (TargetPackage pname) = object ["package" .= pname]
	toJSON TargetSourced = toJSON ("sourced" :: String)
	toJSON TargetStandalone = toJSON ("standalone" :: String)
	toJSON TargetAny = toJSON ()

instance FromJSON TargetFilter where
	parseJSON j = obj j <|> str' <|> any' where
		obj = withObject "target-filter" $ \v -> asum [
			TargetProject <$> v .:: "project",
			TargetFile <$> v .:: "file",
			TargetModule <$> v .:: "module",
			TargetDepsOf <$> v .:: "deps",
			TargetCabal <$> v .:: "cabal",
			TargetPackage <$> v .:: "package"]
		str' = do
			s <- parseJSON j :: A.Parser String
			case s of
				"sourced" -> return TargetSourced
				"standalone" -> return TargetStandalone
				_ -> empty
		any' = (\() -> TargetAny) <$> parseJSON j

instance ToJSON SearchQuery where
	toJSON (SearchQuery q st) = object ["input" .= q, "type" .= st]

instance FromJSON SearchQuery where
	parseJSON = withObject "search-query" $ \v -> SearchQuery <$> v .:: "input" <*> v .:: "type"

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
