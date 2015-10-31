{-# LANGUAGE OverloadedStrings, CPP, TypeSynonymInstances, FlexibleInstances #-}

module HsDev.Server.Types (
	CommandOptions(..), CommandError(..), commandError_, commandError,
	CommandAction, CommandM, CommandActionT,

	Command(..), AddedContents(..), RefineCommand(..), InfoCommand(..),
	AutoScan(..), ContextCommand(..), GhcModCommand(..),
	AutoFixCommand(..), ScanTarget(..), SourceTarget(..),
	ActiveFile(..), TargetFilter(..), SearchQuery(..), SearchType(..),
	parseArgs, cmd,
	FromCmd(..)
	) where

import Control.Applicative
import Control.Arrow (second)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson hiding (Result, Error)
import Data.Aeson.Types (Pair)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Strict as HM (null)
import Data.Maybe (fromMaybe)
import Options.Applicative
import System.Log.Simple hiding (Command)

import HsDev.Database
import qualified HsDev.Database.Async as DB
import HsDev.Project
import HsDev.Symbols
import HsDev.Server.Message
import HsDev.Watcher.Types (Watcher)
import HsDev.Tools.GhcMod (OutputMessage, TypedRegion, WorkerMap)
import HsDev.Tools.Ghc.Worker (Worker, Ghc)
import HsDev.Tools.Types (Note, OutputMessage)
import HsDev.Tools.AutoFix (Correction)
import HsDev.Util ((.::), (.::?), jsonUnion)

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
	commandExit :: IO () }

data CommandError = CommandError String [Pair]

instance Monoid CommandError where
	mempty = CommandError "" []
	mappend (CommandError lmsg lp) (CommandError rmsg rp) = CommandError (lmsg ++ ", " ++ rmsg) (lp ++ rp)

commandError_ :: String -> ExceptT CommandError IO a
commandError_ m = commandError m []

commandError :: String -> [Pair] -> ExceptT CommandError IO a
commandError m ps = throwError $ CommandError m ps

type CommandAction = CommandOptions -> IO Result

type CommandM a = ExceptT CommandError IO a

type CommandActionT a = CommandOptions -> CommandM a



data Command =
	Ping |
	Listen |
	AddData { addedContents :: [AddedContents] } |
	Scan {
		scanTargets :: [ScanTarget],
		scanPaths :: [FilePath],
		scanContents :: [ActiveFile],
		scanGhcOpts :: [String],
		scanDocs :: Bool,
		scanInferTypes :: Bool } |
	Refine {
		refineCommand :: RefineCommand,
		refineTargets :: [SourceTarget] } |
	Remove {
		removeTarget :: ScanTarget } |
	GetInfo { getInfoCommand :: InfoCommand } |
	Context {
		contextCommand :: ContextCommand,
		contextFile :: FilePath,
		contextAutoScan :: AutoScan } |
	Hayoo {
		hayooQuery :: String,
		hayooPage :: Int,
		hayooPages :: Int } |
	CabalList { cabalListPackages :: [String] } |
	Lint {
		lintFiles :: [ActiveFile],
		lintAutoScan :: AutoScan } |
	Check {
		checkFiles :: [ActiveFile],
		checkGhcOpts :: [String],
		checkAutoScan :: AutoScan } |
	CheckLint {
		checkLintFiles :: [ActiveFile],
		checkLintGhcOpts :: [String],
		checkLintAutoScan :: AutoScan } |
	Types {
		typesFile :: ActiveFile,
		typesGhcOpts :: [String],
		typesAutoScan :: AutoScan } |
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

data RefineCommand = Docs | Infer deriving (Show)

data InfoCommand =
	InfoModules [TargetFilter] |
	InfoPackages |
	InfoProjects |
	InfoSandboxes |
	InfoSymbol SearchQuery [TargetFilter] |
	InfoModule [TargetFilter] |
	InfoResolve [TargetFilter] Bool |
	InfoProject (Either String FilePath) |
	InfoSandbox FilePath
		deriving (Show)

data AutoScan = AutoScan { autoScan :: Bool, autoScanGhcOpts :: [String] } deriving (Show)

data ContextCommand =
	Lookup String |
	Whois String |
	ResolveScopeModules |
	ResolveScope SearchQuery Bool |
	Complete String Bool
		deriving (Show)

data GhcModCommand =
	GhcModLang |
	GhcModFlags |
	GhcModType Position FilePath [String] |
	GhcModLint [FilePath] [String] Bool |
	GhcModCheck [FilePath] [String] Bool |
	GhcModCheckLint [FilePath] [String] [String] Bool
		deriving (Show)

data AutoFixCommand =
	AutoFixShow [Note OutputMessage] |
	AutoFixFix [Note Correction] [Note Correction] Bool
		deriving (Show)

data ScanTarget = ScanProject FilePath | ScanPackage String | ScanCabal Cabal | ScanFile FilePath deriving (Show)
data SourceTarget = SourceProject FilePath | SourceFile FilePath | SourceModule String deriving (Show)
data ActiveFile = ActiveFile FilePath (Maybe String) deriving (Show)
-- TODO: Why deps is just string?
data TargetFilter = TargetProject String | TargetFile FilePath | TargetModule String | TargetDepsOf String | TargetCabal Cabal | TargetPackage String | TargetSourced | TargetStandalone | TargetOldToo deriving (Show)
data SearchQuery = SearchQuery String SearchType deriving (Show)
data SearchType = SearchExact | SearchPrefix | SearchInfix | SearchSuffix | SearchRegex deriving (Show)

parseArgs :: Parser a -> [String] -> Either String a
parseArgs p c = maybe (Left $ unwords c) Right $ getParseResult . execParserPure (prefs mempty) (info p mempty) $ c

cmd :: String -> Parser a -> Parser a
cmd n p = subparser (command n (info p mempty))

class FromCmd a where
	cmdP :: Parser a

instance FromCmd Command where
	cmdP = foldr (<|>) empty [
		cmd "ping" (pure Ping),
		cmd "listen" (pure Listen),
		cmd "add" (AddData <$> option readJSON idm),
		cmd "scan" (Scan <$> many cmdP <*> many (pathArg $ help "path") <*> many cmdP <*> ghcOpts <*> docsFlag <*> inferFlag),
		Refine <$> cmdP <*> many cmdP,
		cmd "remove" (Remove <$> cmdP),
		GetInfo <$> cmdP,
		Context <$> cmdP <*> ctx <*> cmdP,
		cmd "hayoo" (Hayoo <$> strArgument idm <*> hayooPageArg <*> hayooPagesArg),
		cmd "cabal" (cmd "list" (CabalList <$> many (strArgument idm))),
		cmd "lint" (Lint <$> many cmdP <*> cmdP),
		cmd "check" (Check <$> many cmdP <*> ghcOpts <*> cmdP),
		cmd "check-lint" (CheckLint <$> many cmdP <*> ghcOpts <*> cmdP),
		cmd "types" (Types <$> cmdP <*> ghcOpts <*> cmdP),
		cmd "ghc-mod" (GhcMod <$> cmdP),
		cmd "autofix" (AutoFix <$> cmdP),
		cmd "ghc" (cmd "eval" (GhcEval <$> many (strArgument idm))),
		cmd "link" (Link <$> holdFlag),
		cmd "exit" (pure Exit)]

instance FromCmd RefineCommand where
	cmdP = cmd "docs" (pure Docs) <|> cmd "infer" (pure Infer)

instance FromCmd InfoCommand where
	cmdP = foldr (<|>) empty [
		cmd "modules" (InfoModules <$> many cmdP),
		cmd "packages" (pure InfoPackages),
		cmd "projects" (pure InfoProjects),
		cmd "sandboxes" (pure InfoSandboxes),
		cmd "symbol" (InfoSymbol <$> cmdP <*> many cmdP),
		cmd "module" (InfoModule <$> many cmdP),
		cmd "resolve" (InfoResolve <$> many cmdP <*> exportsFlag),
		cmd "project" (InfoProject <$> ((Left <$> projectArg) <|> (Right <$> pathArg idm))),
		cmd "sandbox" (InfoSandbox <$> (pathArg $ help "locate sandbox in parent of this path"))]

instance FromCmd AutoScan where
	cmdP = AutoScan <$> autoScanFlag <*> ghcOpts

instance FromCmd ContextCommand where
	cmdP = foldr (<|>) empty [
		cmd "lookup" (Lookup <$> strArgument idm),
		cmd "whois" (Whois <$> strArgument idm),
		cmd "scope" (
			cmd "modules" (pure ResolveScopeModules) <|>
			ResolveScope <$> cmdP <*> globalFlag),
		cmd "complete" (Complete <$> strArgument idm <*> wideFlag)]

instance FromCmd GhcModCommand where
	cmdP = foldr (<|>) empty [
		cmd "lang" (pure GhcModLang),
		cmd "flags" (pure GhcModFlags),
		cmd "type" (GhcModType <$> (Position <$> argument auto idm <*> argument auto idm) <*> fileArg <*> ghcOpts),
		cmd "lint" (GhcModLint <$> many (strArgument idm) <*> hlintOpts <*> autoScanFlag),
		cmd "check" (GhcModCheck <$> many (strArgument idm) <*> ghcOpts <*> autoScanFlag),
		cmd "check-lint" (GhcModCheckLint <$> many (strArgument idm) <*> ghcOpts <*> hlintOpts <*> autoScanFlag)]

instance FromCmd AutoFixCommand where
	cmdP =
		cmd "show" (AutoFixShow <$> option readJSON (long "data" <> metavar "message" <> help "messages to make fixes for")) <|>
		cmd "fix" (AutoFixFix <$>
			option readJSON (long "data" <> metavar "message" <> help "messages to fix") <*>
			option readJSON (long "rest" <> metavar "correction" <> short 'r' <> help "update corrections") <*>
			pureFlag)

instance FromCmd ScanTarget where
	cmdP = foldr (<|>) empty [ScanProject <$> projectArg, ScanPackage <$> packageArg, ScanCabal <$> (flag' Cabal idm <|> (Sandbox <$> sandboxArg)), ScanFile <$> fileArg]

instance FromCmd SourceTarget where
	cmdP = foldr (<|>) empty [SourceProject <$> projectArg, SourceFile <$> fileArg, SourceModule <$> moduleArg]

instance FromCmd ActiveFile where
	cmdP = (ActiveFile <$> strArgument idm <*> pure Nothing) <|> (option readJSON (long "contents"))

instance FromCmd TargetFilter where
	cmdP = foldr (<|>) empty [TargetProject <$> projectArg, TargetFile <$> fileArg, TargetModule <$> moduleArg, TargetDepsOf <$> depsArg, TargetCabal <$> (flag' Cabal idm <|> (Sandbox <$> sandboxArg)), TargetPackage <$> packageArg, flag' TargetSourced (long "src"), flag' TargetStandalone (long "stand"), flag' TargetOldToo (long "no-last")]

instance FromCmd SearchQuery where
	cmdP = SearchQuery <$> strArgument idm <*> (foldr (<|>) empty [
		flag' SearchExact (long "exact"),
		flag' SearchRegex (long "regex"),
		flag' SearchInfix (long "infix"),
		flag' SearchSuffix (long "suffix"),
		pure SearchPrefix <* switch (long "prefix")])

readJSON :: FromJSON a => ReadM a
readJSON = str >>= maybe (readerError "Can't parse JSON argument") return . decode . L.pack

allFlag d = switch (long "all" <> short 'a' <> help d)
autoScanFlag = switch (long "autoscan" <> short 's' <> help "automatically scan related files/projects")
cacheDir = strOption (long "cache-dir" <> metavar "path" <> help "cache path")
cacheFile = strOption (long "cache-file" <> metavar "path" <> help "cache file")
ctx = fileArg
dataArg = strOption (long "data" <> metavar "contents" <> help "data to pass to command")
depsArg = strOption (long "deps" <> metavar "object" <> help "filter to such that in dependency of specified object (file or project)")
docsFlag = switch (long "docs" <> help "scan source file docs")
exportsFlag = switch (long "exports" <> short 'e' <> help "resolve module exports")
fileArg = strOption (long "file" <> metavar "path" <> short 'f')
findArg = strOption (long "find" <> metavar "query" <> help "infix match")
ghcOpts = many (strOption (long "ghc" <> metavar "option" <> short 'g' <> help "options to pass to GHC"))
globalFlag = switch (long "global" <> help "scope of project")
hayooPageArg = option auto (long "page" <> metavar "n" <> short 'p' <> help "page number (0 by default)" <> value 0)
hayooPagesArg = option auto (long "pages" <> metavar "count" <> short 'n' <> help "pages count (1 by default)" <> value 1)
hlintOpts = many (strOption (long "hlint" <> metavar "option" <> short 'h' <> help "options to pass to hlint"))
holdFlag = switch (long "hold" <> short 'h' <> help "don't return any response")
inferFlag = switch (long "infer" <> help "infer types")
localsArg = switch (long "locals" <> short 'l' <> help "look in local declarations")
moduleArg = strOption (long "module" <> metavar "name" <> short 'm' <> help "module name")
noLastFlag = switch (long "no-last" <> help "select not only last version packages")
packageArg = strOption (long "package" <> metavar "name" <> help "module package")
pathArg f = strOption (long "path" <> metavar "path" <> short 'p' <> f)
projectArg = strOption (long "project" <> long "proj" <> metavar "project")
packageVersionArg = strOption (long "version" <> metavar "id" <> short 'v' <> help "package version")
pureFlag = switch (long "pure" <> help "don't modify actual file, just return result")
sandboxArg = strOption (long "sandbox" <> metavar "path" <> help "path to cabal sandbox")
sandboxList = many sandboxArg
sandboxes = (++) <$> flag [] [Cabal] (long "cabal" <> help "cabal") <*> (map Sandbox <$> sandboxList)
sourced = switch (long "src" <> help "source files")
standaloned = switch (long "stand" <> help "standalone files")
wideFlag = switch (long "wide" <> short 'w' <> help "wide mode - complete as if there were no import lists")

cmdJson :: String -> [Pair] -> Value
cmdJson nm ps = object $ ("command" .= nm) : ps

instance ToJSON Command where
	toJSON Ping = cmdJson "ping" []
	toJSON Listen = cmdJson "listen" []
	toJSON (AddData cts) = cmdJson "add" ["data" .= cts]
	toJSON (Scan ts fs as ghcs docs' infer') = cmdJson "scan" ["targets" .= ts, "paths" .= fs, "contents" .= as, "ghc-opts" .= ghcs, "docs" .= docs', "infer" .= infer']
	toJSON (Refine rcmd ts) = object ["targets" .= ts] `jsonUnion` rcmd
	toJSON (Remove t) = cmdJson "remove" [] `jsonUnion` t
	toJSON (GetInfo gcmd) = toJSON gcmd
	toJSON (Context ccmd f as) = toJSON ccmd `jsonUnion` object ["file" .= f, "autoscan" .= as]
	toJSON (Hayoo q p ps) = cmdJson "hayoo" ["query" .= q, "page" .= p, "pages" .= ps]
	toJSON (CabalList ps) = cmdJson "cabal list" ["packages" .= ps]
	toJSON (Lint fs as) = cmdJson "lint" ["files" .= fs, "autoscan" .= as]
	toJSON (Check fs ghcs as) = cmdJson "check" ["files" .= fs, "ghc-opts" .= ghcs, "autoscan" .= as]
	toJSON (CheckLint fs ghcs as) = cmdJson "check-lint" ["files" .= fs, "ghc-opts" .= ghcs, "autoscan" .= as]
	toJSON (Types f ghcs as) = cmdJson "types" ["file" .= f, "ghc-opts" .= ghcs, "autoscan" .= as]
	toJSON (GhcMod gcmd) = toJSON gcmd
	toJSON (AutoFix acmd) = toJSON acmd
	toJSON (GhcEval exprs) = cmdJson "ghc eval" ["exprs" .= exprs]
	toJSON (Link h) = cmdJson "link" ["hold" .= h]
	toJSON Exit = cmdJson "exit" []

-- instance ToJSON AddedContents where
-- 	toJSON (AddedDatabase db) = object ["database" .= db]
-- 	toJSON (AddedModule im) = object ["module" .= im]
-- 	toJSON (AddedProject p) = object ["project" .= p]

instance ToJSON RefineCommand where
	toJSON Docs = cmdJson "docs" []
	toJSON Infer = cmdJson "infer" []

instance ToJSON InfoCommand where
	toJSON (InfoModules tf) = cmdJson "modules" ["filters" .= tf]
	toJSON InfoPackages = cmdJson "packages" []
	toJSON InfoProjects = cmdJson "projects" []
	toJSON InfoSandboxes = cmdJson "sandboxes" []
	toJSON (InfoSymbol q tf) = cmdJson "symbol" ["query" .= q, "filters" .= tf]
	toJSON (InfoModule tf) = cmdJson "module" ["filters" .= tf]
	toJSON (InfoResolve tf es) = cmdJson "resolve" ["filters" .= tf, "exports" .= es]
	toJSON (InfoProject p) = cmdJson "project" $ either (\pname -> ["name" .= pname]) (\ppath -> ["path" .= ppath]) p
	toJSON (InfoSandbox p) = cmdJson "sandbox" ["path" .= p]

instance ToJSON AutoScan where
	toJSON (AutoScan f ghcs) = object ["do" .= f, "ghc-opts" .= ghcs]

instance ToJSON ContextCommand where
	toJSON (Lookup n) = cmdJson "lookup" ["name" .= n]
	toJSON (Whois n) = cmdJson "whois" ["name" .= n]
	toJSON ResolveScopeModules = cmdJson "scope modules" []
	toJSON (ResolveScope q g) = cmdJson "scope" ["query" .= q, "global" .= g]
	toJSON (Complete q w) = cmdJson "complete" ["prefix" .= q, "wide" .= w]

instance ToJSON GhcModCommand where
	toJSON GhcModLang = cmdJson "ghc-mod lang" []
	toJSON GhcModFlags = cmdJson "ghc-mod flags" []
	toJSON (GhcModType pos f ghcs) = cmdJson "ghc-mod type" ["position" .= pos, "file" .= f, "ghc-opts" .= ghcs]
	toJSON (GhcModLint fs lints _) = cmdJson "ghc-mod lint" ["files" .= fs, "hlint-opts" .= lints]
	toJSON (GhcModCheck fs ghcs _) = cmdJson "ghc-mod lint" ["files" .= fs, "ghc-opts" .= ghcs]
	toJSON (GhcModCheckLint fs ghcs lints _) = cmdJson "ghc-mod lint" ["files" .= fs, "ghc-opts" .= ghcs, "hlint-opts" .= lints]

instance ToJSON AutoFixCommand where
	toJSON (AutoFixShow ns) = cmdJson "autofix show" ["messages" .= ns]
	toJSON (AutoFixFix ns rests pure) = cmdJson "autofix fix" ["messages" .= ns, "rest" .= rests, "pure" .= pure]

instance ToJSON ScanTarget where
	toJSON (ScanProject fpath) = object ["project" .= fpath]
	toJSON (ScanPackage pname) = object ["package" .= pname]
	toJSON (ScanCabal cabal) = toJSON ["cabal" .= cabal]
	toJSON (ScanFile fpath) = object ["file" .= fpath]

instance ToJSON SourceTarget where
	toJSON (SourceProject fpath) = object ["project" .= fpath]
	toJSON (SourceFile fpath) = object ["file" .= fpath]
	toJSON (SourceModule mname) = object ["module" .= mname]

instance ToJSON ActiveFile where
	toJSON (ActiveFile fpath mcts) = object ["file" .= fpath, "contents" .= mcts]

instance ToJSON TargetFilter where
	toJSON (TargetProject pname) = object ["project" .= pname]
	toJSON (TargetFile fpath) = object ["file" .= fpath]
	toJSON (TargetModule mname) = object ["module" .= mname]
	toJSON (TargetDepsOf dep) = object ["deps" .= dep]
	toJSON (TargetCabal cabal) = object ["cabal" .= cabal]
	toJSON (TargetPackage pname) = object ["package" .= pname]
	toJSON TargetSourced = toJSON ("sourced" :: String)
	toJSON TargetStandalone = toJSON ("standalone" :: String)
	toJSON TargetOldToo = toJSON ("old-too" :: String)

instance ToJSON SearchQuery where
	toJSON (SearchQuery q st) = object ["input" .= q, "type" .= st]

instance ToJSON SearchType where
	toJSON SearchExact = toJSON ("exact" :: String)
	toJSON SearchPrefix = toJSON ("prefix" :: String)
	toJSON SearchInfix = toJSON ("infix" :: String)
	toJSON SearchSuffix = toJSON ("suffix" :: String)
	toJSON SearchRegex = toJSON ("regex" :: String)

instance ToJSON AddedContents where
	toJSON (AddedDatabase db) = toJSON db
	toJSON (AddedModule m) = toJSON m
	toJSON (AddedProject p) = toJSON p

instance FromJSON AddedContents where
	parseJSON v = foldr (<|>) empty [
		AddedDatabase <$> parseJSON v,
		AddedModule <$> parseJSON v,
		AddedProject <$> parseJSON v]

instance FromJSON ActiveFile where
	parseJSON = withObject "active-file" $ \v -> ActiveFile <$> v .:: "file" <*> v .::? "contents"
