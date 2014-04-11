{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, DefaultSignatures, FlexibleInstances #-}

module System.Command (
	Command(..),
	DefaultConfig(..),
	cmd, cmd_,
	Help(..), addHelpCommand, addHelp,
	brief, help,
	run, runCmd,
	Opts(..),
	mapOpts, traverseOpts,
	option, option_, req, noreq, flag,
	opt, optMaybe, hasOpt, askOpt, askOptDef, askOpts,
	optsToArgs,
	splitArgs, unsplitArgs
	) where

import Control.Arrow
import Control.Applicative
import Control.Monad (join)
import Data.Char
import Data.List (stripPrefix, unfoldr, isPrefixOf)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe, maybeToList)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.Traversable (traverse)
import System.Console.GetOpt

-- | Command
data Command a = Command {
	commandName :: [String],
	commandPosArgs :: [String],
	commandDesc :: Maybe String,
	commandUsage :: [String],
	commandRun :: [String] -> Maybe (Either [String] a) }

instance Functor Command where
	fmap f cmd' = cmd' {
		commandRun = fmap (fmap f) . commandRun cmd' }

-- | Default value for options
class DefaultConfig a where
	defaultConfig :: a
	default defaultConfig :: Monoid a => a
	defaultConfig = mempty

instance DefaultConfig ()
instance DefaultConfig [String]

-- | Make command
-- >cmd name args desc args' onCmd
cmd :: (Monoid c, DefaultConfig c) => [String] -> [String] -> String -> [OptDescr c] -> (c -> [String] -> a) -> Command a
cmd name posArgs descr as act = Command {
	commandName = name,
	commandPosArgs = posArgs,
	commandDesc = descr',
	commandUsage = lines $ usageInfo (unwords (name ++ map (\a -> "[" ++ a ++ "]") posArgs) ++ maybe "" (" -- " ++) descr') as,
	commandRun = \opts -> case getOpt Permute as opts of
		(opts', cs, errs) -> fmap (\cs' -> if null errs then return (act (mconcat opts' `mappend` defaultConfig) cs') else Left errs) (stripPrefix name cs) }
	where
		descr' = if null descr then Nothing else Just descr

-- | Make command without params
cmd_ :: [String] -> [String] -> String -> ([String] -> a) -> Command a
cmd_ name posArgs descr act = cmd name posArgs descr [] (act' act) where
	act' :: a -> () -> a
	act' = const

data Help =
	HelpUsage [String] |
	HelpCommands [([String], [String])]
		deriving (Eq, Ord, Read, Show)

-- | Add help command
addHelpCommand :: String -> (Either String Help -> a) -> [Command a] -> [Command a]
addHelpCommand tool toCmd cmds = cmds' where
	cmds' = helpcmd' : cmds
	helpcmd = fmap toCmd $ cmd_ ["help"] ["command"] ("help command, also can be called in form '" ++ tool ++ " [command] -?'") onHelp
	-- allow help by last argument '-?'
	helpcmd' = helpcmd { commandRun = commandRun helpcmd . rewrite } where
		rewrite as
			| last as == "-?" = "help" : init as
			| otherwise = as
	onHelp [] = Right $ HelpUsage [tool ++ " " ++ brief c | c <- cmds']
	onHelp cmdname = case filter ((cmdname `isPrefixOf`) . commandName) cmds' of
		[] -> Left $ "Unknown command: " ++ unwords cmdname
		helps -> Right $ HelpCommands $ map (commandName &&& (addHeader . help)) helps
	addHeader [] = []
	addHeader (h:hs) = (tool ++ " " ++ h) : hs

-- | Add help commands, which outputs help to stdout
addHelp :: String -> (IO () -> a) -> [Command a] -> [Command a]
addHelp tool liftPrint cmds = addHelpCommand tool toCmd cmds where
	toCmd = liftPrint . either putStrLn printHelp
	printHelp :: Help -> IO ()
	printHelp (HelpUsage u) = mapM_ putStrLn $ map ('\t':) u
	printHelp (HelpCommands cs) = mapM_ putStrLn $ map ('\t':) $ concatMap snd cs

-- | Show brief help for command
brief :: Command a -> String
brief = head . commandUsage

-- | Show detailed help for command
help :: Command a -> [String]
help = commandUsage

-- | Run commands
run :: [Command a] -> a -> ([String] -> a) -> [String] -> a
run cmds onDef onError as = maybe onDef (either onError id) found where
	found = listToMaybe $ mapMaybe (`commandRun` as) cmds

-- | Try run command, wrapping any negative result to 'Maybe'
runCmd :: Command a -> [String] -> Maybe a
runCmd cmd = join . fmap toMaybe . commandRun cmd where
	toMaybe :: Either b c -> Maybe c
	toMaybe = either (const Nothing) Just

-- | Options holder
newtype Opts = Opts { getOpts :: Map String [String] }

instance Monoid Opts where
	mempty = Opts mempty
	(Opts l) `mappend` (Opts r) = Opts $ M.unionWith (++) l r

instance DefaultConfig Opts

-- | Map 'Opts'
mapOpts :: (String -> String) -> Opts -> Opts
mapOpts f = Opts . M.mapKeys f . getOpts

-- | Traverse 'Opts'
traverseOpts :: Applicative f => (String -> String -> f String) -> Opts -> f Opts
traverseOpts f = fmap Opts . M.traverseWithKey (traverse . f) . getOpts

option :: [Char] -> String -> [String] -> (String -> ArgDescr Opts) -> String -> OptDescr Opts
option fs name names onOption d = Option fs (name:names) (onOption name) d

option_ :: [Char] -> String -> (String -> ArgDescr Opts) -> String -> OptDescr Opts
option_ fs name = option fs name []

-- | Required option
req :: String -> String -> ArgDescr Opts
req nm n = ReqArg (opt n) nm

-- | Optional option
noreq :: String -> String -> ArgDescr Opts
noreq nm n = OptArg (optMaybe n) nm

-- | Flag option
flag :: String -> ArgDescr Opts
flag n = NoArg flag' where
	flag' :: Opts
	flag' = Opts $ M.singleton n []

opt :: String -> String -> Opts
opt n v = Opts $ M.singleton n [v]

optMaybe :: String -> Maybe String -> Opts
optMaybe n = Opts . M.singleton n . maybeToList

hasOpt :: String -> Opts -> Bool
hasOpt n = M.member n . getOpts

askOpt :: String -> Opts -> Maybe String
askOpt n = listToMaybe . askOpts n

askOptDef :: String -> Opts -> Maybe (Maybe String)
askOptDef n = fmap listToMaybe . M.lookup n . getOpts

askOpts :: String -> Opts -> [String]
askOpts n = fromMaybe [] . M.lookup n . getOpts

-- | Print 'Opts' as args
optsToArgs :: Opts -> [String]
optsToArgs = concatMap optToArgs . M.toList . getOpts where
	optToArgs :: (String, [String]) -> [String]
	optToArgs (n, []) = ["--" ++ n]
	optToArgs (n, vs) = ["--"++ n ++ "=" ++ v | v <- vs]

-- | Split string to words
splitArgs :: String -> [String]
splitArgs "" = []
splitArgs (c:cs)
	| isSpace c = splitArgs cs
	| c == '"' = let (w, cs') = readQuote cs in w : splitArgs cs'
	| otherwise = let (ws, tl) = break isSpace cs in (c:ws) : splitArgs tl
	where
		readQuote :: String -> (String, String)
		readQuote "" = ("", "")
		readQuote ('\\':ss)
			| null ss = ("\\", "")
			| otherwise = first (head ss :) $ readQuote (tail ss)
		readQuote ('"':ss) = ("", ss)
		readQuote (s:ss) = first (s:) $ readQuote ss

unsplitArgs :: [String] -> String
unsplitArgs = unwords . map escape where
	escape :: String -> String
	escape str
		| any isSpace str || '"' `elem` str = "\"" ++ concat (unfoldr escape' str) ++ "\""
		| otherwise = str
	escape' :: String -> Maybe (String, String)
	escape' [] = Nothing
	escape' (ch:tl) = Just (escaped, tl) where
		escaped = case ch of
			'"' -> "\\\""
			'\\' -> "\\\\"
			_ -> [ch]
