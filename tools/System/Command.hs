{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, DefaultSignatures, FlexibleInstances, TupleSections #-}

module System.Command (
	Command(..),
	DefaultConfig(..),
	cmd, cmd_,
	Help(..), addHelpCommand, addHelp,
	brief, help,
	run, runCmd,
	OptionValue(..),
	Opts(..),
	(%--), hoist,
	option, option_,
	has,
	req, noreq, no,
	arg, opt, def, list, flag,
	toArgs,
	splitArgs, unsplitArgs
	) where

import Control.Arrow
import Control.Applicative
import Control.Monad (join, (>=>))
import Data.Aeson
import Data.Char
import qualified Data.HashMap.Strict as HM (HashMap, toList)
import Data.String
import Data.List (stripPrefix, unfoldr, isPrefixOf)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe, maybeToList, isJust)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable(traverse))
import qualified Data.Vector as V
import System.Console.GetOpt

import Data.Group

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

-- | Convertible to option value
class OptionValue a where
	toOption :: a -> String
	default toOption :: Show a => a -> String
	toOption = show

instance OptionValue String where
	toOption = id

instance OptionValue Int
instance OptionValue Integer
instance OptionValue Float
instance OptionValue Double
instance OptionValue Bool

-- | Options holder
newtype Opts a = Opts { getOpts :: Map String [a] }

instance Eq a => Eq (Opts a) where
	Opts l == Opts r = l == r

instance Functor Opts where
	fmap f (Opts opts) = Opts $ fmap (fmap f) opts

instance Foldable Opts where
	foldMap f (Opts opts) = foldMap (foldMap f) opts

instance Traversable Opts where
	traverse f (Opts opts) = Opts <$> traverse (traverse f) opts

instance Monoid (Opts a) where
	mempty = Opts mempty
	(Opts l) `mappend` (Opts r) = Opts $ M.unionWith mappend l r

instance Eq a => Group (Opts a) where
	add = mappend
	(Opts l) `sub` (Opts r) = Opts $ l `sub` r
	zero = mempty

instance DefaultConfig (Opts a)

instance ToJSON a => ToJSON (Opts a) where
	toJSON (Opts opts) = object $ map toPair $ M.toList opts where
		toPair (n, []) = fromString n .= Null
		toPair (n, [v]) = fromString n .= v
		toPair (n, vs) = fromString n .= vs

instance FromJSON a => FromJSON (Opts a) where
	parseJSON = withObject "options" $ fmap (Opts . M.fromList) . mapM fromPair . HM.toList where
		fromPair (n, v) = (T.unpack n,) <$> case v of
			Null -> return []
			_ -> (return <$> parseJSON v) <|> parseJSON v

-- | Make 'Opts' with one argument
(%--) :: OptionValue a => String -> a -> Opts String
n %-- v = Opts $ M.singleton n [toOption v]

-- | Make 'Opts' with flag enabled
hoist :: String -> Opts a
hoist n = Opts $ M.singleton n []

option :: [Char] -> String -> [String] -> (String -> ArgDescr (Opts a)) -> String -> OptDescr (Opts a)
option fs name names onOpt d = Option fs (name:names) (onOpt name) d

option_ :: [Char] -> String -> (String -> ArgDescr (Opts a)) -> String -> OptDescr (Opts a)
option_ fs name = option fs name []

has :: String -> Opts a -> Bool
has n = M.member n . getOpts

-- | Required option
req :: String -> String -> ArgDescr (Opts String)
req nm n = ReqArg (n %--) nm

-- | Not required option
noreq :: String -> String -> ArgDescr (Opts String)
noreq nm n = OptArg (maybe (hoist n) (n %--)) nm

-- | No option
no :: String -> ArgDescr (Opts String)
no = NoArg . hoist

-- | Get argument
arg :: String -> Opts a -> Maybe a
arg n = M.lookup n . getOpts >=> listToMaybe

-- | Get optional argument
opt :: String -> Opts a -> Maybe (Maybe a)
opt n = fmap listToMaybe . M.lookup n . getOpts

-- | Get argument with default
def :: a -> String -> Opts a -> Maybe a
def d n = fmap (fromMaybe d . listToMaybe) . M.lookup n . getOpts

-- | Get list arguments
list :: String -> Opts a -> [a]
list n = fromMaybe [] . M.lookup n . getOpts

-- | Get flag
flag :: String -> Opts a -> Bool
flag n = isJust . M.lookup n . getOpts

-- | Print 'Opts' as args
toArgs :: Opts String -> [String]
toArgs = concatMap toArgs' . M.toList . getOpts where
	toArgs' :: (String, [String]) -> [String]
	toArgs' (n, []) = ["--" ++ n]
	toArgs' (n, vs) = [concat ["--", n, "=", v] | v <- vs]

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
