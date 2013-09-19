{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, DefaultSignatures, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.Command (
	Command(..),
	DefaultConfig(..),
	cmd, cmd_,
	Help(..), addHelpCommand, addHelp,
	brief, help,
	run,
	Opts(..),
	opt, hasOpt, askOpt, askOpts,
	split, unsplit
	) where

import Control.Arrow
import Data.Char
import Data.List (stripPrefix, unfoldr, isPrefixOf)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.Function (fix)
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

instance Functor ArgDescr where
	fmap f (NoArg v) = NoArg $ f v
	fmap f (ReqArg g d) = ReqArg (f . g) d
	fmap f (OptArg g d) = OptArg (f . g) d

instance Functor OptDescr where
	fmap f (Option short long descr expl) = Option short long (fmap f descr) expl

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

-- | Options holder
newtype Opts = Opts { getOpts :: Map String [String] }

instance Monoid Opts where
	mempty = Opts mempty
	(Opts l) `mappend` (Opts r) = Opts $ M.unionWith (++) l r

instance DefaultConfig Opts

opt :: String -> String -> Opts
opt n v = Opts $ M.singleton n [v]

hasOpt :: String -> Opts -> Bool
hasOpt n = M.member n . getOpts

askOpt :: String -> Opts -> Maybe String
askOpt n = listToMaybe . askOpts n

askOpts :: String -> Opts -> [String]
askOpts n = fromMaybe [] . M.lookup n . getOpts

-- | Split string to words
split :: String -> [String]
split "" = []
split (c:cs)
	| isSpace c = split cs
	| c == '"' = let (w, cs') = readQuote cs in w : split cs'
	| otherwise = let (ws, tl) = break isSpace cs in (c:ws) : split tl
	where
		readQuote :: String -> (String, String)
		readQuote "" = ("", "")
		readQuote ('\\':ss)
			| null ss = ("\\", "")
			| otherwise = first (head ss :) $ readQuote (tail ss)
		readQuote ('"':ss) = ("", ss)
		readQuote (s:ss) = first (s:) $ readQuote ss

unsplit :: [String] -> String
unsplit = unwords . map escape where
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