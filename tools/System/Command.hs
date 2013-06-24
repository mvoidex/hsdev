{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.Command (
	Command(..),
	cmd, cmd_, addhelp,
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

-- | Make command
cmd :: Monoid c => [String] -> [String] -> String -> [OptDescr c] -> (c -> [String] -> a) -> Command a
cmd name posArgs descr as act = Command {
	commandName = name,
	commandPosArgs = posArgs,
	commandDesc = descr',
	commandUsage = lines $ usageInfo (unwords (name ++ map (\a -> "[" ++ a ++ "]") posArgs) ++ maybe "" (" -- " ++) descr') as,
	commandRun = \opts -> case getOpt Permute as opts of
		(opts', cs, errs) -> fmap (\cs' -> if null errs then return (act (mconcat opts') cs') else Left errs) (stripPrefix name cs) }
	where
		descr' = if null descr then Nothing else Just descr

-- | Make command without params
cmd_ :: [String] -> [String] -> String -> ([String] -> a) -> Command a
cmd_ name posArgs descr act = cmd name posArgs descr [] (act' act) where
	act' :: a -> () -> a
	act' = const

-- | Add help command
addhelp :: String -> (IO () -> a) -> IO () -> [Command a] -> [Command a]
addhelp tool toCmd usage cmds = fix addhelp' where
	addhelp' hcmds = helpcmd : cmds where
		helpcmd = fmap toCmd $ cmd_ ["help"] ["command"] "help" $ \as -> case as of
			[] -> usage
			cmdname -> do
				let
					addHeader [] = []
					addHeader (h:hs) = map ('\t':) $ (tool ++ " " ++ h) : map ('\t':) hs
				case filter ((cmdname `isPrefixOf`) . commandName) hcmds of
					[] -> putStrLn $ "Unknown command: " ++ unwords cmdname
					helps -> mapM_ (putStrLn . unlines . addHeader . help) helps

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
