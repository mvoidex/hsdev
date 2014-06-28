{-# LANGUAGE FlexibleInstances #-}

module System.Console.Args (
	Args(..), Opts(..), Arg(..), Opt(..),
	(%--), hoist, has, arg, listArg, flagSet,
	flag, req, list,
	desc, alias, short,
	parse, parse_, tryParse, info,

	-- * Helpers
	split, unsplit, verify,

	module Data.Help
	) where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable(traverse))

import Data.Help
import Text.Format

data Args = Args {
	posArgs :: [String],
	namedArgs :: (Opts String) }
		deriving (Eq)

instance Monoid Args where
	mempty = Args [] mempty
	(Args largs lopts) `mappend` (Args rargs ropts) = Args (largs ++ rargs) (lopts `mappend` ropts)

newtype Opts a = Opts { getOpts :: Map String [a] }

instance Eq a => Eq (Opts a) where
	Opts l == Opts r = l == r

instance Functor Opts where
	fmap f = Opts . fmap (fmap f) . getOpts

instance Foldable Opts where
	foldMap f = foldMap (foldMap f) . getOpts

instance Traversable Opts where
	traverse f = fmap Opts . traverse (traverse f) . getOpts

instance Monoid (Opts a) where
	mempty = Opts mempty
	(Opts l) `mappend` (Opts r) = Opts $ M.unionWith mappend l r

data Arg = Flag | Required String | List String deriving (Eq, Ord, Show)

argName :: Arg -> Maybe String
argName Flag = Nothing
argName (Required n) = Just n
argName (List n) = Just $ n ++ "..."

data Opt = Opt {
	optName :: String,
	optShort :: [Char],
	optLong :: [String],
	optDescription :: Maybe String,
	optArg :: Arg }
		deriving (Eq, Show)

--	(%--), hoist, has, arg, listArg, flagSet,
(%--) :: Format a => String -> a -> Opts String
n %-- v = Opts $ M.singleton n [format v]

-- | Make 'Opts' with flag set
hoist :: String -> Opts a
hoist n = Opts $ M.singleton n []

has :: String -> Opts a -> Bool
has n = M.member n . getOpts

-- | Get argument value
arg :: String -> Opts a -> Maybe a
arg n = M.lookup n . getOpts >=> listToMaybe

-- | Get list argument
listArg :: String -> Opts a -> [a]
listArg n = fromMaybe [] . M.lookup n . getOpts

-- | Is flag set
flagSet :: String -> Opts a -> Bool
flagSet n = isJust . M.lookup n . getOpts

-- | Flag option
flag :: String -> Opt
flag n = Opt n [] [] Nothing Flag

-- | Required option
req :: String -> String -> Opt
req n v = Opt n [] [] Nothing (Required v)

-- | List option
list :: String -> String -> Opt
list n v = Opt n [] [] Nothing (List v)

-- | Set description
--
-- >flag "quiet" `desc` "quiet mode"
desc :: Opt -> String -> Opt
desc o d = o { optDescription = Just d }

-- | Set aliases
--
-- >fliag "quiet" `alias` 
alias :: Opt -> [String] -> Opt
alias o ls = o { optLong = optLong o ++ ls }

short :: Opt -> [Char] -> Opt
short o ss = o { optShort = optShort o ++ ss }

findOpt :: String -> [Opt] -> Maybe Opt
findOpt n = find opt' where
	opt' :: Opt -> Bool
	opt' (Opt n' s l _ _) = n `elem` (n' : (map return s ++ l))

parse :: [Opt] -> [String] -> Either String Args
parse os = unfoldrM parseCmd >=> verify os . mconcat where
	parseCmd :: [String] -> Either String (Maybe (Args, [String]))
	parseCmd [] = Right Nothing
	parseCmd (cmd:cmds)
		| isFlag cmd = do
			opt' <- lookOpt cmd os
			case optArg opt' of
				Flag -> Right $ Just (Args [] $ Opts $ M.singleton (optName opt') [], cmds)
				Required _ -> case cmds of
					(value:cmds')
						| not (isFlag value) -> Right $ Just (Args [] $ Opts $ M.singleton (optName opt') [value], cmds')
						| otherwise -> Left $ "No value specified for option '$'" ~~ optName opt'
					[] -> Left $ "No value specified for option '" ++ optName opt' ++ "'"
				List _ -> case cmds of
					(value:cmds')
						| not (isFlag value) -> Right $ Just (Args [] $ Opts $ M.singleton (optName opt') [value], cmds')
						| otherwise -> Left $ "No value specified for option '$'" ~~ optName opt'
					[] -> Left $ "No value specified for option '$'" ~~ optName opt'
		| otherwise = Right $ Just (Args [cmd] mempty, cmds)

	lookOpt :: String -> [Opt] -> Either String Opt
	lookOpt n = maybe (Left $ "Invalid option '$'" ~~ n) Right . findOpt (dropWhile (== '-') n)

-- | Parse with no options declarations
parse_ :: [String] -> Args
parse_ = mconcat . unfoldr parseCmd where
	parseCmd :: [String] -> Maybe (Args, [String])
	parseCmd [] = Nothing
	parseCmd (cmd:cmds)
		| isFlag cmd = case cmds of
			(value:cmds')
				| not (isFlag value) -> Just (Args [] $ Opts $ M.singleton cmd [value], cmds')
				| otherwise -> Just (Args [] $ Opts $ M.singleton cmd [], cmds)
			[] -> Just (Args [] $ Opts $ M.singleton cmd [], [])
		| otherwise = Just (Args [cmd] mempty, cmds)

tryParse :: [Opt] -> [String] -> Args
tryParse os s = either (const $ parse_ s) id $ parse os s

instance Help Opt where
	brief (Opt n _ _ _ arg) = concat [
		longOpt n,
		maybe "" (" " ++) $ argName arg]
	help (Opt n ss ls desc arg) = [concat [
		unwords (map shortOpt ss ++ map longOpt (n : ls)),
		maybe "" (" " ++) $ argName arg,
		maybe "" (" -- " ++) desc]]

instance Help [Opt] where
	brief = unwords . map brief
	help = concatMap help

info :: [Opt] -> String
info = unlines . indented

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

verify :: [Opt] -> Args -> Either String Args
verify os = withOpts $ fmap (Opts . M.fromList) . mapM (uncurry verify') . M.toList . getOpts where
	withOpts :: Functor f => (Opts String -> f (Opts String)) -> Args -> f Args
	withOpts f (Args a o) = Args a <$> f o
	verify' :: String -> [String] -> Either String (String, [String])
	verify' n v = case findOpt n os of
		Nothing -> Left $ "Invalid option '$'" ~~ n
		Just opt -> maybe (Right (n, v)) Left $ case (optArg opt, v) of
			(Flag, []) -> Nothing
			(Flag, _) -> Just $ "Flag '$' has a value" ~~ n
			(Required _, []) -> Just $ "No value for '$'" ~~ n
			(Required _, [_]) -> Nothing
			(Required _, _:_) -> Just $ "Too much values for '$'" ~~ n
			(List _, []) -> Just $ "No values for '$'" ~~ n
			(List _, _) -> Nothing

isFlag :: String -> Bool
isFlag ('-':'-':s) = not $ null s
isFlag ('-':_:[]) = True
isFlag _ = False

longOpt :: String -> String
longOpt = ("--" ++)

shortOpt :: Char -> String
shortOpt = ('-':) . return
