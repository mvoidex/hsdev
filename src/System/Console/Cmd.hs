{-# LANGUAGE FlexibleInstances #-}

module System.Console.Cmd (
	Cmd(..), Arg(..), Opt(..),
	flag, req, list,
	desc, alias, short,
	parse, info,

	module Data.Help
	) where

import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid

import Data.Help
import Text.Format

data Cmd = Cmd {
	args :: [String],
	opts :: Map String [String] }
		deriving (Eq, Show)

instance Monoid Cmd where
	mempty = Cmd [] M.empty
	(Cmd largs lopts) `mappend` (Cmd rargs ropts) = Cmd (largs ++ rargs) (M.unionWith (++) lopts ropts)

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

parse :: [Opt] -> [String] -> Either String Cmd
parse os = join . liftM (verify . mconcat) . unfoldrM parseCmd where
	parseCmd :: [String] -> Either String (Maybe (Cmd, [String]))
	parseCmd [] = Right Nothing
	parseCmd (cmd:cmds)
		| isFlag cmd = do
			opt' <- lookOpt cmd os
			case optArg opt' of
				Flag -> Right $ Just (Cmd [] $ M.singleton (optName opt') [], cmds)
				Required _ -> case cmds of
					(value:cmds')
						| not (isFlag value) -> Right $ Just (Cmd [] $ M.singleton (optName opt') [value], cmds')
						| otherwise -> Left $ "No value specified for option '$'" ~~ optName opt'
					[] -> Left $ "No value specified for option '" ++ optName opt' ++ "'"
				List _ -> case cmds of
					(value:cmds')
						| not (isFlag value) -> Right $ Just (Cmd [] $ M.singleton (optName opt') [value], cmds')
						| otherwise -> Left $ "No value specified for option '$'" ~~ optName opt'
					[] -> Left $ "No value specified for option '$'" ~~ optName opt'
		| otherwise = Right $ Just (Cmd [cmd] M.empty, cmds)
	verify :: Cmd -> Either String Cmd
	verify = withOpts $ fmap M.fromList . mapM (uncurry verify') . M.toList where
		withOpts :: Functor f => (Map String [String] -> f (Map String [String])) -> Cmd -> f Cmd
		withOpts f c = (\v -> c { opts = v }) <$> f (opts c)
		verify' :: String -> [String] -> Either String (String, [String])
		verify' n v = case findOpt n os of
			Nothing -> Left $ format "Invalid option '$'" ~~ n
			Just opt -> maybe (Right (n, v)) Left $ case (optArg opt, v) of
				(Flag, []) -> Nothing
				(Flag, _) -> Just $ "Flag '$' has a value" ~~ n
				(Required _, []) -> Just $ "No value for '$'" ~~ n
				(Required _, [_]) -> Nothing
				(Required _, _:_) -> Just $ "Too much values for '$'" ~~ n
				(List _, []) -> Just $ "No values for '$'" ~~ n
				(List _, _) -> Nothing

	lookOpt :: String -> [Opt] -> Either String Opt
	lookOpt n = maybe (Left $ "Invalid option '$'" ~~ n) Right . findOpt (dropWhile (== '-') n)

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

isFlag :: String -> Bool
isFlag ('-':'-':s) = not $ null s
isFlag ('-':_:[]) = True
isFlag _ = False

longOpt :: String -> String
longOpt = ("--" ++)

shortOpt :: Char -> String
shortOpt = ('-':) . return
