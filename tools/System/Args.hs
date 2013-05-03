module System.Args (
	args,
	arg, argN, flag,
	parse, force, try,
	split
	) where

import Control.Arrow (first)
import Data.Char (isSpace)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Text.Read

-- | Arguments
data Args = Args {
	namedArgs :: Map String String,
	flagArgs :: [String],
	positionalArgs :: [String] }

instance Monoid Args where
	mempty = Args mempty mempty mempty
	mappend (Args l1 l2 l3) (Args r1 r2 r3) = Args (l1 `mappend` r1) (l2 `mappend` r2) (l3 `mappend` r3)

-- | Parse arguments
-- Takes list of flag-arguments, they have no value, they can be only 'turned on'
args :: [String] -> [String] -> Args
args _ [] = Args M.empty [] []
args flags (('-':key):tl)
	| key `elem` flags = mappend (Args M.empty [key] []) (args flags tl)
	| not (null tl) && (head (head tl) /= '-') = mappend (Args (M.singleton key (head tl)) [] []) (args flags $ tail tl)
	| otherwise = Args (M.singleton key "") [] []
args flags (a:as) = mappend (Args M.empty [] [a]) (args flags as)

-- | Get named argument
arg :: String -> Args -> Either String String
arg name as = maybe (Left $ "Argument '" ++ name ++ "' not found") Right $ M.lookup name $ namedArgs as

-- | Get positional argument
argN :: Int -> Args -> Either String String
argN n as
	| n >= length (positionalArgs as) = Left $ "No argument at index " ++ show n
	| otherwise = Right (positionalArgs as !! n)

flag :: String -> Args -> Bool
flag f as = f `elem` (flagArgs as)

-- | Read argument
parse :: Read a => Either String String -> Either String a
parse (Left e) = Left e
parse (Right s) = maybe (Left $ "Can't parse: '" ++ s ++ "'") Right $ readMaybe s

-- | Force argument
force :: Either String a -> a
force = either error id

-- | Convert to maybe
try :: Either String a -> Maybe a
try = either (const Nothing) Just

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
