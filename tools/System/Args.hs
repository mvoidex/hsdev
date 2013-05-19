module System.Args (
	Args, ArgSpec(..), ArgsSpec,
	preview, detailed,
	(|>),
	arg, flag, def, defList, impl, opt, expl, list, desc,
	verify, args, cmdline, json,
	has, gets, gets_, get, get_, val, val_,
	split,
	usage
	) where

import Control.Arrow
import Control.Monad
import Data.Aeson hiding (json, json')
import Data.Char (isSpace)
import Data.Function
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Ord
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Text as T (unpack)
import qualified Data.Vector as V (toList)
import Text.Read (readMaybe)

-- | Named arguments
type Args = Map String [String]

-- | Argument description
data ArgSpec = ArgSpec {
	argName :: String,
	argValueName :: Maybe String,
	argDesc :: Maybe String,
	argOptional :: Bool,
	argList :: Bool,
	argDefault :: Maybe [String],
	argImplicit :: Maybe [String] }

-- | Arguments
type ArgsSpec = [ArgSpec]

-- | Preview argument spec info
preview :: ArgSpec -> String
preview a = bra (argOptional a || isJust (argDefault a)) str where
	str = '-':argName a ++ maybe "" ((' ':) . bra (isJust $ argImplicit a) . ("<" ++) . (++ ">")) (argValueName a) ++ if argList a then "..." else ""
	bra True s = "[" ++ s ++ "]"
	bra False s = s

-- | Detailed info
detailed :: ArgSpec -> Maybe String
detailed a = if hasInfo then Just (unwords info) else Nothing where
	info = catMaybes [
		Just $ '-':argName a,
		fmap (\vn -> vn ++ maybe "" (('=':) . intercalate ",") (argImplicit a)) (argValueName a),
		argDefault a >>= \d -> if null d then Nothing else Just ("(" ++ intercalate "," d ++ ")"),
		fmap (" -- " ++) (argDesc a)]
	hasInfo = isJust (argDesc a) || maybe False (not . null) (argDefault a) || (isJust (argImplicit a) && isJust (argValueName a))

-- | Chain operator
(|>) :: a -> (a -> b) -> b
x |> f = f x
infixl 5 |>

-- | Argument by name
arg :: String -> String -> ArgSpec
arg n m = ArgSpec n (Just m) Nothing False False Nothing Nothing

-- | Flag argument
flag :: String -> ArgSpec
flag f = ArgSpec f Nothing Nothing True False Nothing (Just [])

-- | Set default argument
def :: String -> ArgSpec -> ArgSpec
def v a = a { argDefault = Just [v] }

-- | Set default list argument
defList :: [String] -> ArgSpec -> ArgSpec
defList v a = a { argDefault = Just v }

-- | Set implicit argument
impl :: String -> ArgSpec -> ArgSpec
impl v a = a { argImplicit = Just [v] }

-- | Argument is optional
opt :: ArgSpec -> ArgSpec
opt a = a { argOptional = True }

-- | Argument is explicit
expl :: ArgSpec -> ArgSpec
expl a = a { argOptional = False }

-- | Argument is list
list :: ArgSpec -> ArgSpec
list a = a { argList = True, argOptional = True, argDefault = Just [] }

-- | Set argument description
desc :: String -> ArgSpec -> ArgSpec
desc d a = a { argDesc = Just d }

-- | Verify arguments, set default and implicit values
verify :: ArgsSpec -> Args -> Either [String] Args
verify specs as = if M.null errs then Right as' else Left (M.elems errs) where
	(errs, as') = M.mapEither id $ M.unionWith setImpl (M.mapWithKey checkList as) impls `M.union` defs
	defs = M.fromList $ mapMaybe def' specs where
		def' a = liftM ((,) (argName a)) $ maybe (if argOptional a then Nothing else Just (Left $ argName a ++ " not specified")) (Just . Right) $ argDefault a
	impls = (`M.intersection` as) $ M.fromList $ map impl' specs where
		impl' a = (argName a, maybe (Left $ "No value for " ++ argName a) Right $ argImplicit a)
	checkList nm vals = maybe noArg verifySpec $ find ((== nm) . argName) specs where
		noArg = Left $ "Unknown key: " ++ nm
		verifySpec spec'
			| not (argList spec') && (length vals > 1) = Left $ "Multiple keys: " ++ nm
			| otherwise = Right vals
	setImpl v iv = do
		v' <- v
		case v' of
			[] -> iv
			_ -> Right v'

-- | Parse arguments from command line
args :: ArgsSpec -> [String] -> Either String Args
args specs i = as >>= left unlines . verify specs where
	as = liftM (M.fromList . map ((head *** concat) . unzip) . groupBy ((==) `on` fst) . sortBy (comparing fst)) $ unfoldrM splitKey i

-- | Parse arguments from command line
cmdline :: ArgsSpec -> String -> Either String Args
cmdline specs = args specs . split

-- | Parse arguments from JSON
json :: ArgsSpec -> Value -> Either String Args
json specs (Object v) = left unlines $ if M.null errs then verify specs as else Left (M.elems errs) where
	(errs, as) = M.mapEither id $ M.map parseArg $ M.mapKeys T.unpack $ M.fromList $ HM.toList v
	parseArg Null = Right []
	parseArg (Bool b) = Right [show b]
	parseArg (Number n) = Right [show n]
	parseArg (String s) = Right [T.unpack s]
	parseArg (Array a) = liftM concat $ mapM parseArg $ V.toList a
	parseArg _ = Left "Invalid value"
json _ _ = Left "Invalid json value"

-- | Check whether argument exists
has :: String -> Args -> Bool
has = M.member

-- | Get argument list
gets :: String -> Args -> Maybe [String]
gets = M.lookup

-- | Force get argument list
gets_ :: String -> Args -> [String]
gets_ name as = fromMaybe (error $ "No argument " ++ name) $ gets name as

-- | Get argument
get :: String -> Args -> Maybe String
get name as = M.lookup name as >>= listToMaybe

-- | Force get argument
get_ :: String -> Args -> String
get_ name as = fromMaybe (error $ "No argument " ++ name) $ get name as

-- | Read argument
val :: Read a => String -> Args -> Maybe a
val name as = get name as >>= readMaybe

-- | Force read argument
val_ :: Read a => String -> Args -> Maybe a
val_ name as = fromMaybe (error $ "Can't read " ++ name) $ val name as

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

-- | Print usage
usage :: ArgsSpec -> [String]
usage as = line : det where
	line = unwords $ map preview as
	det = mapMaybe (fmap ('\t':) . detailed) as

splitKey :: [String] -> Either String (Maybe ((String, [String]), [String]))
splitKey [] = Right Nothing
splitKey (('-':key):vals) = Right $ Just ((key, v), vs) where
	(v, vs) = break ("-" `isPrefixOf`) vals
splitKey (key:_) = Left $ "Invalid input: " ++ key

unfoldrM :: Monad m => (a -> m (Maybe (b, a))) -> a -> m [b]
unfoldrM f x = f x >>= maybe (return []) (\(h, t) -> liftM (h:) (unfoldrM f t))

--test :: ArgsSpec
--test = [
--	arg "name" "symbol-name",
--	arg "project" "p" |> opt |> desc "Project to find in",
--	arg "module" "m" |> opt |> desc "Symbol module",
--	arg "cabal" "sandbox" |> opt |> impl "cabal" |> desc "Cabal database to find in",
--	flag "test",
--	arg "timeout" "ms" |> def "100",
--	arg "g" "ghc_opts" |> list]
