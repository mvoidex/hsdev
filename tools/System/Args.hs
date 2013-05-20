module System.Args (
	Args, ArgSpec(..), ArgsSpec,
	preview, detailed,
	(|>),
	arg, flag, def, defList, impl, opt, expl, list, desc,
	args, verify, implArgs, args_, cmdline, json,
	has, gets, gets_, get, get_, val, val_,
	split,
	usage,
	unargs, unargsJson
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
import Data.String
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Text as T (unpack)
import qualified Data.Vector as V (toList, fromList)
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

-- | Parse arguments from command line
args :: ArgsSpec -> [String] -> Either String Args
args specs i = do
	as <- args_ i
	left unlines $ verify specs (implArgs specs as)

-- | Verify arguments
verify :: ArgsSpec -> Args -> Either [String] Args
verify specs as = if null errors then Right as else Left errors where
	errors = mapMaybe valid specs ++ unknownKeys

	valid spec
		| not (argOptional spec) && isNothing (argDefault spec) =
			if M.member (argName spec) as then Nothing else Just (argName spec ++ " not specified")
		| isNothing (argImplicit spec) && not (argList spec) = do
			v <- M.lookup (argName spec) as
			if null v then return ("No value for " ++ argName spec) else Nothing
		| not (argList spec) = do
			v <- M.lookup (argName spec) as
			if length v > 1 then return ("Multiple keys: " ++ argName spec) else Nothing
		| otherwise = Nothing

	unknownKeys = map ("Unknown key: " ++ ) $ filter (not . null) $ M.keys as \\ map argName specs

-- | Set default and implicit values
implArgs :: ArgsSpec -> Args -> Args
implArgs specs as = as' where
	as' = M.filterWithKey (\k _ -> not (null k)) $ M.unionWith setImpl (as `M.union` posMap) impls `M.union` defs
	posMap = M.fromList $ zipWith (\s v -> (argName s, [v])) posSpecs posVals where
		posSpecs = filter (\s -> not (argOptional s || isJust (argDefault s))) specs
	posVals = fromMaybe [] $ M.lookup "" as
	defs = M.fromList $ mapMaybe def' specs where
		def' a = liftM ((,) (argName a)) $ argDefault a
	impls = (`M.intersection` as) $ M.fromList $ mapMaybe impl' specs where
		impl' a = liftM ((,) (argName a)) $ argImplicit a
	setImpl [] iv = iv
	setImpl v _ = v

-- | Parse arguments without verification
args_ :: [String] -> Either String Args
args_ i = do
	(posArgs, namedArgs) <- fmap (partition (null . fst)) $ unfoldrM splitKey i
	let
		argsMap = M.fromList . map ((head *** concat) . unzip) . groupBy ((==) `on` fst) . sortBy (comparing fst) $ namedArgs
		posMap = M.singleton "" $ concatMap snd posArgs
	return $ argsMap `M.union` posMap

-- | Parse arguments from command line
cmdline :: ArgsSpec -> String -> Either String Args
cmdline specs = args specs . split

-- | Parse arguments from JSON
json :: ArgsSpec -> Value -> Either String Args
json specs (Object v) = left unlines $ if M.null errs then verify specs (implArgs specs as) else Left (M.elems errs) where
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
val_ :: Read a => String -> Args -> a
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

-- | Convert arguments to command string
unargs :: Args -> [String]
unargs as = pos ++ named where
	pos = fromMaybe [] $ M.lookup "" as
	as' = M.delete "" as
	named = concatMap (uncurry toArgs) (M.toList as')
	toArgs n [] = ['-':n]
	toArgs n vs = concatMap (\v -> ('-':n):[v]) vs

-- | Convert arguments to JSON
unargsJson :: Args -> Value
unargsJson as = Object $ HM.fromList $ M.toList $ M.mapKeys fromString $ M.map toJson as where
	toJson [] = Bool True
	toJson [s] = String $ fromString s
	toJson ss = Array $ V.fromList $ map (String . fromString) ss

splitKey :: [String] -> Either String (Maybe ((String, [String]), [String]))
splitKey [] = Right Nothing
splitKey (('-':key):vals) = Right $ Just ((key, v), vs) where
	v = maybeToList $ do
		val' <- listToMaybe vals
		when ("-" `isPrefixOf` val') mzero
		return val'
	vs = drop (length v) vals
splitKey (key:vals) = Right $ Just (("", [key]), vals)

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
