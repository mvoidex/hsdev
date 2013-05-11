{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, UndecidableInstances #-}

module System.Args (
	Args,
	Command(commandName, commandDesc, commandArgs),
	cmd,
	nodesc,
	($=),
	parseCommand,
	args,
	arg, at, has,
	split
	) where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Writer
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Text.Read (readMaybe)

data Args a = Args {
	posArgs :: [a],
	namedArgs :: Map String a }
		deriving (Eq)

instance Monoid (Args a) where
	mempty = Args [] mempty
	mappend l r = Args (posArgs l `mappend` posArgs r) (namedArgs l `mappend` namedArgs r)

instance Show (Args String) where
	show (Args ps ns) = unwords $ ps ++ concatMap show' (M.toList ns) where
		show' (n, "") = ['-':n]
		show' (n, v) = ['-':n, v]

newtype ParserT a = ParserT { runParserT :: WriterT (Args String) (StateT [String] (Either String)) a }
	deriving (MonadState [String], MonadWriter (Args String), MonadError String, Functor, Monad, Applicative, Alternative)

data Arg =
	PosArg { argName :: String } |
	NamedArg { argName :: String, argArg :: Maybe (String, Maybe String), argOptional :: Bool }
		deriving (Eq)

instance Show Arg where
	show (PosArg n) = n
	show (NamedArg n a o) = (if o then (\s -> "[" ++ s ++ "]") else id) $ ('-':n) ++ maybe "" showArg a where
		showArg (aname, Nothing) = ' ':aname
		showArg (aname, Just adef) = " [" ++ aname ++ "=" ++ adef ++ "]"

newtype Parser a = Parser { runParser :: String -> [(a, String)] }

instance Functor Parser where
	fmap f (Parser g) = Parser $ \s -> map (first f) (g s)

instance Monad Parser where
	return v = Parser $ \s -> [(v, s)]
	(Parser x) >>= f = Parser $ \s -> concatMap f' (x s) where
		f' (v, str) = runParser (f v) str

instance Applicative Parser where
	pure v = Parser $ \s -> [(v, s)]
	f <*> x = Parser $ \s -> concatMap x' (runParser f s) where
		x' (f', str) = do
			(val, str') <- runParser x str
			return (f' val, str')

instance Alternative Parser where
	empty = Parser $ const []
	(Parser l) <|> (Parser r) = Parser $ \s -> l s ++ r s

instance Read Arg where
	readsPrec _ = take 1 . runParser (named' <|> pos') where
		pos' = do
			nm <- name
			if null nm then empty else return (PosArg nm)
		named' = fmap (\n -> n { argOptional = True }) (bra nvp) <|> nvp
		nvp = do
			ch '-'
			nm <- name
			Parser $ \s -> return ((), dropWhile isSpace s)
			a <- defarg <|> valarg <|> pure Nothing
			return $ NamedArg nm a False
		defarg = bra $ do
			a <- name
			v <- (ch '=' >> val) <|> pure ""
			return $ Just (a, Just v)
		valarg = do
			a <- name
			return $ Just (a, Nothing)
		bra p = do
			ch '['
			r <- p
			ch ']'
			return r
		name = Parser $ \s -> filter (not . null . fst) [span (\c -> isDigit c || isAlpha c || (c `elem` "-_")) s]
		val = name <|> pure ""
		ch c = Parser $ \s -> if [c] `isPrefixOf` s then return ((), drop 1 s) else []

data Command = Command {
	commandName :: String,
	commandDesc :: String,
	commandArgs :: [(Arg, String)] }
		deriving (Eq)

instance Show Command where
	show (Command name d as) = intercalate "\n" $ (unwords (name : map (show . fst) as) ++ " -- " ++ d) : map ('\t':) as' where
		as' = mapMaybe argDesc as
		argDesc (_, "") = Nothing
		argDesc (PosArg n, d) = Just $ n ++ " -- " ++ d
		argDesc (NamedArg n _ _, d) = Just $ ('-':n) ++ " -- " ++ d

cmd :: String -> String -> [(Arg, String)] -> Command
cmd = Command

nodesc :: String
nodesc = ""

($=) :: String -> String -> (Arg, String)
name $= d = (read name, d)

parseCommand :: [Command] -> [String] -> Either String (String, Args String)
parseCommand _ [] = Left "No command given"
parseCommand cmds (s:ss) = liftM ((,) s) $ parseCommand' $ filter ((== s) . commandName) cmds where
	parseCommand' cs = foldr (<|>) (Left $ "Can't parse command " ++ s) $ map ((`args` ss) . parse') cs where
		parse' (Command _ _ as) = mapM_ (parseArg . fst) as
		parseArg (PosArg _) = pos
		parseArg (NamedArg n a o) = (if o then opt else id) $ named n (maybe (pure "") parseValue a) where
			parseValue (_, Nothing) = val
			parseValue (_, Just d) = val <|> pure d
			val = withPeek val' where
				val' ('-':_) = throwError "Invalid value"
				val' s = return s

posArg :: a -> Args a
posArg v = Args [v] mempty

namedArg :: String -> a -> Args a
namedArg name v = Args [] (M.singleton name v)

lit :: String -> ParserT ()
lit name = withPeek $ \s -> unless (s == name) (throwError $ "Expected: " ++ name)

pos :: ParserT ()
pos = withPeek $ tell . posArg

named :: String -> ParserT String -> ParserT ()
named name p = do
	str <- get
	let
		(before, after) = break (== ('-':name)) str
	when (null after) $ throwError ("Named argument " ++ name ++ " not found")
	put (tail after)
	(r, _) <- ParserT $ lift $ runWriterT (runParserT p)
	after' <- get
	put $ before ++ after'
	tell $ namedArg name r

notEof :: ParserT a -> ParserT a
notEof p = do
	e <- gets null
	when e $ throwError "EOF"
	p

eof :: ParserT ()
eof = do
	e <- gets null
	unless e $ throwError "EOF expected"

peek :: ParserT String
peek = notEof $ do
	s <- gets head
	modify tail
	return s

withPeek :: (String -> ParserT a) -> ParserT a
withPeek f = notEof $ do
	p <- peek
	catchError (f p) (\e -> unpeek p >> throwError e)

unpeek :: String -> ParserT ()
unpeek s = modify (s:)

opt :: ParserT () -> ParserT ()
opt p = p <|> pure ()

-- | Parse args
args :: ParserT () -> [String] -> Either String (Args String)
args p s = evalStateT (execWriterT (runParserT (p >> eof))) s where

class ReadArg a where
	readArg :: String -> Maybe a

instance ReadArg String where
	readArg = Just

instance ReadArg Int where
	readArg = readMaybe

arg :: ReadArg a => String -> Args String -> Maybe a
arg name pargs = M.lookup name (namedArgs pargs) >>= readArg

at :: ReadArg a => Int -> Args String -> Maybe a
at n pargs
	| n >= length (posArgs pargs) = Nothing
	| otherwise = readArg $ posArgs pargs !! n

has :: String -> Args String -> Bool
has name = M.member name . namedArgs

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

test :: Command
test = cmd "find" "Find symbol" [
	"name" $= nodesc,
	"[-project p]" $= "Project to find in",
	"[-file f]" $= "File to find in",
	"[-module m]" $= "Symbol module",
	"[-cabal [sandbox]]" $= nodesc]
