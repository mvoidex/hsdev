{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances, GeneralizedNewtypeDeriving, LambdaCase #-}

module System.Win32.PowerShell (
	-- * Run PowerShell
	ps,
	-- * PowerShell functions
	startProcess, codepage, outNull,
	-- * PowerShell monad
	PS(..), seqPS, emit, emit_, (=:), invoke,
	-- * Expression
	Args(..), CmdLet(..), Expr(..), compile,
	raw, name, lit, var, bra, cbra, (.=), flag, named, call, cmdlet, lambda, (.|), foreach, filter,
	-- * Convertible
	ToPS(..),
	-- * Escape functions
	translate, translateArg,
	quote, quoteDouble,
	escape
	) where

import Prelude hiding (filter)

import Control.Applicative (Applicative(..))
import Control.Monad
import Control.Monad.Writer
import Data.Char (isAlphaNum)
import Data.List (intercalate)
import qualified Data.List as List (filter)
import Data.Map (Map)
import qualified Data.Map as M

import HsDev.Tools.Base (ToolM, tool_)

ps :: PS a -> ToolM String
ps script = tool_ "powershell" ["-Command", compile $ seqPS script]

-- start-process
startProcess :: String -> [String] -> CmdLet
startProcess n as = cmdlet "start-process" [name n, lit $ intercalate ", " (map translateArg as)] [named "WindowStyle" $ lit "Hidden"]

-- chcp
codepage :: Int -> CmdLet
codepage n = cmdlet "chcp" [lit n] []

-- out-null
outNull :: CmdLet
outNull = cmdlet "out-null" [] []

newtype PS a = PS { unPS :: Writer [Expr] a }
	deriving (Applicative, Functor, Monad, MonadWriter [Expr])

seqPS :: PS a -> Expr
seqPS (PS act) = case execWriter act of
	[] -> error "No expressions"
	es -> foldr1 Sequence es

emit :: Expr -> PS Expr
emit e = PS $ tell [e] >> return e

emit_ :: Expr -> PS ()
emit_ = void . emit

infixr 6 =:
(=:) :: String -> Expr -> PS Expr
n =: expr = emit_ (n .= expr) >> return (var n)

-- | Invoke cmdlet
invoke :: CmdLet -> PS ()
invoke = emit_ . Invoke

-- | Positional and named args
data Args = Args [Expr] (Map String (Maybe Expr))

instance Monoid Args where
	mempty = Args [] M.empty
	mappend (Args lp ln) (Args rp rn) = Args (lp ++ rp) (M.union ln rn)

-- | Call to cmdlet
data CmdLet = CmdLet {
	cmdLet :: Expr,
	cmdArgs :: Args }

-- | Expression
data Expr =
	Emit String |
	-- ^ native expression
	Literal String |
	-- ^ literal
	Var String |
	-- ^ $name
	Bracket Expr |
	-- ^ (expr)
	Assign [String] [Expr] |
	-- ^ $x, $y, ... = expr1, expr2, ...
	Invoke CmdLet |
	-- ^ cmd args... named...
	Lambda [String] ([Expr] -> Expr) |
	-- ^ { param($...); expr }
	Sequence Expr Expr |
	-- ^ expr1; expr2
	Pipe Expr CmdLet
	-- ^ expr | cmd

compile :: Expr -> String
compile (Emit s) = s
compile (Literal l) = l
compile (Var v) = '$':v
compile (Bracket e) = "(" ++ compile e ++ ")"
compile (Assign vs es) = intercalate ", " (map ('$':) vs) ++ " = " ++ intercalate ", " (map (compile . cbra) es)
compile (Invoke (CmdLet e (Args p ns))) = unwords $ List.filter (not . null) [invoke', p', ns'] where
	invoke' = case e of
		Var n -> n
		_ -> unwords ["&", compile $ cbra e]
	p' = intercalate " " $ map (compile . cbra) p
	ns' = intercalate " " $ concatMap named' $ M.toList ns where
		named' :: (String, Maybe Expr) -> [String]
		named' (n, Just v) = ['-':n, compile $ cbra v]
		named' (n, Nothing) = ['-':n]
-- { param($File); $args[0]; }
compile (Lambda ns body) = unwords ["{", param', body', "}"] where
	param' = "param(" ++ intercalate "," ns ++ ");"
	body' = compile $ body (map var ns)
compile (Sequence l r) = compile l ++ "; " ++ compile r
compile (Pipe e c) = compile e ++ " | " ++ compile (Invoke c)

raw :: String -> Expr
raw = Emit

name :: String -> Expr
name = Literal

lit :: ToPS a => a -> Expr
lit = Literal . toPS

var :: String -> Expr
var = Var

bra :: Expr -> Expr
bra = Bracket

cbra :: Expr -> Expr
cbra e@(Literal _) = e
cbra v@(Var _) = v
cbra b = bra b

infixr 6 .= 
(.=) :: String -> Expr -> Expr
v .= e = Assign [v] [e]

flag :: String -> (String, Maybe Expr)
flag n = (n, Nothing)

named :: String -> Expr -> (String, Maybe Expr)
named n e = (n, Just e)

call :: Expr -> [Expr] -> [(String, Maybe Expr)] -> CmdLet
call f pos named' = CmdLet f $ Args pos (M.fromList named')

cmdlet :: String -> [Expr] -> [(String, Maybe Expr)] -> CmdLet
cmdlet n pos = call (name n) pos

lambda :: [String] -> ([Expr] -> Expr) -> Expr
lambda = Lambda

infixr 6 .|
(.|) :: Expr -> CmdLet -> Expr
e .| c = Pipe e c

foreach :: (Expr -> Expr) -> CmdLet
foreach f = cmdlet "%" [f $ var "_"] []

filter :: (Expr -> Expr) -> CmdLet
filter p = cmdlet "?" [p $ var "_"] []

class ToPS a where
	toPS :: a -> String

instance ToPS Int where
	toPS = show

instance ToPS String where
	toPS = translate

instance ToPS Bool where
	toPS True = "$true"
	toPS False = "$false"

instance ToPS a => ToPS [a] where
	toPS = intercalate ", " . map toPS

translate :: String -> String
translate s
	| all (\ch -> isAlphaNum ch || ch `elem` "-_") s = s
	| otherwise = '"' : snd (foldr escape' (True, "\"") s)
	where
		escape' '"' (_, s') = (True, '\\' : '"' : s')
		escape' '\\' (True, s') = (True, '\\' : '\\' : s')
		escape' '\\' (False, s') = (False, '\\' : s')
		escape' c (_, s') = (False, c : s')

translateArg :: String -> String
translateArg s
	| all isAlphaNum s = s
	| otherwise = "'" ++ translate s ++ "'"

quote :: String -> String
quote s = "'" ++ concatMap (\case { '\'' -> "''"; ch -> [ch] }) s ++ "'"

quoteDouble :: String -> String
quoteDouble s = "\"" ++ concatMap (\case { '"' -> "\"\""; ch -> [ch] }) s ++ "\""

escape :: (String -> String) -> String -> String
escape fn s
	| all (\ch -> isAlphaNum ch || ch `elem` "-_") s = s
	| otherwise = fn s
