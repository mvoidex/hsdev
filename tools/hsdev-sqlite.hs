{-# LANGUAGE OverloadedStrings #-}

module Main (
	main
	) where

import Control.Exception
import Data.List
import Database.SQLite.Simple
import System.Directory
import Text.Format

import HsDev.Util

import Tool

data Command =
	Usages String String |
	Whoat FilePath Int Int |
	Unresolveds FilePath |
	Whois FilePath String |
	Lookup FilePath String |
	Complete FilePath String

instance FromCmd Command where
	cmdP = subparser $ mconcat [
		cmd "usages" "show usages" (Usages <$> strArgument idm <*> strArgument idm),
		cmd "whoat" "whoat" (Whoat <$> strArgument idm <*> argument auto idm <*> argument auto idm),
		cmd "unresolveds" "unresolved symbols" (Unresolveds <$> strArgument idm),
		cmd "whois" "whois" (Whois <$> strArgument idm <*> strArgument idm),
		cmd "lookup" "lookup" (Lookup <$> strArgument idm <*> strArgument idm),
		cmd "complete" "complete" (Complete <$> strArgument idm <*> strArgument idm)]

data Opts = Opts Command FilePath

instance FromCmd Opts where
	cmdP = Opts <$> cmdP <*> (strOption (long "db" <> help "sqlite database") <|> pure "hsdev.db")

main :: IO ()
main = toolMain "hsdev-sqlite" "hsdev commands via sqlite" cmdP main' where
	main' :: Opts -> IO ()
	main' (Opts cmd' db') = bracket (open db') close $ \conn -> case cmd' of
		Usages q n -> do
			rs <- query conn "select m.file, n.line, n.column from modules as m, names as n where (m.id == n.module_id) and (n.resolved_module == ?) and (n.resolved_name == ?);"
				(q, n) :: IO [(String, Int, Int)]
			forM_ rs $ \(file, line, column) -> putStrLn ("{}:{}:{}" ~~ file ~~ line ~~ column)
		Whoat f l c -> do
			f' <- canonicalizePath f
			rs <- query conn "select mdef.name, s.name, s.what from modules as m, names as n, modules as mdef, symbols as s where (m.id == n.module_id) and (mdef.id == s.module_id) and (mdef.name == n.resolved_module) and (s.name == n.resolved_name) and (m.file == ?) and ((?, ?) between (n.line, n.column) and (n.line_to, n.column_to));"
				(f', l, c) :: IO [(String, String, String)]
			forM_ rs $ \(m, sym, what) -> putStrLn ("{}.{} {}" ~~ m ~~ sym ~~ what)
		Unresolveds f -> do
			f' <- canonicalizePath f
			rs <- query conn "select n.qualifier, n.name, n.line, n.column from modules as m, names as n where (m.id == n.module_id) and (m.file == ?) and (n.resolve_error is not null);"
				(Only f') :: IO [(Maybe String, String, Int, Int)]
			forM_ rs $ \(m, nm, line, column) -> putStrLn ("{} at {}:{}" ~~ (maybe nm (\m' -> m' ++ "." ++ nm) m) ~~ line ~~ column)
		Whois f name -> do
			f' <- canonicalizePath f
			let
				(q, ident) = case break (== '.') name of
					(s, "") -> (Nothing, s)
					(pre, post) -> (Just pre, tail post)
			rs <- query conn "select s.name, mdef.name from modules as m, modules as mdef, scopes as sc, symbols as s where (m.id == sc.module_id) and (s.id == sc.symbol_id) and (s.module_id == mdef.id) and (m.file == ?) and (sc.qualifier is ?) and (sc.name == ?);"
				(f', q, ident) :: IO [(String, String)]
			forM_ rs $ \(nm, m) -> putStrLn ("{}.{}" ~~ m ~~ nm)
		Lookup f name -> do
			f' <- canonicalizePath f
			rs <- query conn "select s.name, m.name from projects as p, projects_deps as pdeps, modules as m, modules as srcm, symbols as s where (p.id == pdeps.project_id) and (m.cabal == p.cabal or m.package_name == pdeps.package_name) and (s.module_id == m.id) and (p.cabal == srcm.cabal) and (srcm.file == ?) and (s.name == ?);"
				(f', name) :: IO [(String, String)]
			forM_ rs $ \(nm, m) -> putStrLn ("{}.{}" ~~ m ~~ nm)
		Complete f prefix -> do
			f' <- canonicalizePath f
			rs <- query conn "select c.completion from modules as m, completions as c where (m.id == c.module_id) and (m.file == ?) and (c.completion like ?);"
				(f', prefix ++ "%") :: IO [Only String]
			let
				dots = length . filter (== '.')
				rs' = sort [r | Only r <- rs, dots r == dots prefix]
			forM_ rs' putStrLn
