{-# LANGUAGE OverloadedStrings #-}

module Main (
	main
	) where

import Control.Lens hiding ((.=))
import Control.Exception
import Data.Aeson
import qualified Data.ByteString.Lazy as L
import Data.List
import qualified Data.Text as T
import Database.SQLite.Simple
import System.Directory
import Text.Format

import System.Directory.Paths
import HsDev.Util
import HsDev.Server.Types
import HsDev.Symbols.Name

import Tool

data Opts = Opts Command FilePath

instance FromCmd Opts where
	cmdP = Opts <$> cmdP <*> (strOption (long "db" <> help "sqlite database") <|> pure "hsdev.db")

main :: IO ()
main = toolMain "hsdev-sqlite" "hsdev commands via sqlite" cmdP $ \(Opts cmd' db') -> bracket (open db') close (main' cmd') where
	main' :: Command -> Connection -> IO ()
	main' cmd' conn = do
		execute_ conn "pragma case_sensitive_like = true;"
		cmd'' <- canonicalize cmd'
		runCommand cmd'' >>= L.putStrLn . encode
		where
			runCommand :: Command -> IO Value

			runCommand Ping = toValue $ return $ object ["message" .= ("pong" :: String)]
			runCommand (Listen (Just l)) = notImplemented
			runCommand (Listen Nothing) = notImplemented
			runCommand (SetLogLevel l) = notImplemented
			runCommand (AddData fs) = notImplemented
			runCommand Dump = notImplemented
			runCommand (DumpSqlite fpath) = notImplemented
			runCommand (Scan projs cabal sboxes fs paths' ghcs' docs' infer') = notImplemented
			runCommand (RefineDocs projs fs ms) = notImplemented
			runCommand (InferTypes projs fs ms) = notImplemented
			runCommand (Remove projs cabal sboxes files) = notImplemented
			runCommand RemoveAll = notImplemented
			runCommand InfoPackages = notImplemented
			runCommand InfoProjects = notImplemented
			runCommand InfoSandboxes = notImplemented
			runCommand (InfoSymbol sq fs h _) = notImplemented
			runCommand (InfoModule sq fs h i) = notImplemented
			runCommand (InfoProject (Left projName)) = notImplemented
			runCommand (InfoProject (Right projPath)) = notImplemented
			runCommand (InfoSandbox sandbox') = notImplemented
			runCommand (Lookup nm fpath) = toValue $ do
				rs <- query conn "select s.name, m.name from projects as p, projects_deps as pdeps, modules as m, modules as srcm, symbols as s where (p.id == pdeps.project_id) and (m.cabal == p.cabal or m.package_name == pdeps.package_name) and (s.module_id == m.id) and (p.cabal == srcm.cabal) and (srcm.file == ?) and (s.name == ?);"
					(fpath ^. path, nm) :: IO [(String, String)]
				return $ map (\(nm', m) -> object [
					"name" .= nm',
					"module" .= m]) rs
			runCommand (Whois nm fpath) = toValue $ do
				let
					q = nameModule $ toName nm
					ident = nameIdent $ toName nm
				rs <- query conn "select s.name, mdef.name from modules as m, modules as mdef, scopes as sc, symbols as s where (m.id == sc.module_id) and (s.id == sc.symbol_id) and (s.module_id == mdef.id) and (m.file == ?) and (sc.qualifier is ?) and (sc.name == ?);"
					(fpath ^. path, q, ident) :: IO [(String, String)]
				return $ map (\(nm', m) -> object [
					"name" .= nm',
					"module" .= m]) rs
			runCommand (Whoat l c fpath) = toValue $ do
				rs <- query conn "select mdef.name, s.name, s.what from modules as m, names as n, modules as mdef, symbols as s, projects as p, projects_modules_scope as msc where (m.id == n.module_id) and (mdef.id == s.module_id) and (mdef.name == n.resolved_module) and (s.name == n.resolved_name) and (p.cabal == m.cabal) and (p.id == msc.project_id) and (mdef.id == msc.module_id) and (m.file == ?) and ((?, ?) between (n.line, n.column) and (n.line_to, n.column_to));"
					(fpath ^. path, l, c) :: IO [(String, String, String)]
				return $ map (\(m, sym, what) -> object [
					"module" .= m,
					"name" .= sym,
					"what" .= what]) rs
			runCommand (ResolveScopeModules sq fpath) = notImplemented
			runCommand (ResolveScope sq fpath) = notImplemented
			runCommand (FindUsages nm) = toValue $ do
				let
					q = nameModule $ toName nm
					ident = nameIdent $ toName nm
				rs <- query conn "select m.file, n.line, n.column from modules as m, names as n where (m.id == n.module_id) and (n.resolved_module == ? or ? is null) and (n.resolved_name == ?);"
					(q, q, ident) :: IO [(String, Int, Int)]
				return $ map (\(file, line, column) -> object [
					"file" .= file,
					"line" .= line,
					"column" .= column]) rs
			runCommand (Complete input True fpath) = toValue $ do
				rs <- query conn "select s.name from projects_modules_scope as msc, projects as p, modules as srcm, modules as m, symbols as s where (srcm.cabal == p.cabal) and (p.id == msc.project_id) and (msc.module_id == m.id) and (s.module_id == m.id) and (msrc.file == ?) and (s.name like ?) union select m.name from projects_modules_scope as msc, projects as p, modules as srcm, modules as m where (srcm.cabal == p.cabal) and (p.id == msc.project_id) and (m.id == msc.module_id) and (msrc.file == ?) and (m.name like ?);"
					(fpath ^. path, input `T.append` "%", fpath ^. path, input `T.append` "%") :: IO [Only T.Text]
				let
					dots = T.length . T.filter (== '.')
				return $ ordNub [T.intercalate "." . take (succ $ dots input) . T.splitOn (".") $ r | Only r <- rs]
			runCommand (Complete input False fpath) = toValue $ do
				rs <- query conn "select c.completion from modules as m, completions as c where (m.id == c.module_id) and (m.file == ?) and (c.completion like ?);"
					(fpath ^. path, input `T.append` "%") :: IO [Only T.Text]
				let
					dots = T.length . T.filter (== '.')
				return $ ordNub [T.intercalate "." . take (succ $ dots input) . T.splitOn (".") $ r | Only r <- rs]
			runCommand (Hayoo hq p ps) = notImplemented
			runCommand (CabalList packages') = notImplemented
			runCommand (UnresolvedSymbols fs) = toValue $ liftM concat $ forM fs $ \f -> do
				rs <- query conn "select n.qualifier, n.name, n.line, n.column from modules as m, names as n where (m.id == n.module_id) and (m.file == ?) and (n.resolve_error is not null);"
					(Only $ f ^. path) :: IO [(Maybe String, String, Int, Int)]
				return $ map (\(m, nm, line, column) -> object [
					"qualifier" .= m,
					"name" .= nm,
					"line" .= line,
					"column" .= column]) rs
			runCommand (Lint fs) = notImplemented
			runCommand (Check fs ghcs' clear) = notImplemented
			runCommand (CheckLint fs ghcs' clear) = notImplemented
			runCommand (Types fs ghcs' clear) = notImplemented
			runCommand (AutoFix ns) = notImplemented
			runCommand (Refactor ns rest isPure) = notImplemented
			runCommand (Rename nm newName fpath) = notImplemented
			runCommand (GhcEval exprs mfile) = notImplemented
			runCommand Langs = notImplemented
			runCommand Flags = notImplemented
			runCommand (Link hold) = notImplemented
			runCommand Exit = notImplemented

	notImplemented :: IO Value
	notImplemented = toValue $ return ("not implemented" :: T.Text)

toValue :: (ToJSON a, Monad m) => m a -> m Value
toValue = liftM toJSON
