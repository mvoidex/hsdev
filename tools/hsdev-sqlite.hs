{-# LANGUAGE OverloadedStrings, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-matches #-}

module Main (
	main
	) where

import Control.Lens hiding ((.=))
import Control.Exception
import Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Language.Haskell.Extension
import System.Directory
import Text.Format

import System.Directory.Paths
import HsDev.Util
import HsDev.Server.Types
import HsDev.Symbols
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
			runCommand InfoPackages = toValue $
				(query_ conn "select package_name, package_version from package_dbs;" :: IO [ModulePackage])
			runCommand InfoProjects = toValue $ do
				ps <- query_ conn "select p.id, p.name, p.cabal, p.version from projects as p;" :: IO [Only Int :. Project]
				forM ps $ \(Only pid :. proj) -> do
					libs <- query conn "select l.modules, b.depends, b.language, b.extensions, b.ghc_options, b.source_dirs, b.other_modules from libraries as l, build_infos as b where (l.project_id == ?) and (l.build_info_id == b.id);"
						(Only pid) :: IO [Library]
					exes <- query conn "select e.name, e.path, b.depends, b.language, b.extensions, b.ghc_options, b.source_dirs, b.other_modules from executables as e, build_infos as b where (e.project_id == ?) and (e.build_info_id == b.id);"
						(Only pid) :: IO [Executable]
					tsts <- query conn "select t.name, t.enabled, t.main, b.depends, b.language, b.extensions, b.ghc_options, b.source_dirs, b.other_modules from tests as t, build_infos as b where (t.project_id == ?) and (t.build_info_id == b.id);"
						(Only pid) :: IO [Test]
					return $
						set (projectDescription . _Just . projectLibrary) (listToMaybe libs) .
						set (projectDescription . _Just . projectExecutables) exes .
						set (projectDescription . _Just . projectTests) tsts $
						proj
			runCommand InfoSandboxes = notImplemented
			runCommand (InfoSymbol sq fs True _) = toValue $ do
				rs <- query conn "select s.name, m.name, m.file, m.cabal, m.install_dirs, m.package_name, m.package_version, m.other_location from modules as m, symbols as s where (m.id == s.module_id) and (s.name like ?);"
					(Only $ qlike sq) :: IO [SymbolId]
				return rs
			runCommand (InfoSymbol sq fs False _) = toValue $ do
				rs <- query conn "select s.name, m.name, m.file, m.cabal, m.install_dirs, m.package_name, m.package_version, m.other_location, s.docs, s.line, s.column, s.what, s.type, s.parent, s.constructors, s.args, s.context, s.associate, s.pat_type, s.pat_constructor from modules as m, symbols as s where (m.id == s.module_id) and (s.name like ?);"
					(Only $ qlike sq) :: IO [Symbol]
				return rs
			runCommand (InfoModule sq fs h i) = toValue $ do
				rs <- query conn "select m.id, m.name, m.file, m.cabal, m.install_dirs, m.package_name, m.package_version, m.other_location from modules as m where (m.name like ?);"
					(Only $ qlike sq) :: IO [Only Int :. ModuleId]
				if h
					then return (toJSON $ map (\(_ :. m) -> m) rs)
					else liftM toJSON $ forM rs $ \(Only mid :. mheader) -> do
						[(docs, fixities)] <- query conn "select m.docs, m.fixities from modules as m where (m.id == ?);"
							(Only mid) :: IO [(Maybe T.Text, Maybe Value)]
						let
							fixities' = fromMaybe [] (fixities >>= fromJSON')
						exports' <- query conn "select s.name, m.name, m.file, m.cabal, m.install_dirs, m.package_name, m.package_version, m.other_location, s.docs, s.line, s.column, s.what, s.type, s.parent, s.constructors, s.args, s.context, s.associate, s.pat_type, s.pat_constructor from modules as m, symbols as s, exports as e where (m.id == s.module_id) and (e.module_id == ?) and (e.symbol_id == s.id);"
							(Only mid) :: IO [Symbol]
						return $ Module mheader docs exports' fixities' mempty Nothing
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

	qlike :: SearchQuery -> T.Text
	qlike (SearchQuery input stype) = case stype of
		SearchExact -> input
		SearchPrefix -> input `T.append` "%"
		SearchInfix -> "%" `T.append` input `T.append` "%"
		SearchSuffix -> "%" `T.append` input
		SearchRegex -> error "Can't search for regex in sqlite"

toValue :: (ToJSON a, Monad m) => m a -> m Value
toValue = liftM toJSON

instance ToField Value where
	toField = SQLBlob . L.toStrict . encode

instance FromField Value where
	fromField fld = case fieldData fld of
		SQLText s -> either fail return . eitherDecode . L.fromStrict . T.encodeUtf8 $ s
		SQLBlob s -> either fail return . eitherDecode . L.fromStrict $ s
		_ -> fail "invalid json field type"

instance FromRow ModulePackage where
	fromRow = ModulePackage <$> field <*> (fromMaybe T.empty <$> field)

instance FromRow ModuleId where
	fromRow = do
		name <- field
		file <- field
		cabal <- field
		dirs <- field
		pname <- field
		pver <- field
		other <- field

		mloc <- maybe (fail "Can't parse module location") return $ msum [
			FileModule <$> file <*> pure (project <$> cabal),
			InstalledModule <$> (fromJSON' =<< dirs) <*> pure (ModulePackage <$> pname <*> pver) <*> pure name,
			OtherLocation <$> other]

		return $ ModuleId name mloc

instance FromRow SymbolId where
	fromRow = SymbolId <$> field <*> fromRow

instance FromRow Symbol where
	fromRow = Symbol <$> fromRow <*> field <*> pos <*> infoP where
		pos = do
			line <- field
			column <- field
			return $ Position <$> line <*> column
		infoP = do
			what <- str' <$> field
			ty <- field
			parent <- field
			ctors <- field
			args <- field
			ctx <- field
			assoc <- field
			patTy <- field
			patCtor <- field
			maybe (fail "Can't parse symbol info") return $ case what of
				"function" -> return $ Function ty
				"method" -> Method <$> pure ty <*> parent
				"selector" -> Selector <$> pure ty <*> parent <*> (fromJSON' =<< ctors)
				"ctor" -> Constructor <$> (fromJSON' =<< args) <*> parent
				"type" -> Type <$> (fromJSON' =<< args) <*> (fromJSON' =<< ctx)
				"newtype" -> NewType <$> (fromJSON' =<< args) <*> (fromJSON' =<< ctx)
				"data" -> Data <$> (fromJSON' =<< args) <*> (fromJSON' =<< ctx)
				"class" -> Class <$> (fromJSON' =<< args) <*> (fromJSON' =<< ctx)
				"type-family" -> TypeFam <$> (fromJSON' =<< args) <*> (fromJSON' =<< ctx) <*> pure assoc
				"data-family" -> DataFam <$> (fromJSON' =<< args) <*> (fromJSON' =<< ctx) <*> pure assoc
				"pat-ctor" -> PatConstructor <$> (fromJSON' =<< args) <*> pure patTy
				"pat-selector" -> PatSelector <$> pure ty <*> pure patTy <*> patCtor
				_ -> Nothing
		str' :: String -> String
		str' = id

instance FromRow Project where
	fromRow = do
		name <- field
		cabal <- field
		ver <- field
		return $ Project name (takeDir cabal) cabal $ Just $ ProjectDescription ver Nothing [] []

instance FromRow Library where
	fromRow = do
		mods <- field >>= maybe (fail "Error parsing library modules") return . fromJSON'
		binfo <- fromRow
		return $ Library mods binfo

instance FromRow Executable where
	fromRow = Executable <$> field <*> field <*> fromRow

instance FromRow Test where
	fromRow = Test <$> field <*> field <*> field <*> fromRow

instance FromRow Info where
	fromRow = Info <$>
		(field >>= maybe (fail "Error parsing depends") return . fromJSON') <*>
		field <*>
		(field >>= maybe (fail "Error parsing extensions") return . fromJSON') <*>
		(field >>= maybe (fail "Error parsing extensions") return . fromJSON') <*>
		(field >>= maybe (fail "Error parsing extensions") return . fromJSON') <*>
		(field >>= maybe (fail "Error parsing extensions") return . fromJSON')

instance FromField Language where
	fromField fld = case fieldData fld of
		SQLText txt -> parseDT "Language" (T.unpack txt)
		_ -> fail "Can't parse language, invalid type"

fromJSON' :: FromJSON a => Value -> Maybe a
fromJSON' v = case fromJSON v of
	A.Success r -> Just r
	_ -> Nothing
