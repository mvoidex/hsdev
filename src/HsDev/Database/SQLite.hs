{-# LANGUAGE OverloadedStrings, TypeOperators, TypeApplications #-}

module HsDev.Database.SQLite (
	initialize, purge,
	query, query_, queryNamed, execute, execute_, executeNamed,
	updatePackageDb, removePackageDb, insertPackageDb,
	updateProject, removeProject, insertProject, insertBuildInfo,
	updateModule,
	removeModule, removeModuleSymbols,
	insertModule, insertModuleSymbols,
	upsertModule,
	lookupModuleLocation, lookupModule, insertLookupModule,
	lookupSymbol, insertLookupSymbol,
	lastRow,

	loadModule, loadModules,
	loadProject,

	-- * Reexports
	module Database.SQLite.Simple,
	module HsDev.Database.SQLite.Select,
	module HsDev.Database.SQLite.Instances
	) where

import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson hiding (Error)
import Data.Generics.Uniplate.Operations
import Data.Maybe
import Data.String
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple hiding (query, query_, queryNamed, execute, execute_, executeNamed)
import qualified Database.SQLite.Simple as SQL (query, query_, queryNamed, execute, execute_, executeNamed)
import Distribution.Text (display)
import Language.Haskell.Exts.Syntax hiding (Name, Module)
import Language.Haskell.Extension ()
import System.Log.Simple
import Text.Format

import System.Directory.Paths

import HsDev.Database.SQLite.Instances
import HsDev.Database.SQLite.Schema
import HsDev.Database.SQLite.Select
import qualified HsDev.Display as Display
import HsDev.Error
import HsDev.PackageDb.Types
import HsDev.Project.Types
import HsDev.Symbols.Name
import HsDev.Symbols.Parsed
import HsDev.Symbols.Types hiding (loadProject)
import qualified HsDev.Symbols.Parsed as P
import qualified HsDev.Symbols.Name as Name
import HsDev.Server.Types
import HsDev.Util

-- | Initialize database
initialize :: String -> IO Connection
initialize p = do
	conn <- open p
	[Only hasTables] <- SQL.query_ conn "select count(*) > 0 from sqlite_master where type == 'table';"
	goodVersion <- if hasTables
		then do
			[Only equalVersion] <- SQL.query conn "select sum(json(value) == json(?)) > 0 from hsdev where option == 'version';" (Only $ toJSON version)
			return equalVersion
		else return True
	when (not goodVersion) $ do
		-- TODO: Completely drop schema to reinitialize
		hsdevError $ OtherError "Not implemented: dropping schema of db"
	when (not hasTables || not goodVersion) $ withTransaction conn $ do
		mapM_ (SQL.execute_ conn) commands
		SQL.execute @(Text, Value) conn "insert into hsdev values (?, ?);" ("version", toJSON version)
	return conn

purge :: SessionMonad m => m ()
purge = do
	tables <- query_ @(Only String) "select name from sqlite_master where type == 'table';"
	forM_ tables $ \(Only table) ->
		execute_ $ fromString $ "delete from {};" ~~ table

query :: (ToRow q, FromRow r, SessionMonad m) => Query -> q -> m [r]
query q' params = do
	conn <- serverSqlDatabase
	liftIO $ SQL.query conn q' params

query_ :: (FromRow r, SessionMonad m) => Query -> m [r]
query_ q' = do
	conn <- serverSqlDatabase
	liftIO $ SQL.query_ conn q'

queryNamed :: (FromRow r, SessionMonad m) => Query -> [NamedParam] -> m [r]
queryNamed q' ps' = do
	conn <- serverSqlDatabase
	liftIO $ SQL.queryNamed conn q' ps'

execute :: (ToRow q, SessionMonad m) => Query -> q -> m ()
execute q' params = do
	conn <- serverSqlDatabase
	liftIO $ SQL.execute conn q' params

execute_ :: SessionMonad m => Query -> m ()
execute_ q' = do
	conn <- serverSqlDatabase
	liftIO $ SQL.execute_ conn q'

executeNamed :: SessionMonad m => Query -> [NamedParam] -> m ()
executeNamed q' ps' = do
	conn <- serverSqlDatabase
	liftIO $ SQL.executeNamed conn q' ps'

updatePackageDb :: SessionMonad m => PackageDb -> [ModulePackage] -> m ()
updatePackageDb pdb pkgs = scope "update-package-db" $ do
	sendLog Trace $ "update package-db: {}" ~~ Display.display pdb
	removePackageDb pdb
	insertPackageDb pdb pkgs

removePackageDb :: SessionMonad m => PackageDb -> m ()
removePackageDb pdb = scope "remove-package-db" $
	execute "delete from package_dbs where package_db == ?;" (Only pdb)

insertPackageDb :: SessionMonad m => PackageDb -> [ModulePackage] -> m ()
insertPackageDb pdb pkgs = scope "insert-package-db" $ forM_ pkgs $ \pkg -> do
	execute
		"insert into package_dbs (package_db, package_name, package_version) values (?, ?, ?);"
		(pdb, pkg ^. packageName, pkg ^. packageVersion)

updateProject :: SessionMonad m => Project -> Maybe PackageDbStack -> m ()
updateProject proj pdbs = scope "update-project" $ do
	sendLog Trace $ "update project: {}" ~~ Display.display proj
	removeProject proj
	insertProject proj pdbs

removeProject :: SessionMonad m => Project -> m ()
removeProject proj = scope "remove-project" $ do
	projId <- query @_ @(Only Int) "select id from projects where cabal == ?;" (Only $ proj ^. projectCabal)
	case projId of
		[] -> return ()
		pids -> do
			when (length pids > 1) $ do
				sendLog Warning $ "multiple projects for cabal {} found" ~~ (proj ^. projectCabal)
			forM_ pids $ \pid -> do
				bids <- query @_ @(Only Int) "select build_info_id from targets where project_id == ?;" pid
				execute "delete from projects where id == ?;" pid
				execute "delete from libraries where project_id == ?;" pid
				execute "delete from executables where project_id == ?;" pid
				execute "delete from tests where project_id == ?;" pid
				forM_ bids $ \bid -> execute "delete from build_infos where id == ?;" bid

insertProject :: SessionMonad m => Project -> Maybe PackageDbStack -> m ()
insertProject proj pdbs = scope "insert-project" $ do
	execute "insert into projects (name, cabal, version, package_db_stack) values (?, ?, ?, ?);" (
		proj ^. projectName,
		proj ^. projectCabal . path,
		proj ^? projectDescription . _Just . projectVersion,
		fmap (encode . packageDbs) pdbs)
	projId <- lastRow

	forM_ (proj ^? projectDescription . _Just . projectLibrary . _Just) $ \lib -> do
		buildInfoId <- insertBuildInfo $ lib ^. libraryBuildInfo
		execute "insert into libraries (project_id, modules, build_info_id) values (?, ?, ?);"
			(projId, encode $ lib ^. libraryModules, buildInfoId)

	forM_ (proj ^.. projectDescription . _Just . projectExecutables . each) $ \exe -> do
		buildInfoId <- insertBuildInfo $ exe ^. executableBuildInfo
		execute "insert into executables (project_id, name, path, build_info_id) values (?, ?, ?, ?);"
			(projId, exe ^. executableName, exe ^. executablePath . path, buildInfoId)

	forM_ (proj ^.. projectDescription . _Just . projectTests . each) $ \test -> do
		buildInfoId <- insertBuildInfo $ test ^. testBuildInfo
		execute "insert into tests (project_id, name, enabled, main, build_info_id) values (?, ?, ?, ?, ?);"
			(projId, test ^. testName, test ^. testEnabled, test ^? testMain . _Just . path, buildInfoId)

insertBuildInfo :: SessionMonad m => Info -> m Int
insertBuildInfo info = scope "insert-build-info" $ do
	execute "insert into build_infos (depends, language, extensions, ghc_options, source_dirs, other_modules) values (?, ?, ?, ?, ?, ?);" (
			encode $ info ^. infoDepends,
			fmap display $ info ^. infoLanguage,
			encode $ map display $ info ^. infoExtensions,
			encode $ info ^. infoGHCOptions,
			encode $ info ^.. infoSourceDirs . each . path,
			encode $ info ^. infoOtherModules)
	lastRow

updateModule :: SessionMonad m => InspectedModule -> m ()
updateModule im = scope "update-module" $ do
	_ <- upsertModule im
	insertModuleSymbols im

removeModule :: SessionMonad m => Int -> m ()
removeModule mid = scope "remove-module" $ do
	execute "delete from imports where module_id == ?;" (Only mid)
	execute "delete from exports where module_id == ?;" (Only mid)
	execute "delete from scopes where module_id == ?;" (Only mid)
	execute "delete from names where module_id == ?;" (Only mid)
	execute "delete from modules where id == ?;" (Only mid)

removeModuleSymbols :: SessionMonad m => Int -> m ()
removeModuleSymbols mid = scope "remove-module-symbols" $ do
	sids <- query @_ @(Only Int) "select id from symbols where module_id == ?;" (Only mid)
	forM_ sids $ \sid -> do
		execute "delete from exports where symbol_id == ?;" sid
		execute "delete from scopes where symbol_id == ?;" sid
	execute "delete from symbols where module_id == ?;" (Only mid)

insertModule :: SessionMonad m => InspectedModule -> m ()
insertModule im = scope "insert-module" $ do
	execute "insert into modules (file, cabal, install_dirs, package_name, package_version, installed_name, other_location, name, docs, fixities, tags, inspection_error, inspection_time, inspection_opts) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);" $ (
		im ^? inspectedKey . moduleFile . path,
		im ^? inspectedKey . moduleProject . _Just . projectCabal,
		fmap (encode . map (view path)) (im ^? inspectedKey . moduleInstallDirs),
		im ^? inspectedKey . modulePackage . _Just . packageName,
		im ^? inspectedKey . modulePackage . _Just . packageVersion,
		im ^? inspectedKey . installedModuleName,
		im ^? inspectedKey . otherLocationName)
		:. (
		msum [im ^? inspected . moduleId . moduleName, im ^? inspectedKey . installedModuleName],
		im ^? inspected . moduleDocs,
		fmap encode $ im ^? inspected . moduleFixities,
		encode $ asDict $ im ^. inspectionTags,
		fmap show $ im ^? inspectionResult . _Left)
		:.
		fromMaybe InspectionNone (im ^? inspection)
		where
			asDict tags = object [fromString (Display.display t) .= True | t <- S.toList tags]

upsertModule :: SessionMonad m => InspectedModule -> m Int
upsertModule im = scope "upsert-module" $ do
	mmid <- lookupModuleLocation (im ^. inspectedKey)
	case mmid of
		Just mid -> do
			execute "update modules set file = ?, cabal = ?, install_dirs = ?, package_name = ?, package_version = ?, installed_name = ?, other_location = ?, name = ?, docs = ?, fixities = ?, tags = ?, inspection_error = ?, inspection_time = ?, inspection_opts = ? where id == ?;"
				(moduleData :. (Only mid))
			return mid
		Nothing -> do
			execute "insert into modules (file, cabal, install_dirs, package_name, package_version, installed_name, other_location, name, docs, fixities, tags, inspection_error, inspection_time, inspection_opts) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);"
				moduleData
			lastRow
	where
		moduleData = (
			im ^? inspectedKey . moduleFile . path,
			im ^? inspectedKey . moduleProject . _Just . projectCabal,
			fmap (encode . map (view path)) (im ^? inspectedKey . moduleInstallDirs),
			im ^? inspectedKey . modulePackage . _Just . packageName,
			im ^? inspectedKey . modulePackage . _Just . packageVersion,
			im ^? inspectedKey . installedModuleName,
			im ^? inspectedKey . otherLocationName)
			:. (
			msum [im ^? inspected . moduleId . moduleName, im ^? inspectedKey . installedModuleName],
			im ^? inspected . moduleDocs,
			fmap encode $ im ^? inspected . moduleFixities,
			encode $ asDict $ im ^. inspectionTags,
			fmap show $ im ^? inspectionResult . _Left)
			:.
			fromMaybe InspectionNone (im ^? inspection)
		asDict tags = object [fromString (Display.display t) .= True | t <- S.toList tags]

insertModuleSymbols :: SessionMonad m => InspectedModule -> m ()
insertModuleSymbols im = scope "insert-module-symbols" $ do
	[Only mid] <- queryNamed "select id from modules where ((file is null and :file is null) or file = :file) and ((package_name is null and :package_name is null) or package_name = :package_name) and ((package_version is null and :package_version is null) or package_version = :package_version) and ((other_location is null and :other_location is null) or other_location = :other_location) and ((installed_name is null and :installed_name is null) or installed_name = :installed_name);" [
		":file" := im ^? inspectedKey . moduleFile . path,
		":package_name" := im ^? inspectedKey . modulePackage . _Just . packageName,
		":package_version" := im ^? inspectedKey . modulePackage . _Just . packageVersion,
		":other_location" := im ^? inspectedKey . otherLocationName,
		":installed_name" := im ^? inspectedKey . installedModuleName]
	execute "delete from imports where module_id == ?;" (Only mid)
	execute "delete from exports where module_id == ?;" (Only mid)
	execute "delete from scopes where module_id == ?;" (Only mid)
	execute "delete from names where module_id == ?;" (Only mid)
	insertModuleImports mid
	scope "exports" $ forM_ (im ^.. inspected . moduleExports . each) (insertExportSymbol mid)
	scope "scopes" $ forM_ (im ^.. inspected . scopeSymbols) (uncurry $ insertScopeSymbol mid)
	scope "names" $ do
		forM_ (im ^.. inspected . moduleSource . _Just . P.qnames) (insertResolvedQName mid)
		forM_ (im ^.. inspected . moduleSource . _Just . P.names) (insertResolvedName mid)
	where
		insertModuleImports :: SessionMonad m => Int -> m ()
		insertModuleImports mid = scope "insert-module-imports" $ do
			case im ^? inspected . moduleSource . _Just of
				Nothing -> return ()
				Just psrc -> do
					let
						imps = childrenBi psrc :: [ImportDecl Ann]
					forM_ imps $ \(ImportDecl _ mname qual _ _ _ alias specList) -> do
						execute "insert into imports (module_id, module_name, qualified, alias, hiding, import_list) values (?, ?, ?, ?, ?, ?);" (
							mid,
							getModuleName mname,
							qual,
							fmap getModuleName alias,
							maybe False getHiding specList,
							fmap makeImportList specList)
			where
				getModuleName (ModuleName _ s) = s
				getHiding (ImportSpecList _ h _) = h

				makeImportList (ImportSpecList _ _ specs) = encode $ map asJson specs
				asJson (IVar _ nm) = object ["name" .= fromName_ (void nm), "what" .= str' "var"]
				asJson (IAbs _ ns nm) = object ["name" .= fromName_ (void nm), "what" .= str' "abs", "ns" .= fromNamespace ns] where
					fromNamespace :: Namespace l -> Maybe String
					fromNamespace (NoNamespace _) = Nothing
					fromNamespace (TypeNamespace _) = Just "type"
					fromNamespace (PatternNamespace _) = Just "pat"
				asJson (IThingAll _ nm) = object ["name" .= fromName_ (void nm), "what" .= str' "all"]
				asJson (IThingWith _ nm cs) = object ["name" .= fromName_ (void nm), "what" .= str' "with", "list" .= map (fromName_ . void . toName') cs] where
					toName' (VarName _ n') = n'
					toName' (ConName _ n') = n'

				str' :: String -> String
				str' = id

		insertExportSymbol :: SessionMonad m => Int -> Symbol -> m ()
		insertExportSymbol mid sym = do
			defMid <- insertLookupModule (sym ^. symbolId . symbolModule)
			sid <- insertLookupSymbol defMid sym
			execute "insert into exports (module_id, symbol_id) values (?, ?);" (mid, sid)

		insertScopeSymbol :: SessionMonad m => Int -> Symbol -> [Name] -> m ()
		insertScopeSymbol mid sym scopeNames = do
			defMid <- insertLookupModule (sym ^. symbolId . symbolModule)
			sid <- insertLookupSymbol defMid sym
			forM_ scopeNames $ \name -> execute "insert into scopes (module_id, qualifier, name, symbol_id) values (?, ?, ?, ?);" (
				mid,
				Name.nameModule name,
				Name.nameIdent name,
				sid)

		insertResolvedQName mid qname = do
			execute "insert into names (module_id, qualifier, name, line, column, line_to, column_to, def_line, def_column, resolved_module, resolved_name, resolve_error) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);" $ (
				mid,
				Name.nameModule $ void qname,
				Name.nameIdent $ void qname,
				qname ^. P.pos . positionLine,
				qname ^. P.pos . positionColumn,
				qname ^. P.regionL . regionTo . positionLine,
				qname ^. P.regionL . regionTo . positionColumn)
				:. (
				qname ^? P.defPos . positionLine,
				qname ^? P.defPos . positionColumn,
				(qname ^? P.resolvedName) >>= Name.nameModule,
				Name.nameIdent <$> (qname ^? P.resolvedName),
				P.resolveError qname)

		insertResolvedName mid name = do
			hasName' <- hasResolved mid name
			when (not hasName') $ do
				execute "insert into names (module_id, qualifier, name, line, column, line_to, column_to, def_line, def_column, resolved_module, resolved_name, resolve_error) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);" $ (
					mid,
					Nothing :: Maybe Text,
					Name.fromName_ $ void name,
					name ^. P.pos . positionLine,
					name ^. P.pos . positionColumn,
					name ^. P.regionL . regionTo . positionLine,
					name ^. P.regionL . regionTo . positionColumn)
					:. (
					name ^? P.defPos . positionLine,
					name ^? P.defPos . positionColumn,
					(name ^? P.resolvedName) >>= Name.nameModule,
					Name.nameIdent <$> (name ^? P.resolvedName),
					P.resolveError name)

		hasResolved mid n = do
			[Only hasName] <- query "select count(*) > 0 from names where module_id == ? and line == ? and column == ? and line_to == ? and column_to == ?;" (
				mid,
				n ^. regionL . regionFrom . positionLine,
				n ^. regionL . regionFrom . positionColumn,
				n ^. regionL . regionTo . positionLine,
				n ^. regionL . regionTo . positionColumn)
			return hasName

lookupModuleLocation :: SessionMonad m => ModuleLocation -> m (Maybe Int)
lookupModuleLocation m = do
	mids <- queryNamed "select id from modules where ((file is null and :file is null) or file = :file) and ((package_name is null and :package_name is null) or package_name = :package_name) and ((package_version is null and :package_version is null) or package_version = :package_version) and ((installed_name is null and :installed_name is null) or installed_name = :installed_name) and ((other_location is null and :other_location is null) or other_location = :other_location);" [
		":file" := m ^? moduleFile . path,
		":package_name" := m ^? modulePackage . _Just . packageName,
		":package_version" := m ^? modulePackage . _Just . packageVersion,
		":installed_name" := m ^? installedModuleName,
		":other_location" := m ^? otherLocationName]
	when (length mids > 1) $ sendLog Warning  $ "different modules with location: {}" ~~ Display.display m
	return $ listToMaybe [mid | Only mid <- mids]

lookupModule :: SessionMonad m => ModuleId -> m (Maybe Int)
lookupModule m = do
	mids <- queryNamed "select id from modules where ((name is null and :name is null) or name = :name) and ((file is null and :file is null) or file = :file) and ((package_name is null and :package_name is null) or package_name = :package_name) and ((package_version is null and :package_version is null) or package_version = :package_version) and ((installed_name is null and :installed_name is null) or installed_name = :installed_name) and ((other_location is null and :other_location is null) or other_location = :other_location);" [
		":name" := m ^. moduleName,
		":file" := m ^? moduleLocation . moduleFile . path,
		":package_name" := m ^? moduleLocation . modulePackage . _Just . packageName,
		":package_version" := m ^? moduleLocation . modulePackage . _Just . packageVersion,
		":installed_name" := m ^? moduleLocation . installedModuleName,
		":other_location" := m ^? moduleLocation . otherLocationName]
	when (length mids > 1) $ sendLog Warning  $ "different modules with same name and location: {}" ~~ (m ^. moduleName)
	return $ listToMaybe [mid | Only mid <- mids]

insertLookupModule :: SessionMonad m => ModuleId -> m Int
insertLookupModule m = do
	modId <- lookupModule m
	case modId of
		Just mid -> return mid
		Nothing -> do
			execute "insert into modules (file, cabal, install_dirs, package_name, package_version, installed_name, other_location, name) values (?, ?, ?, ?, ?, ?, ?, ?);" (
				m ^? moduleLocation . moduleFile . path,
				m ^? moduleLocation . moduleProject . _Just . projectCabal,
				fmap (encode . map (view path)) (m ^? moduleLocation . moduleInstallDirs),
				m ^? moduleLocation . modulePackage . _Just . packageName,
				m ^? moduleLocation . modulePackage . _Just . packageVersion,
				m ^? moduleLocation . installedModuleName,
				m ^? moduleLocation . otherLocationName,
				m ^. moduleName)
			lastRow

lookupSymbol :: SessionMonad m => Int -> Symbol -> m (Maybe Int)
lookupSymbol mid sym = do
	sids <- queryNamed "select id from symbols where ((name is null and :name is null) or name = :name) and module_id = :module_id;" [
		":name" := sym ^. symbolId . symbolName,
		":module_id" := mid]
	when (length sids > 1) $ sendLog Warning $ "different symbols with same module id: {}.{}" ~~ show mid ~~ (sym ^. symbolId . symbolName)
	return $ listToMaybe [sid | Only sid <- sids]

insertLookupSymbol :: SessionMonad m => Int -> Symbol -> m Int
insertLookupSymbol mid sym = do
	msid <- lookupSymbol mid sym
	case msid of
		Just sid -> return sid
		Nothing -> do
			execute "insert into symbols (name, module_id, docs, line, column, what, type, parent, constructors, args, context, associate, pat_type, pat_constructor) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);" $ (
				sym ^. symbolId . symbolName,
				mid,
				sym ^. symbolDocs,
				sym ^? symbolPosition . _Just . positionLine,
				sym ^? symbolPosition . _Just . positionColumn)
				:. (
				symbolType sym,
				sym ^? symbolInfo . functionType . _Just,
				msum [sym ^? symbolInfo . parentClass, sym ^? symbolInfo . parentType],
				encode $ sym ^? symbolInfo . selectorConstructors,
				encode $ sym ^? symbolInfo . typeArgs,
				encode $ sym ^? symbolInfo . typeContext,
				sym ^? symbolInfo . familyAssociate . _Just,
				sym ^? symbolInfo . patternType . _Just,
				sym ^? symbolInfo . patternConstructor)
			lastRow

lastRow :: SessionMonad m => m Int
lastRow = do
	[Only i] <- query_ "select last_insert_rowid();"
	return i

loadModule :: SessionMonad m => Int -> m Module
loadModule mid = scope "load-module" $ do
	ms <- query @_ @(ModuleId :. (Maybe Text, Maybe Value, Int)) (toQuery (qModuleId `mappend` select_ ["mu.docs", "mu.fixities", "mu.id"] [] [fromString $ "mu.id == ?"])) (Only mid)
	case ms of
		[] -> sqlFailure $ "module with id {} not found" ~~ mid
		mods@((mid' :. (mdocs, mfixities, _)):_) -> do
			when (length mods > 1) $ sendLog Warning $ "multiple modules with same id = {} found" ~~ mid
			syms <- query @_ @Symbol (toQuery (qSymbol `mappend` select_ [] ["exports as e"] ["e.module_id == ?", "e.symbol_id == s.id"])) (Only mid)
			return $ Module {
				_moduleId = mid',
				_moduleDocs = mdocs,
				_moduleExports = syms,
				_moduleFixities = fromMaybe [] (mfixities >>= fromJSON'),
				_moduleScope = mempty,
				_moduleSource = Nothing }

loadModules :: (SessionMonad m, ToRow q) => String -> q -> m [Module]
loadModules selectExpr args = scope "load-modules" $ do
	ms <- query @_ @(ModuleId :. (Maybe Text, Maybe Value, Int)) (toQuery (qModuleId `mappend` select_ ["mu.docs", "mu.fixities", "mu.id"] [] [fromString $ "mu.id in (" ++ selectExpr ++ ")"])) args
	forM ms $ \(mid' :. (mdocs, mfixities, mid)) -> do
		syms <- query @_ @Symbol (toQuery (qSymbol `mappend` select_ [] ["exports as e"] ["e.module_id == ?", "e.symbol_id == s.id"])) (Only mid)
		return $ Module {
			_moduleId = mid',
			_moduleDocs = mdocs,
			_moduleExports = syms,
			_moduleFixities = fromMaybe [] (mfixities >>= fromJSON'),
			_moduleScope = mempty,
			_moduleSource = Nothing }

loadProject :: SessionMonad m => Path -> m Project
loadProject cabal = scope "load-project" $ do
	projs <- query @_ @(Only Int :. Project) "select id, name, cabal, version from projects where cabal == ?;" (Only $ view path cabal)
	(Only pid :. proj) <- case projs of
		[] -> sqlFailure $ "project with cabal {} not found" ~~ view path cabal
		_ -> do
			when (length projs > 1) $ sendLog Warning $ "multiple projects with same cabal = {} found" ~~ view path cabal
			return $ head projs

	libs <- query (toQuery $ select_ ["lib.modules"] ["libraries as lib"] [] `mappend` qBuildInfo `mappend` where_ [
		"lib.build_info_id == bi.id",
		"lib.project_id == ?"])
			(Only pid)

	exes <- query (toQuery $ select_ ["exe.name", "exe.path"] ["executables as exe"] [] `mappend` qBuildInfo `mappend` where_ [
		"exe.build_info_id == bi.id",
		"exe.project_id == ?"])
			(Only pid)

	tests <- query (toQuery $ select_ ["tst.name", "tst.enabled", "tst.main"] ["tests as tst"] [] `mappend` qBuildInfo `mappend` where_ [
		"tst.build_info_id == bi.id",
		"tst.project_id == ?"])
			(Only pid)

	return $
		set (projectDescription . _Just . projectLibrary) (listToMaybe libs) .
		set (projectDescription . _Just . projectExecutables) exes .
		set (projectDescription . _Just . projectTests) tests $
		proj

-- Util

sqlFailure :: SessionMonad m => Text -> m a
sqlFailure msg = do
	sendLog Error msg
	hsdevError $ SQLiteError $ T.unpack msg
