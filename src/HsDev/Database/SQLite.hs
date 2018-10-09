{-# LANGUAGE OverloadedStrings, TypeOperators, TypeApplications #-}

module HsDev.Database.SQLite (
	initialize, purge,
	privateMemory, sharedMemory,
	query, query_, queryNamed, execute, execute_, executeMany, executeNamed,
	withTemporaryTable,
	updatePackageDb, removePackageDb, insertPackageDb,
	updateProject, removeProject, insertProject, insertBuildInfo,
	removeModuleContents, removeModule,
	lookupModuleLocation, lookupModule,
	lookupSymbol,
	lastRow,

	loadModule, loadModules,
	loadProject,

	updateModules, upsertModules,

	-- * Utils
	lookupId,
	escapeLike,

	-- * Reexports
	module Database.SQLite.Simple,
	module HsDev.Database.SQLite.Select,
	module HsDev.Database.SQLite.Instances,
	module HsDev.Database.SQLite.Transaction
	) where

import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson hiding (Error)
import Data.List (intercalate)
import Data.Maybe
import Data.String
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple hiding (query, query_, queryNamed, execute, execute_, executeNamed, executeMany, withTransaction)
import qualified Database.SQLite.Simple as SQL (query, query_, queryNamed, execute, execute_, executeNamed, executeMany, withTransaction)
import Distribution.Text (display)
import Language.Haskell.Extension ()
import System.Directory
import System.Log.Simple
import Text.Format

import System.Directory.Paths

import HsDev.Database.SQLite.Instances
import HsDev.Database.SQLite.Schema
import HsDev.Database.SQLite.Select
import HsDev.Database.SQLite.Transaction
import qualified HsDev.Display as Display
import HsDev.Error
import HsDev.PackageDb.Types
import HsDev.Project.Types
import HsDev.Symbols (hasTag)
import HsDev.Symbols.Types hiding (loadProject)
import HsDev.Server.Types
import HsDev.Util

-- | Open new connection and set some pragmas
new :: String -> IO Connection
new p = do
	conn <- open p
	SQL.execute_ conn "pragma case_sensitive_like = true;"
	SQL.execute_ conn "pragma synchronous = off;"
	SQL.execute_ conn "pragma journal_mode = memory;"
	return conn

-- | Initialize database
initialize :: String -> IO Connection
initialize p = do
	conn <- new p
	[Only hasTables] <- SQL.query_ conn "select count(*) > 0 from sqlite_master where type == 'table';"
	goodVersion <- if hasTables
		then do
			[Only equalVersion] <- SQL.query conn "select sum(json(value) == json(?)) > 0 from hsdev where option == 'version';" (Only $ toJSON version)
			return equalVersion
		else return True
	let
		start
			| not goodVersion = do
					close conn
					removeFile p
					conn' <- new p
					initDb conn'
			| not hasTables = initDb conn
			| otherwise = return conn
		initDb conn' = SQL.withTransaction conn' $ do
			mapM_ (SQL.execute_ conn') commands
			SQL.execute @(Text, Value) conn' "insert into hsdev values (?, ?);" ("version", toJSON version)
			return conn'
	start

purge :: SessionMonad m => m ()
purge = do
	tables <- query_ @(Only String) "select name from sqlite_master where type == 'table';"
	forM_ tables $ \(Only table) ->
		execute_ $ fromString $ "delete from {};" ~~ table

-- | Private memory for db
privateMemory :: String
privateMemory = ":memory:"

-- | Shared db in memory
sharedMemory :: String
sharedMemory = "file::memory:?cache=shared"

-- | Retries for simple queries
retried :: (MonadIO m, MonadCatch m) => m a -> m a
retried = retry def

query :: (ToRow q, FromRow r, SessionMonad m) => Query -> q -> m [r]
query q' params = retried $ do
	conn <- serverSqlDatabase
	liftIO $ SQL.query conn q' params

query_ :: (FromRow r, SessionMonad m) => Query -> m [r]
query_ q' = retried $ do
	conn <- serverSqlDatabase
	liftIO $ SQL.query_ conn q'

queryNamed :: (FromRow r, SessionMonad m) => Query -> [NamedParam] -> m [r]
queryNamed q' ps' = retried $ do
	conn <- serverSqlDatabase
	liftIO $ SQL.queryNamed conn q' ps'

execute :: (ToRow q, SessionMonad m) => Query -> q -> m ()
execute q' params = retried $ do
	conn <- serverSqlDatabase
	liftIO $ SQL.execute conn q' params

execute_ :: SessionMonad m => Query -> m ()
execute_ q' = retried $ do
	conn <- serverSqlDatabase
	liftIO $ SQL.execute_ conn q'

executeMany :: (ToRow q, SessionMonad m) => Query -> [q] -> m ()
executeMany q' params = do
	conn <- serverSqlDatabase
	liftIO $ SQL.executeMany conn q' params

executeNamed :: SessionMonad m => Query -> [NamedParam] -> m ()
executeNamed q' ps' = do
	conn <- serverSqlDatabase
	liftIO $ SQL.executeNamed conn q' ps'

withTemporaryTable :: SessionMonad m => String -> [String] -> m a -> m a
withTemporaryTable tableName columns = bracket_ createTable dropTable where
	createTable = execute_ $ fromString $ "create temporary table {} ({});" ~~ tableName ~~ (intercalate ", " columns)
	dropTable = execute_ $ fromString $ "drop table {};" ~~ tableName

updatePackageDb :: SessionMonad m => PackageDb -> [ModulePackage] -> m ()
updatePackageDb pdb pkgs = scope "update-package-db" $ transaction_ Immediate $ do
	sendLog Trace $ "update package-db: {}" ~~ Display.display pdb
	removePackageDb pdb
	insertPackageDb pdb pkgs

removePackageDb :: SessionMonad m => PackageDb -> m ()
removePackageDb pdb = scope "remove-package-db" $
	execute "delete from package_dbs where package_db == ?;" (Only pdb)

insertPackageDb :: SessionMonad m => PackageDb -> [ModulePackage] -> m ()
insertPackageDb pdb pkgs = scope "insert-package-db" $ forM_ pkgs $ \pkg ->
	execute
		"insert into package_dbs (package_db, package_name, package_version) values (?, ?, ?);"
		(pdb, pkg ^. packageName, pkg ^. packageVersion)

updateProject :: SessionMonad m => Project -> m ()
updateProject proj = scope "update-project" $ transaction_ Immediate $ do
	sendLog Trace $ "update project: {}" ~~ Display.display proj
	removeProject proj
	insertProject proj

removeProject :: SessionMonad m => Project -> m ()
removeProject proj = scope "remove-project" $ do
	projId <- query @_ @(Only Int) "select id from projects where cabal == ?;" (Only $ proj ^. projectCabal)
	case projId of
		[] -> return ()
		pids -> do
			when (length pids > 1) $
				sendLog Warning $ "multiple projects for cabal {} found" ~~ (proj ^. projectCabal)
			forM_ pids $ \pid -> do
				bids <- query @_ @(Only Int) "select build_info_id from targets where project_id == ?;" pid
				execute "delete from projects where id == ?;" pid
				execute "delete from libraries where project_id == ?;" pid
				execute "delete from executables where project_id == ?;" pid
				execute "delete from tests where project_id == ?;" pid
				forM_ bids $ \bid -> execute "delete from build_infos where id == ?;" bid

insertProject :: SessionMonad m => Project -> m ()
insertProject proj = scope "insert-project" $ do
	execute "insert into projects (name, cabal, version, build_tool, package_db_stack) values (?, ?, ?, ?, ?);" proj
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

removeModuleContents :: SessionMonad m => Int -> m ()
removeModuleContents mid = scope "remove-module-contents" $ do
	execute "delete from imports where module_id = ?;" (Only mid)
	execute "delete from exports where module_id = ?;" (Only mid)
	execute "delete from scopes where module_id = ?;" (Only mid)
	execute "delete from names where module_id = ?;" (Only mid)
	execute "delete from types where module_id = ?;" (Only mid)
	execute "delete from symbols where module_id = ?;" (Only mid)

removeModule :: SessionMonad m => Int -> m ()
removeModule mid = scope "remove-module" $ do
	removeModuleContents mid
	execute "delete from modules where id == ?;" (Only mid)

upsertModules :: SessionMonad m => [InspectedModule] -> m [Int]
upsertModules ims = scope "upsert-modules" $ bracket_ initTemp removeTemp $ do
	executeMany "insert into upserted_modules (file, cabal, install_dirs, package_name, package_version, installed_name, exposed, other_location, name, docs, fixities, tags, inspection_error, inspection_time, inspection_opts) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);" $ map moduleData ims
	execute_ "update upserted_modules set id = (select m.id from modules as m where (m.file = upserted_modules.file) or ((m.package_name = upserted_modules.package_name) and (m.package_version = upserted_modules.package_version) and (m.installed_name = upserted_modules.installed_name)) or (m.other_location = upserted_modules.other_location));"
	execute_ "insert or replace into modules (id, file, cabal, install_dirs, package_name, package_version, installed_name, exposed, other_location, name, docs, fixities, tags, inspection_error, inspection_time, inspection_opts) select id, file, cabal, install_dirs, package_name, package_version, installed_name, exposed, other_location, name, docs, fixities, tags, inspection_error, inspection_time, inspection_opts from upserted_modules where id is not null;"
	execute_ "insert into modules (file, cabal, install_dirs, package_name, package_version, installed_name, exposed, other_location, name, docs, fixities, tags, inspection_error, inspection_time, inspection_opts) select file, cabal, install_dirs, package_name, package_version, installed_name, exposed, other_location, name, docs, fixities, tags, inspection_error, inspection_time, inspection_opts from upserted_modules where id is null;"
	execute_ "update upserted_modules set id = (select m.id from modules as m where (m.file = upserted_modules.file) or ((m.package_name = upserted_modules.package_name) and (m.package_version = upserted_modules.package_version) and (m.installed_name = upserted_modules.installed_name)) or (m.other_location = upserted_modules.other_location)) where id is null;"

	liftM (map fromOnly) $ query_ "select id from upserted_modules order by rowid;"
	where
		initTemp :: SessionMonad m => m ()
		initTemp = do
			execute_ "create temporary table upserted_modules as select * from modules where 0;"
			execute_ "create index upserted_modules_id_index on upserted_modules (id);"

		removeTemp :: SessionMonad m => m ()
		removeTemp = execute_ "drop table if exists upserted_modules;"

		moduleData im = (
			im ^? inspectedKey . moduleFile . path,
			im ^? inspectedKey . moduleProject . _Just . projectCabal,
			fmap (encode . map (view path)) (im ^? inspectedKey . moduleInstallDirs),
			im ^? inspectedKey . modulePackage . packageName,
			im ^? inspectedKey . modulePackage . packageVersion,
			im ^? inspectedKey . installedModuleName,
			im ^? inspectedKey . installedModuleExposed,
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

lookupModuleLocation :: SessionMonad m => ModuleLocation -> m (Maybe Int)
lookupModuleLocation m = do
	mids <- queryNamed "select id from modules where (file = :file) or (package_name = :package_name and package_version = :package_version and installed_name = :installed_name) or (other_location = :other_location);" [
		":file" := m ^? moduleFile . path,
		":package_name" := m ^? modulePackage . packageName,
		":package_version" := m ^? modulePackage . packageVersion,
		":installed_name" := m ^? installedModuleName,
		":other_location" := m ^? otherLocationName]
	when (length mids > 1) $ sendLog Warning  $ "different modules with location: {}" ~~ Display.display m
	return $ listToMaybe [mid | Only mid <- mids]

lookupModule :: SessionMonad m => ModuleId -> m (Maybe Int)
lookupModule m = do
	mids <- queryNamed "select id from modules where ((name is null and :name is null) or name = :name) and ((file = :file) or (package_name = :package_name and package_version = :package_version and installed_name = :installed_name) or (other_location = :other_location));" [
		":name" := m ^. moduleName,
		":file" := m ^? moduleLocation . moduleFile . path,
		":package_name" := m ^? moduleLocation . modulePackage . packageName,
		":package_version" := m ^? moduleLocation . modulePackage . packageVersion,
		":installed_name" := m ^? moduleLocation . installedModuleName,
		":other_location" := m ^? moduleLocation . otherLocationName]
	when (length mids > 1) $ sendLog Warning  $ "different modules with same name and location: {}" ~~ (m ^. moduleName)
	return $ listToMaybe [mid | Only mid <- mids]

lookupSymbol :: SessionMonad m => Int -> SymbolId -> m (Maybe Int)
lookupSymbol mid sym = do
	sids <- query "select id from symbols where name == ? and module_id == ?;" (
		sym ^. symbolName,
		mid)
	when (length sids > 1) $ sendLog Warning $ "different symbols with same module id: {}.{}" ~~ show mid ~~ (sym ^. symbolName)
	return $ listToMaybe [sid | Only sid <- sids]

lastRow :: SessionMonad m => m Int
lastRow = do
	[Only i] <- query_ "select last_insert_rowid();"
	return i

loadModule :: SessionMonad m => Int -> m Module
loadModule mid = scope "load-module" $ do
	ms <- query @_ @(ModuleId :. (Maybe Text, Maybe Value, Int))
		(toQuery $ mconcat [
			qModuleId,
			select_ ["mu.docs", "mu.fixities", "mu.id"],
			where_ [fromString "mu.id == ?"]])
		(Only mid)
	case ms of
		[] -> sqlFailure $ "module with id {} not found" ~~ mid
		mods@((mid' :. (mdocs, mfixities, _)):_) -> do
			when (length mods > 1) $ sendLog Warning $ "multiple modules with same id = {} found" ~~ mid
			syms <- query @_ @Symbol
				(toQuery $ mconcat [
					qSymbol,
					from_ ["exports as e"],
					where_ ["e.module_id == ?", "e.symbol_id == s.id"]])
				(Only mid)
			imps <- query @_ @Import "select line, column, module_name, qualified, alias from imports where module_id == ?;" (Only mid)
			return Module {
				_moduleId = mid',
				_moduleDocs = mdocs,
				_moduleImports = imps,
				_moduleExports = syms,
				_moduleFixities = fromMaybe [] (mfixities >>= fromJSON'),
				_moduleScope = mempty,
				_moduleSource = Nothing }

loadModules :: (SessionMonad m, ToRow q) => String -> q -> m [Module]
loadModules selectExpr args = scope "load-modules" $ do
	ms <- query @_ @(ModuleId :. (Maybe Text, Maybe Value, Int))
		(toQuery $ mconcat [
			qModuleId,
			select_ ["mu.docs", "mu.fixities", "mu.id"],
			where_ [fromString $ "mu.id in (" ++ selectExpr ++ ")"]])
		args
	forM ms $ \(mid' :. (mdocs, mfixities, mid)) -> do
		syms <- query @_ @Symbol
			(toQuery $ mconcat [
				qSymbol,
				from_ ["exports as e"],
				where_ ["e.module_id == ?", "e.symbol_id == s.id"]])
			(Only mid)
		imps <- query @_ @Import "select line, column, module_name, qualified, alias from imports where module_id == ?;" (Only mid)
		return Module {
			_moduleId = mid',
			_moduleDocs = mdocs,
			_moduleImports = imps,
			_moduleExports = syms,
			_moduleFixities = fromMaybe [] (mfixities >>= fromJSON'),
			_moduleScope = mempty,
			_moduleSource = Nothing }

loadProject :: SessionMonad m => Path -> m Project
loadProject cabal = scope "load-project" $ do
	projs <- query @_ @(Only Int :. Project) "select id, name, cabal, version, build_tool, package_db_stack from projects where cabal == ?;" (Only $ view path cabal)
	(Only pid :. proj) <- case projs of
		[] -> sqlFailure $ "project with cabal {} not found" ~~ view path cabal
		_ -> do
			when (length projs > 1) $ sendLog Warning $ "multiple projects with same cabal = {} found" ~~ view path cabal
			return $ head projs

	libs <- query
		(toQuery $ mconcat [
			select_ ["lib.modules"],
			from_ ["libraries as lib"],
			qBuildInfo,
			where_ [
				"lib.build_info_id == bi.id",
				"lib.project_id == ?"]])
			(Only pid)

	exes <- query
		(toQuery $ mconcat [
			select_ ["exe.name", "exe.path"],
			from_ ["executables as exe"],
			qBuildInfo,
			where_ [
				"exe.build_info_id == bi.id",
				"exe.project_id == ?"]])
			(Only pid)

	tests <- query
		(toQuery $ mconcat [
			select_ ["tst.name", "tst.enabled", "tst.main"],
			from_ ["tests as tst"],
			qBuildInfo,
			where_ [
				"tst.build_info_id == bi.id",
				"tst.project_id == ?"]])
			(Only pid)

	return $
		set (projectDescription . _Just . projectLibrary) (listToMaybe libs) .
		set (projectDescription . _Just . projectExecutables) exes .
		set (projectDescription . _Just . projectTests) tests $
		proj

-- | Update a bunch of modules
updateModules :: SessionMonad m => [InspectedModule] -> m ()
updateModules ims = scope "update-modules" $ do
	ids <- upsertModules ims
	updateModulesSymbols $ zip ids ims

-- | Update symbols of bunch of modules
updateModulesSymbols :: SessionMonad m => [(Int, InspectedModule)] -> m ()
updateModulesSymbols ims = scope "update-modules" $ timer "updated modules" $ bracket_ initTemps dropTemps $ do
	initUpdatedIds ims

	removeModulesContents
	transaction_ Immediate $ insertModulesDefs ims
	transaction_ Immediate $ insertModulesExports ims
	where
		initTemps :: SessionMonad m => m ()
		initTemps = execute_ "create temporary table updated_ids (id integer not null, cabal text, module text not null, only_header int not null, dirty int not null);"

		dropTemps :: SessionMonad m => m ()
		dropTemps = execute_ "drop table if exists updated_ids;" 

		initUpdatedIds :: SessionMonad m => [(Int, InspectedModule)] -> m ()
		initUpdatedIds imods = transaction_ Immediate $ do
			execute_ "create unique index updated_ids_id_index on updated_ids (id);"
			execute_ "create index updated_ids_module_index on updated_ids (module);"
			executeMany "insert into updated_ids (id, cabal, module, only_header, dirty) values (?, ?, ?, ?, ?);" $ do
				(mid, im) <- imods
				return (
					mid,
					im ^? inspectedKey . moduleProject . _Just . projectCabal,
					im ^?! inspected . moduleId . moduleName,
					hasTag OnlyHeaderTag im,
					hasTag DirtyTag im)

		removeModulesContents :: SessionMonad m => m ()
		removeModulesContents = scope "remove-modules-contents" $ do
			transaction_ Immediate $ do
				execute_ "delete from symbols where module_id in (select id from updated_ids where not only_header or not dirty);"
				execute_ "update symbols set line = null, column = null where module_id in (select id from updated_ids where only_header and dirty);"
				execute_ "delete from imports where module_id in (select id from updated_ids);"
				execute_ "delete from exports where module_id in (select id from updated_ids);"

			transaction_ Immediate $ execute_ "delete from scopes where module_id in (select id from updated_ids);"
			transaction_ Immediate $ execute_ "delete from names where module_id in (select id from updated_ids);"
			transaction_ Immediate $ execute_ "delete from types where module_id in (select id from updated_ids);"

		insertModulesDefs :: SessionMonad m => [(Int, InspectedModule)] -> m ()
		insertModulesDefs imods = executeMany "insert into symbols (name, module_id, docs, line, column, what, type, parent, constructors, args, context, associate, pat_type, pat_constructor) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);" $ do
			(mid, im) <- imods
			m <- im ^.. inspected
			sym <- m ^. moduleExports
			guard (sym ^. symbolId . symbolModule == m ^. moduleId)
			return $ (
				sym ^. symbolId . symbolName,
				mid,
				sym ^. symbolDocs,
				sym ^? symbolPosition . _Just . positionLine,
				sym ^? symbolPosition . _Just . positionColumn)
				:. (sym ^. symbolInfo)

		insertModulesExports :: SessionMonad m => [(Int, InspectedModule)] -> m ()
		insertModulesExports imods = executeMany "insert into exports (module_id, symbol_id) select ?, s.id from modules as m, symbols as s where ((? = m.file) or (? = m.package_name and ? = m.package_version and ? = m.installed_name) or (? = m.other_location)) and s.module_id = m.id and ? = s.name and ? = s.what;" $ do
			(mid, im) <- imods
			m <- im ^.. inspected
			sym <- m ^. moduleExports
			return $
				(Only mid) :.
				mkLocationId (sym ^. symbolId . symbolModule) :.
				(sym ^. symbolId . symbolName, symbolType sym)

		mkLocationId m' = (
			m' ^? moduleLocation . moduleFile,
			m' ^? moduleLocation . modulePackage . packageName,
			m' ^? moduleLocation . modulePackage . packageVersion,
			m' ^? moduleLocation . installedModuleName,
			m' ^? moduleLocation . otherLocationName)


escapeLike :: Text -> Text
escapeLike = T.replace "%" "\\%" . T.replace "_" "\\_" . T.replace "\\" "\\\\"

-- Util

sqlFailure :: SessionMonad m => Text -> m a
sqlFailure msg = do
	sendLog Error msg
	hsdevError $ SQLiteError $ T.unpack msg

lookupId :: SessionMonad m => ModuleLocation -> m Int
lookupId = lookupModuleLocation >=> maybe err return where
	err = hsdevError $ SQLiteError "module not exist in db"
