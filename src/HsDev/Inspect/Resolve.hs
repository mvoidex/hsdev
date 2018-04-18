{-# LANGUAGE OverloadedStrings, TypeApplications, TypeOperators #-}

module HsDev.Inspect.Resolve (
	-- * Prepare
	loadEnv, saveEnv,
	loadEnvironment, loadFixities, withEnv,
	-- * Resolving
	resolveModule, resolvePreloaded, resolve,
	-- * Saving results
	updateResolveds
	) where

import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Data.Aeson
import Data.Generics.Uniplate.Operations
import Data.Functor
import Data.Function
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.String
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Haskell.Exts as H
import qualified Language.Haskell.Names as N
import qualified Language.Haskell.Names.Open as N
import qualified Language.Haskell.Names.Annotated as N
import qualified Language.Haskell.Names.Imports as N
import qualified Language.Haskell.Names.Exports as N
import qualified Language.Haskell.Names.ModuleSymbols as N
import qualified Language.Haskell.Names.SyntaxUtils as N
import System.Log.Simple
import Text.Format

import Data.LookupTable
import System.Directory.Paths
import HsDev.Database.SQLite as SQLite
import qualified HsDev.Display as Display
import HsDev.Error
import HsDev.Inspect.Definitions
import HsDev.Inspect.Types
import HsDev.Symbols
import qualified HsDev.Symbols.HaskellNames as HN
import HsDev.Symbols.Parsed as P
import HsDev.Server.Types
import HsDev.Util

-- | Try resolve module symbols
resolveModule :: MonadThrow m => Environment -> FixitiesTable -> Preloaded -> InspectM ModuleLocation ModuleTag m Resolved
resolveModule env fixities p = inspectTag ResolvedNamesTag $ inspectUntag OnlyHeaderTag $ case H.parseFileContentsWithMode (p' ^. preloadedMode) (T.unpack $ p ^. preloaded) of
	H.ParseFailed loc reason -> hsdevError $ InspectError $ "Parse failed at " ++ show loc ++ ": " ++ reason
	H.ParseOk m -> return (resolve env m)
	where
		qimps = M.keys $ N.importTable env (p ^. preloadedModule)
		p' = p { _preloadedMode = (_preloadedMode p) { H.fixities = Just (mapMaybe (`M.lookup` fixities) qimps) } }

-- | Resolve just preloaded part of module, this will give imports and scope
resolvePreloaded :: MonadThrow m => Environment -> Preloaded -> InspectM ModuleLocation ModuleTag m Resolved
resolvePreloaded env = inspectTag ResolvedNamesTag . return . resolve env . view preloadedModule

-- | Resolve parsed module
resolve :: Environment -> H.Module H.SrcSpanInfo -> Resolved
resolve env m = Resolved {
	_resolvedModule = void mn,
	_resolvedSource = annotated,
	_resolvedDefs = getSymbols decls',
	_resolvedImports = map (toImport . dropScope) idecls',
	_resolvedExports = N.exportedSymbols tbl m,
	_resolvedScope = tbl,
	_resolvedFixities = [H.Fixity (void assoc) (fromMaybe 0 pr) (fixName opName)
		| H.InfixDecl _ assoc pr ops <- decls', opName <- map getOpName ops] }
	where
		getOpName (H.VarOp _ nm) = nm
		getOpName (H.ConOp _ nm) = nm
		fixName o = H.Qual () (void mn) (void o)
		itbl = N.importTable env m
		tbl = N.moduleTable itbl m
		-- Not using 'annotate' because we already computed needed tables
		annotated = H.Module (noScope l) mhead' mpragmas' idecls' decls'
		H.Module l mhead mpragmas idecls decls = m
		mhead' = fmap scopeHead mhead
		mpragmas' = fmap withNoScope mpragmas
		scopeHead (H.ModuleHead lh mname mwarns mexports) = H.ModuleHead (noScope lh) (withNoScope mname) (fmap withNoScope mwarns) $
			fmap (N.annotateExportSpecList tbl) mexports
		idecls' = N.annotateImportDecls mn env idecls
		decls' = map (N.annotateDecl (N.initialScope (N.dropAnn mn) tbl)) decls
		mn = N.getModuleName m

-- | Load environment and fixities from cache or sql
loadEnv :: SessionMonad m => Maybe Path -> m (Environment, FixitiesTable)
loadEnv mcabal = do
	envTable <- askSession sessionResolveEnvironment
	cacheInTableM envTable mcabal $ (,) <$> loadEnvironment mcabal <*> loadFixities mcabal

-- | Save environment and fixities to cache
saveEnv :: SessionMonad m => Maybe Path -> Environment -> FixitiesTable -> m ()
saveEnv mcabal env fixities = do
	envTable <- askSession sessionResolveEnvironment
	void $ insertTable mcabal (env, fixities) envTable

-- | Load environment from sql
loadEnvironment :: SessionMonad m => Maybe Path -> m Environment
loadEnvironment mcabal = transaction_ Deferred $ do
	sendLog Trace $ "loading environment for {}" ~~ fromMaybe "<standalone>" mcabal
	env <- query @_ @(Only (H.ModuleName ()) :. N.Symbol)
		(toQuery $ mconcat [
			select_ ["em.name"],
			from_ ["projects_modules_scope as ps", "exports as e", "modules as em"],
			where_ [
				"ps.cabal is ?",
				"ps.module_id = em.id",
				"e.symbol_id = s.id",
				"e.module_id = em.id"],
			qNSymbol "m" "s"])
		(Only mcabal)
	return $ M.fromList $ do
		group' <- groupBy ((==) `on` fst) . sortBy (comparing fst) $ [(m, s) | (Only m :. s) <- env]
		let
			(gmod:_, gsyms) = unzip group'
		return (gmod, gsyms)

-- | Load fixities from sql
loadFixities :: SessionMonad m => Maybe Path -> m FixitiesTable
loadFixities mcabal = transaction_ Deferred $ do
	fixities' <- query @_ @(Only Value)
		(toQuery $ mconcat [
			select_ ["m.fixities"],
			from_ ["projects_modules_scope as ps", "modules as m"],
			where_ [
				"ps.cabal is ?",
				"ps.module_id = m.id",
				"m.fixities is not null"]])
		(Only mcabal)
	return $ mconcat [M.singleton n f |
		f@(H.Fixity _ _ n) <- concat (mapMaybe (fromJSON' . fromOnly) fixities')]

-- | Run with temporary table for environment
withEnv :: SessionMonad m => Maybe Path -> m a -> m a
withEnv mcabal = (initEnv >>) where
	initEnv = do
		execute_ "create temporary table if not exists resolve (cabal text);"
		execute_ "create temporary table if not exists env (module text not null, name text not null, what text not null, id integer not null);"
		execute_ "create unique index if not exists env_id_index on env (id);"
		execute_ "create unique index if not exists env_symbol_index on env (module, name, what);"

		curEnv <- query_ "select cabal from resolve;"
		unless (fmap fromOnly (listToMaybe curEnv) == Just mcabal) $ do
			execute_ "delete from resolve;"
			execute "insert into resolve values (?);" (Only mcabal)
			execute_ "delete from env;"
			executeNamed "insert into env select m.name, s.name, s.what, min(s.id) from modules as m, symbols as s where m.id = s.module_id and s.id in (select distinct e.symbol_id from exports as e where e.module_id in (select ps.module_id from projects_modules_scope as ps where ps.cabal is :cabal)) group by m.name, s.name, s.what;" [
				":cabal" := mcabal]

		[Only cnt] <- query_ @(Only Int) "select count(*) from env;"
		sendLog Trace $ "created env table with {} symbols" ~~ cnt

-- | Save results in sql, updated temporary env table
updateResolveds :: SessionMonad m => Maybe Path -> [InspectedResolved] -> m ()
updateResolveds mcabal ims = scope "update-resolveds" $ withEnv mcabal $ do
	ids <- upsertResolveds ims
	updateResolvedsSymbols (zip ids ims)

upsertResolveds :: SessionMonad m => [InspectedResolved] -> m [Int]
upsertResolveds ims = scope "upsert-resolveds" $ bracket_ initTemp removeTemp $ do
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
			msum [im ^? inspected . resolvedModule . moduleName_, im ^? inspectedKey . installedModuleName],
			Nothing @Text,
			fmap encode $ im ^? inspected . resolvedFixities,
			encode $ asDict $ im ^. inspectionTags,
			fmap show $ im ^? inspectionResult . _Left)
			:.
			fromMaybe InspectionNone (im ^? inspection)
		asDict tags = object [fromString (Display.display t) .= True | t <- S.toList tags]

updateResolvedsSymbols :: SessionMonad m => [(Int, InspectedResolved)] -> m ()
updateResolvedsSymbols ims = bracket_ initTemps dropTemps $ do
	initUpdatedIds ims

	removeModulesContents
	transaction_ Immediate $ insertModulesDefs ims
	transaction_ Immediate $ insertModulesImports ims
	transaction_ Immediate $ insertExportsSymbols ims
	transaction_ Immediate $ do
		insertScopesSymbols ims
		insertResolvedsNames ims
	commitTemps

	where
		initTemps :: SessionMonad m => m ()
		initTemps = do
			execute_ "create temporary table updated_ids (id integer not null, cabal text, module text not null, only_header int not null, dirty int not null);"
			execute_ "create temporary table updating_scopes as select * from scopes where 0;"
			execute_ "create index updating_scopes_name_index on updating_scopes (module_id, name);"
			execute_ "create temporary table updating_names as select * from names where 0;"
			execute_ "create unique index updating_names_position_index on updating_names (module_id, line, column, line_to, column_to);"

		dropTemps :: SessionMonad m => m ()
		dropTemps = do
			execute_ "drop table if exists updated_ids;"
			execute_ "drop table if exists updating_scopes;"
			execute_ "drop table if exists updating_names;"

		commitTemps :: SessionMonad m => m ()
		commitTemps = do
			transaction_ Immediate $ execute_ "insert into scopes select * from updating_scopes;"
			transaction_ Immediate $ execute_ "insert into names select * from updating_names;"

		initUpdatedIds :: SessionMonad m => [(Int, InspectedResolved)] -> m ()
		initUpdatedIds imods = transaction_ Immediate $ do
			execute_ "create unique index updated_ids_id_index on updated_ids (id);"
			execute_ "create index updated_ids_module_index on updated_ids (module);"
			executeMany "insert into updated_ids (id, cabal, module, only_header, dirty) values (?, ?, ?, ?, ?);" $ do
				(mid, im) <- imods
				return (
					mid,
					im ^? inspectedKey . moduleProject . _Just . projectCabal,
					im ^?! inspected . resolvedModule . moduleName_,
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

		insertModulesImports :: SessionMonad m => [(Int, InspectedResolved)] -> m ()
		insertModulesImports imods = scope "imports" $ do
			executeMany "insert into imports (module_id, line, column, module_name, qualified, alias, hiding, import_list) values (?, ?, ?, ?, ?, ?, ?, ?);" $ do
				(mid, im) <- imods
				let
					p = im ^?! inspected . resolvedSource
				idecl@(H.ImportDecl _ mname qual _ _ _ alias specList) <- childrenBi p :: [H.ImportDecl Ann]
				return (
					mid,
					idecl ^. pos . positionLine,
					idecl ^. pos . positionColumn,
					getModuleName mname,
					qual,
					fmap getModuleName alias,
					maybe False getHiding specList,
					fmap makeImportList specList)
			execute_ "update imports set import_module_id = (select im.id from updated_ids as u, modules as im, projects_modules_scope as ps where ((ps.cabal is null and u.cabal is null) or (ps.cabal == u.cabal)) and ps.module_id == im.id and im.name == imports.module_name) where module_id in (select u.id from updated_ids as u);"
			where
				getModuleName (H.ModuleName _ s) = s
				getHiding (H.ImportSpecList _ h _) = h

				makeImportList (H.ImportSpecList _ _ specs) = encode $ map asJson specs
				asJson (H.IVar _ nm) = object ["name" .= fromName_ (void nm), "what" .= id @String "var"]
				asJson (H.IAbs _ ns nm) = object ["name" .= fromName_ (void nm), "what" .= id @String "abs", "ns" .= fromNamespace ns] where
					fromNamespace :: H.Namespace l -> Maybe String
					fromNamespace (H.NoNamespace _) = Nothing
					fromNamespace (H.TypeNamespace _) = Just "type"
					fromNamespace (H.PatternNamespace _) = Just "pat"
				asJson (H.IThingAll _ nm) = object ["name" .= fromName_ (void nm), "what" .= id @String "all"]
				asJson (H.IThingWith _ nm cs) = object ["name" .= fromName_ (void nm), "what" .= id @String "with", "list" .= map (fromName_ . void . toName') cs] where
					toName' (H.VarName _ n') = n'
					toName' (H.ConName _ n') = n'

		insertModulesDefs :: SessionMonad m => [(Int, InspectedResolved)] -> m ()
		insertModulesDefs imods = scope "defs" $ do
			executeMany "insert into symbols (name, module_id, docs, line, column, what, type, parent, constructors, args, context, associate, pat_type, pat_constructor) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);" $ do
				(mid, im) <- imods
				sym <- im ^.. inspected . resolvedDefs . each
				return $ (
					sym ^. symbolId . symbolName,
					mid,
					sym ^. symbolDocs,
					sym ^? symbolPosition . _Just . positionLine,
					sym ^? symbolPosition . _Just . positionColumn)
					:.
					(sym ^. symbolInfo)
			execute_ "insert or replace into env (module, name, what, id) select m.name, s.name, s.what, s.id from modules as m, symbols as s where m.id in (select id from updated_ids) and s.module_id = m.id;"

		insertExportsSymbols :: SessionMonad m => [(Int, InspectedResolved)] -> m ()
		insertExportsSymbols imods = scope "exports" $ executeMany "insert into exports (module_id, symbol_id) select ?, env.id from env where env.module = ? and env.name = ? and env.what = ?;" $ do
			(mid, im) <- imods
			sym <- im ^.. inspected . resolvedExports . each
			return (
				mid,
				N.symbolModule sym,
				N.symbolName sym,
				symbolType (HN.fromSymbol sym))

		insertScopesSymbols :: SessionMonad m => [(Int, InspectedResolved)] -> m ()
		insertScopesSymbols imods = scope "scope" $ executeMany "insert into updating_scopes (module_id, qualifier, name, symbol_id) select ?, ?, ?, env.id from env where env.module = ? and env.name = ? and env.what = ?;" $ do
			(mid, im) <- imods
			(qn, syms) <- M.toList (im ^. inspected . resolvedScope)
			sym <- syms
			return (
				mid,
				nameModule qn,
				nameIdent qn,
				N.symbolModule sym,
				N.symbolName sym,
				symbolType (HN.fromSymbol sym))

		insertResolvedsNames :: SessionMonad m => [(Int, InspectedResolved)] -> m ()
		insertResolvedsNames imods = scope "names" $ do
			insertNames
			replaceQNames
			resolveGlobalBinders
			setResolvedsSymbolIds
			where
				insertNames = executeMany insertQuery namesData
				replaceQNames = executeMany insertQuery qnamesData
				resolveGlobalBinders = execute_ "update updating_names set (resolved_module, resolved_name, resolved_what) = (select u.module, s.name, s.what from updated_ids as u, symbols as s where u.id = s.module_id and s.module_id = updating_names.module_id and s.line = updating_names.line and s.column = updating_names.column) where (line, column) = (def_line, def_column) and resolved_module is null and resolved_name is null;"
				setResolvedsSymbolIds = execute_ "update updating_names set symbol_id = (select sc.symbol_id from updating_scopes as sc, symbols as s, modules as m where updating_names.module_id == sc.module_id and ((updating_names.qualifier is null and sc.qualifier is null) or (updating_names.qualifier == sc.qualifier)) and updating_names.name == sc.name and s.id == sc.symbol_id and m.id == s.module_id and s.name == updating_names.resolved_name and s.what == updating_names.resolved_what and m.name == updating_names.resolved_module) where resolved_module is not null and resolved_name is not null and resolved_what is not null;"
				insertQuery = "insert or replace into updating_names (module_id, qualifier, name, line, column, line_to, column_to, def_line, def_column, resolved_module, resolved_name, resolved_what, resolve_error) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);"

				namesData = map (uncurry toData) $ do
					(mid, im) <- imods
					n <- im ^.. inspected . resolvedSource . P.names
					return (mid, n)
				qnamesData = map (uncurry toQData) $ do
					(mid, im) <- imods
					n <- im ^.. inspected . resolvedSource . P.qnames
					return (mid, n)

				toData mid name = (
					mid,
					Nothing :: Maybe Text,
					fromName_ $ void name,
					name ^. P.pos . positionLine,
					name ^. P.pos . positionColumn,
					name ^. P.regionL . regionTo . positionLine,
					name ^. P.regionL . regionTo . positionColumn)
					:. (
					name ^? P.defPos . positionLine,
					name ^? P.defPos . positionColumn,
					(name ^? P.resolvedName) >>= nameModule,
					nameIdent <$> (name ^? P.resolvedName),
					fmap (symbolType . HN.fromSymbol) $ name ^? P.symbolL,
					P.resolveError name)
				toQData mid qname = (
					mid,
					nameModule $ void qname,
					nameIdent $ void qname,
					qname ^. P.pos . positionLine,
					qname ^. P.pos . positionColumn,
					qname ^. P.regionL . regionTo . positionLine,
					qname ^. P.regionL . regionTo . positionColumn)
					:. (
					qname ^? P.defPos . positionLine,
					qname ^? P.defPos . positionColumn,
					(qname ^? P.resolvedName) >>= nameModule,
					nameIdent <$> (qname ^? P.resolvedName),
					fmap (symbolType . HN.fromSymbol) $ qname ^? P.symbolL,
					P.resolveError qname)
