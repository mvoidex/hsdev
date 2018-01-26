{-# LANGUAGE OverloadedStrings, TypeApplications, TypeOperators #-}

module HsDev.Inspect.Resolve (
	-- * Prepare
	loadEnvironment, loadFixities, withEnv,
	-- * Resolving
	resolvePreloaded, resolve,
	-- * Saving results
	updateResolved, updateResolveds
	) where

import Control.Lens hiding ((.=))
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

import System.Directory.Paths
import HsDev.Database.SQLite as SQLite hiding (removeModuleContents)
import qualified HsDev.Display as Display
import HsDev.Error
import HsDev.Inspect.Definitions
import HsDev.Inspect.Types
import HsDev.Symbols.Name
import HsDev.Symbols.Types
import qualified HsDev.Symbols.HaskellNames as HN
import HsDev.Symbols.Parsed as P
import HsDev.Server.Types
import HsDev.Util

-- | Try resolve preloaded module
resolvePreloaded :: Environment -> FixitiesTable -> Preloaded -> Either String Resolved
resolvePreloaded env fixities p = case H.parseFileContentsWithMode (p' ^. preloadedMode) (T.unpack $ p ^. preloaded) of
	H.ParseFailed loc reason -> Left $ "Parse failed at " ++ show loc ++ ": " ++ reason
	H.ParseOk m -> Right $ resolve env m
	where
		qimps = M.keys $ N.importTable env (p ^. preloadedModule)
		p' = p { _preloadedMode = (_preloadedMode p) { H.fixities = Just (mapMaybe (`M.lookup` fixities) qimps) } }

-- | Resolve parsed module
resolve :: Environment -> H.Module H.SrcSpanInfo -> Resolved
resolve env m = Resolved {
	_resolvedModule = void mn,
	_resolvedSource = annotated,
	_resolvedDefs = getSymbols decls',
	_resolvedImports = map (fromModuleName_ . void . H.importModule) idecls',
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

-- | Load environment from sql
loadEnvironment :: SessionMonad m => Maybe Path -> m Environment
loadEnvironment mcabal = do
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
loadFixities mcabal = do
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
withEnv mcabal = withTemporaryTable "env" ["module text not null", "name text not null", "what text not null", "id integer not null"] . (initEnv >>) where
	initEnv = do
		execute_ "create unique index env_id_index on env (id);"
		execute_ "create unique index env_symbol_index on env (module, name, what);"
		executeNamed "insert into env select m.name, s.name, s.what, min(s.id) from modules as m, symbols as s where m.id = s.module_id and s.id in (select distinct e.symbol_id from exports as e where e.module_id in (select ps.module_id from projects_modules_scope as ps where ps.cabal is :cabal)) group by m.name, s.name, s.what;" [
			":cabal" := mcabal]
		[Only cnt] <- query_ @(Only Int) "select count(*) from env;"
		sendLog Trace $ "created env table with {} symbols" ~~ cnt

-- | Save results in sql, also updates temporary environment table
updateResolved :: SessionMonad m => InspectedResolved -> m ()
updateResolved im = scope "update-resolved" $ do
	_ <- upsertResolved im
	insertResolvedSymbols im

-- | Save results in sql, updated temporary env table
updateResolveds :: SessionMonad m => [InspectedResolved] -> m ()
updateResolveds ims = scope "update-resolveds" $ do
	mapM_ (upsertResolved >=> removeModuleContents) ims
	insertResolvedsSymbols ims

upsertResolved :: SessionMonad m => InspectedResolved -> m Int
upsertResolved im = do
	mmid <- lookupModuleLocation (im ^. inspectedKey)
	case mmid of
		Nothing -> do
			execute "insert into modules (file, cabal, install_dirs, package_name, package_version, installed_name, other_location, name, docs, fixities, tags, inspection_error, inspection_time, inspection_opts) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);"
				moduleData
			lastRow
		Just mid' -> do
			execute "update modules set file = ?, cabal = ?, install_dirs = ?, package_name = ?, package_version = ?, installed_name = ?, other_location = ?, name = ?, docs = ?, fixities = ?, tags = ?, inspection_error = ?, inspection_time = ?, inspection_opts = ? where id == ?;"
				(moduleData :. Only mid')
			return mid'
	where
		moduleData = (
			im ^? inspectedKey . moduleFile . path,
			im ^? inspectedKey . moduleProject . _Just . projectCabal,
			fmap (encode . map (view path)) (im ^? inspectedKey . moduleInstallDirs),
			im ^? inspectedKey . modulePackage . packageName,
			im ^? inspectedKey . modulePackage . packageVersion,
			im ^? inspectedKey . installedModuleName,
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

insertResolvedSymbols :: SessionMonad m => InspectedResolved -> m ()
insertResolvedSymbols im = do
	Just mid <- lookupModuleLocation (im ^. inspectedKey)
	removeModuleContents mid
	execute "delete from symbols where module_id = ?;" (Only mid)
	insertModuleImports mid (im ^?! inspected . resolvedSource)
	insertModuleDefs mid (im ^.. inspected . resolvedDefs . each)
	insertExportSymbols mid (im ^.. inspected . resolvedExports . each)
	insertScopeSymbols mid (M.toList (im ^?! inspected . resolvedScope))
	insertResolvedNames mid (im ^?! inspected . resolvedSource)
	updateEnvironment mid
	where
		insertModuleImports :: SessionMonad m => Int -> Parsed -> m ()
		insertModuleImports mid p = scope "imports" $ do
			let
				imps = childrenBi p :: [H.ImportDecl Ann]
				importRow idecl@(H.ImportDecl _ mname qual _ _ _ alias specList) = (
					mid,
					idecl ^. pos . positionLine,
					getModuleName mname,
					qual,
					fmap getModuleName alias,
					maybe False getHiding specList,
					fmap makeImportList specList)
			executeMany "insert into imports (module_id, line, module_name, qualified, alias, hiding, import_list) values (?, ?, ?, ?, ?, ?, ?);"
				(map importRow imps)
			executeNamed "update imports set import_module_id = (select im.id from modules as im, projects_modules_scope as ps where ((ps.cabal is null and :cabal is null) or (ps.cabal == :cabal)) and ps.module_id == im.id and im.name == imports.module_name) where module_id == :module_id;" [
			 ":cabal" := im ^? inspectedKey . moduleProject . _Just . projectCabal,
			 ":module_id" := mid]
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

		insertModuleDefs :: SessionMonad m => Int -> [Symbol] -> m ()
		insertModuleDefs mid syms = scope "defs" $ do
			executeMany "insert into symbols (name, module_id, docs, line, column, what, type, parent, constructors, args, context, associate, pat_type, pat_constructor) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);" $ do
				sym <- syms
				return $ (
					sym ^. symbolId . symbolName,
					mid,
					sym ^. symbolDocs,
					sym ^? symbolPosition . _Just . positionLine,
					sym ^? symbolPosition . _Just . positionColumn)
					:.
					(sym ^. symbolInfo)
			execute "insert or replace into env (module, name, what, id) select ?, s.name, s.what, s.id from symbols as s where s.module_id = ?;" (im ^?! inspected . resolvedModule . moduleName_, mid)

		insertExportSymbols :: SessionMonad m => Int -> [N.Symbol] -> m ()
		insertExportSymbols mid syms = scope "exports" $ do
			executeMany "insert into exports (module_id, symbol_id) select ?, env.id from env where env.module = ? and env.name = ? and env.what = ?;" $ do
				sym <- syms
				return (
					mid,
					N.symbolModule sym,
					N.symbolName sym,
					symbolType (HN.fromSymbol sym))

		insertScopeSymbols :: SessionMonad m => Int -> [(Name, [N.Symbol])] -> m ()
		insertScopeSymbols mid snames = scope "scope" $ do
			executeMany "insert into scopes (module_id, qualifier, name, symbol_id) select ?, ?, ?, env.id from env where env.module = ? and env.name = ? and env.what = ?;" $ do
				(qn, syms) <- snames
				sym <- syms
				return (
					mid,
					nameModule qn,
					nameIdent qn,
					N.symbolModule sym,
					N.symbolName sym,
					symbolType (HN.fromSymbol sym))

		insertResolvedNames :: SessionMonad m => Int -> Parsed -> m ()
		insertResolvedNames mid p = scope "resolved" $ do
			insertNames
			replaceQNames
			executeNamed "update names set resolved_module = :module, (resolved_name, resolved_what) = (select s.name, s.what from symbols as s where s.module_id = names.module_id and s.line = names.line and s.column = names.column) where module_id = :module_id and (line, column) = (def_line, def_column) and resolved_module is null and resolved_name is null;" [
				":module" := im ^?! inspected . resolvedModule . moduleName_,
				":module_id" := mid]
			setResolvedSymbolIds
			where
				insertNames = executeMany insertQuery namesData
				replaceQNames = executeMany insertQuery qnamesData
				setResolvedSymbolIds = execute "update names set symbol_id = (select sc.symbol_id from scopes as sc, symbols as s, modules as m where names.module_id == sc.module_id and ((names.qualifier is null and sc.qualifier is null) or (names.qualifier == sc.qualifier)) and names.name == sc.name and s.id == sc.symbol_id and m.id == s.module_id and s.name == names.resolved_name and s.what == names.resolved_what and m.name == names.resolved_module) where module_id == ? and resolved_module is not null and resolved_name is not null and resolved_what is not null;" (Only mid)
				insertQuery = "insert or replace into names (module_id, qualifier, name, line, column, line_to, column_to, def_line, def_column, resolved_module, resolved_name, resolved_what, resolve_error) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);"
				namesData = map toData $ p ^.. P.names
				qnamesData = map toQData $ p ^.. P.qnames
				toData name = (
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
				toQData qname = (
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

		updateEnvironment :: SessionMonad m => Int -> m ()
		updateEnvironment mid = do
			execute "insert or replace into env (module, name, what, id) select m.name, s.name, s.what, min(s.id) from modules as m, symbols as s, exports as e where m.id = s.module_id and s.id = e.symbol_id and e.module_id = ? group by m.name, s.name, s.what;" (Only mid)

insertResolvedsSymbols :: SessionMonad m => [InspectedResolved] -> m ()
insertResolvedsSymbols ims = withTemporaryTable "updated_ids" ["id integer not null", "cabal text", "module text not null"] $ do
	ids <- mapM SQLite.lookupId (ims ^.. each . inspectedKey)
	let
		imods = zip ids ims

	initUpdatedIds imods

	insertModulesImports imods
	insertModulesDefs imods
	insertExportsSymbols imods
	insertScopesSymbols imods
	insertResolvedsNames imods
	updateEnvironment

	where
		initUpdatedIds :: SessionMonad m => [(Int, InspectedResolved)] -> m ()
		initUpdatedIds imods = do
			execute_ "create unique index updated_ids_id_index on updated_ids (id);"
			execute_ "create index updated_ids_module_index on updated_ids (module);"
			executeMany "insert into updated_ids (id, cabal, module) values (?, ?, ?);" $ do
				(mid, im) <- imods
				return (
					mid,
					im ^? inspectedKey . moduleProject . _Just . projectCabal,
					im ^?! inspected . resolvedModule . moduleName_)

		insertModulesImports :: SessionMonad m => [(Int, InspectedResolved)] -> m ()
		insertModulesImports imods = scope "imports" $ do
			executeMany "insert into imports (module_id, line, module_name, qualified, alias, hiding, import_list) values (?, ?, ?, ?, ?, ?, ?);" $ do
				(mid, im) <- imods
				let
					p = im ^?! inspected . resolvedSource
				idecl@(H.ImportDecl _ mname qual _ _ _ alias specList) <- childrenBi p :: [H.ImportDecl Ann]
				return (
					mid,
					idecl ^. pos . positionLine,
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
		insertScopesSymbols imods = scope "scope" $ executeMany "insert into scopes (module_id, qualifier, name, symbol_id) select ?, ?, ?, env.id from env where env.module = ? and env.name = ? and env.what = ?;" $ do
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
				resolveGlobalBinders = execute_ "update names set (resolved_module, resolved_name, resolved_what) = (select u.module, s.name, s.what from updated_ids as u, symbols as s where u.id = s.module_id and s.module_id = names.module_id and s.line = names.line and s.column = names.column) where module_id in (select u.id from updated_ids as u) and (line, column) = (def_line, def_column) and resolved_module is null and resolved_name is null;"
				setResolvedsSymbolIds = execute_ "update names set symbol_id = (select sc.symbol_id from scopes as sc, symbols as s, modules as m where names.module_id == sc.module_id and ((names.qualifier is null and sc.qualifier is null) or (names.qualifier == sc.qualifier)) and names.name == sc.name and s.id == sc.symbol_id and m.id == s.module_id and s.name == names.resolved_name and s.what == names.resolved_what and m.name == names.resolved_module) where module_id in (select u.id from updated_ids as u) and resolved_module is not null and resolved_name is not null and resolved_what is not null;"
				insertQuery = "insert or replace into names (module_id, qualifier, name, line, column, line_to, column_to, def_line, def_column, resolved_module, resolved_name, resolved_what, resolve_error) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);"

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

		updateEnvironment :: SessionMonad m => m ()
		updateEnvironment = do
			execute_ "insert or replace into env (module, name, what, id) select m.name, s.name, s.what, min(s.id) from modules as m, symbols as s, exports as e, updated_ids as u where m.id = s.module_id and s.id = e.symbol_id and e.module_id = u.id group by m.name, s.name, s.what;"

removeModuleContents :: SessionMonad m => Int -> m ()
removeModuleContents mid = scope "remove-modules-contents" $ do
	execute "delete from imports where module_id == ?;" (Only mid)
	execute "delete from exports where module_id == ?;" (Only mid)
	execute "delete from scopes where module_id == ?;" (Only mid)
	execute "delete from names where module_id == ?;" (Only mid)
	execute "delete from types where module_id == ?;" (Only mid)
	execute "delete from symbols where module_id == ?;" (Only mid)
