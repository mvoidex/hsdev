{-# LANGUAGE FlexibleContexts, OverloadedStrings, MultiParamTypeClasses, RankNTypes, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Database.Update (
	Status(..), Progress(..), Task(..), isStatus,
	UpdateOptions(..),

	UpdateM(..),
	runUpdate,

	postStatus, updater, runTask, runTasks, runTasks_,

	scanModules, scanFile, scanFileContents, scanCabal, prepareSandbox, scanSandbox, scanPackageDb, scanProjectFile, scanProjectStack, scanProject, scanDirectory, scanContents,
	scanDocs, inferModTypes,
	scan,
	processEvent, updateEvents, applyUpdates,

	module HsDev.Database.Update.Types,

	module HsDev.Watcher,

	module Control.Monad.Except
	) where

import qualified Control.Concurrent.Async as A
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Exception (ErrorCall, evaluate, displayException)
import Control.Lens hiding ((.=))
import Control.Monad.Catch (catch, handle, MonadThrow)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State (get, modify, evalStateT)
import Data.Aeson
import Data.Aeson.Types
import Data.Function (on)
import Data.List (nubBy, sortBy)
import Data.Ord
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Haskell.Exts as H
import qualified Language.Haskell.Names as N
import System.FilePath
import qualified System.Log.Simple as Log

import Data.LookupTable
import HsDev.Error
import qualified HsDev.Database.SQLite as SQLite
import HsDev.Display
import HsDev.Inspect
import HsDev.Inspect.Order
import HsDev.PackageDb
import HsDev.Project
import HsDev.Sandbox
import qualified HsDev.Stack as S
import HsDev.Symbols
import HsDev.Symbols.Parsed (Parsed)
import HsDev.Tools.Ghc.Session hiding (wait, evaluate)
import HsDev.Tools.Ghc.Types (fileTypes, typedType)
import HsDev.Tools.Types
import HsDev.Tools.HDocs
import qualified HsDev.Scan as S
import HsDev.Scan.Browse
import HsDev.Util (ordNub, fromJSON')
import qualified HsDev.Util as Util (withCurrentDirectory)
import HsDev.Server.Types (commandNotify, inSessionGhc, FileSource(..), serverSources, serverUpdateSources, withSqlTransaction)
import HsDev.Server.Message
import HsDev.Database.Update.Types
import HsDev.Watcher
import Text.Format
import System.Directory.Paths

onStatus :: UpdateMonad m => m ()
onStatus = asks (view (updateOptions . updateTasks)) >>= commandNotify . Notification . toJSON . reverse

childTask :: UpdateMonad m => Task -> m a -> m a
childTask t = local (over (updateOptions . updateTasks) (t:))

isStatus :: Value -> Bool
isStatus = isJust . parseMaybe (parseJSON :: Value -> Parser Task)

runUpdate :: ServerMonadBase m => UpdateOptions -> UpdateM m a -> ClientM m a
runUpdate uopts act = Log.scope "update" $ do
	(r, updatedMods) <- withUpdateState uopts $ \ust ->
		runWriterT (runUpdateM act' `runReaderT` ust)
	Log.sendLog Log.Debug $ "updated {} modules" ~~ length updatedMods
	return r
	where
		act' = do
			(r, _) <- listen act
			-- (r, mlocs') <- listen act

			-- dbs <- liftM S.unions $ forM mlocs' $ \mloc' -> do
			-- 	mid <- SQLite.lookupModuleLocation mloc'
			-- 	case mid of
			-- 		Nothing -> return (S.empty :: S.Set PackageDb)
			-- 		Just mid' -> liftM (S.fromList . map SQLite.fromOnly) $ SQLite.query (SQLite.toQuery $ SQLite.select_
			-- 			["ps.package_db"]
			-- 			["package_dbs as ps", "modules as m"]
			-- 			["m.package_name == ps.package_name", "m.package_version == ps.package_version", "m.id == ?"]) (SQLite.Only mid')

			-- If some sourced files depends on currently scanned package-dbs
			-- We must resolve them and even rescan if there was errors scanning without
			-- dependencies provided (lack of fixities can cause errors inspecting files)

			-- sboxes = databaseSandboxes dbval
			-- sboxOf :: Path -> Maybe Sandbox
			-- sboxOf fpath = find (pathInSandbox fpath) sboxes
			-- projsRows <- SQLite.query_ "select name, cabal, version, ifnull(package_db_stack, json('[]')) from projects;"
			-- let
			-- 	projs = [proj' | (proj' SQLite.:. (SQLite.Only (SQLite.JSON projPdbs))) <- projsRows,
			-- 		not (S.null (S.fromList projPdbs `S.intersection` dbs))]

			-- 	stands = []
			-- 	-- HOWTO?
			-- 	-- stands = do
			-- 	-- 	sloc <- dbval ^.. standaloneSlice . modules . moduleId . moduleLocation
			-- 	-- 	guard $ sboxUpdated $ sboxOf (sloc ^?! moduleFile)
			-- 	-- 	guard (notElem sloc mlocs')
			-- 	-- 	return (sloc, dbval ^.. databaseModules . ix sloc . inspection . inspectionOpts . each . unpacked, Nothing)

			-- Log.sendLog Log.Trace $ "updated package-dbs: {}, have to rescan {} projects and {} files"
			-- 	~~ intercalate ", " (map display $ S.toList dbs)
			-- 	~~ length projs ~~ length stands
			-- (_, rlocs') <- listen $ runTasks_ (scanModules [] stands : [scanProject [] (proj ^. projectCabal) | proj <- projs])
			-- let
			-- 	ulocs' = filter (isJust . preview moduleFile) (ordNub $ mlocs' ++ rlocs')
			-- 	getMods :: (MonadIO m) => m [InspectedModule]
			-- 	getMods = do
			-- 		db' <- liftIO $ readAsync db
			-- 		return $ filter ((`elem` ulocs') . view inspectedKey) $ toList $ view databaseModules db'

			-- FIXME: Now it's broken since `Database` is not used anymore
			when (view updateDocs uopts) $ do
				Log.sendLog Log.Trace "forking inspecting source docs"
				Log.sendLog Log.Warning "not implemented"
				-- void $ fork (getMods >>= waiter . mapM_ scanDocs_)
			when (view updateInfer uopts) $ do
				Log.sendLog Log.Trace "forking inferring types"
				Log.sendLog Log.Warning "not implemented"
				-- void $ fork (getMods >>= waiter . mapM_ inferModTypes_)
			return r
		-- scanDocs_ :: UpdateMonad m => InspectedModule -> m ()
		-- scanDocs_ im = do
		-- 	im' <- (S.scanModify (\opts -> inSessionGhc . liftGhc . inspectDocsGhc opts) im) <|> return im
		-- 	sendUpdateAction $ Log.scope "scan-docs" $ SQLite.updateModule im'
		-- inferModTypes_ :: UpdateMonad m => InspectedModule -> m ()
		-- inferModTypes_ im = do
		-- 	-- TODO: locate sandbox
		-- 	im' <- (S.scanModify infer' im) <|> return im
		-- 	sendUpdateAction $ Log.scope "infer-types" $ SQLite.updateModule im'
		-- infer' :: UpdateMonad m => [String] -> Module -> m Module
		-- infer' opts m = case preview (moduleId . moduleLocation . moduleFile) m of
		-- 	Nothing -> return m
		-- 	Just _ -> inSessionGhc $ do
		-- 		targetSession opts m
		-- 		inferTypes opts m Nothing

-- | Post status
postStatus :: UpdateMonad m => Task -> m ()
postStatus t = childTask t onStatus

-- | Mark module as updated
updater :: UpdateMonad m => [ModuleLocation] -> m ()
updater mlocs = tell $!! mlocs

-- | Run one task
runTask :: (Display t, UpdateMonad m, NFData a) => String -> t -> m a -> m a
runTask action subj act = Log.scope "task" $ do
	postStatus $ set taskStatus StatusWorking task
	x <- childTask task act
	x `deepseq` postStatus (set taskStatus StatusOk task)
	return x
	`catch`
	(\e -> postStatus (set taskStatus (StatusError e) task) >> hsdevError e)
	where
		task = Task {
			_taskName = action,
			_taskStatus = StatusWorking,
			_taskSubjectType = displayType subj,
			_taskSubjectName = display subj,
			_taskProgress = Nothing }

-- | Run many tasks with numeration
runTasks :: UpdateMonad m => [m a] -> m [a]
runTasks ts = liftM catMaybes $ zipWithM taskNum [1..] (map noErr ts) where
	total = length ts
	taskNum n = local setProgress where
		setProgress = set (updateOptions . updateTasks . _head . taskProgress) (Just (Progress n total))
	noErr v = hsdevIgnore Nothing (Just <$> v)

-- | Run many tasks with numeration
runTasks_ :: UpdateMonad m => [m ()] -> m ()
runTasks_ = void . runTasks

-- | Scan modules
scanModules :: UpdateMonad m => [String] -> [S.ModuleToScan] -> m ()
scanModules opts ms = Log.scope "scan-modules" $ mapM_ (uncurry scanModules') grouped where
	scanModules' mproj ms' = do
		pdbs <- maybe (return userDb) (inSessionGhc . getProjectPackageDbStack) mproj
		case mproj of
			Just proj -> sendUpdateAction $ Log.scope "scan-modules" $ SQLite.updateProject proj (Just pdbs)
			Nothing -> return ()
		updater $ ms' ^.. each . _1
		srcs <- serverSources
		defines <- askSession sessionDefines
		-- Make table of already scanned and up to date modules
		let
			isActual (_, _, Just _) = return Nothing
			isActual (mloc, mopts, Nothing) = case findParsedSource mloc of
				Nothing -> return Nothing
				Just parsed' -> do
					modId <- SQLite.lookupModuleLocation mloc
					case modId of
						Nothing -> return Nothing
						Just modId' -> do
							[insp] <- SQLite.query "select inspection_time, inspection_opts from modules as m where m.id == ?;" (SQLite.Only modId')
							insp' <- liftIO $ case mloc of
								FileModule p' _ -> fileInspection p' (opts ++ mopts)
								InstalledModule{} -> installedInspection (opts ++ mopts)
								_ -> return InspectionNone
							if fresh insp insp'
								then do
									m' <- SQLite.loadModule modId'
									return $ Just (mloc, set moduleSource (Just parsed') m')
								else return Nothing

			findParsedSource :: ModuleLocation -> Maybe Parsed
			findParsedSource mloc' = do
				mfile' <- mloc' ^? moduleFile
				srcs ^? ix mfile'

		alreadyScanned <- liftM (M.fromList . catMaybes) $ traverse isActual ms'

		let
			inspectionInfos = M.fromList
				[(mloc, sourceInspection mfile mcts (opts ++ mopts)) |
					(mloc, mopts, mcts) <- ms',
					mfile <- mloc ^.. moduleFile]
			dropScope (N.Scoped _ v) = v
			pload (mloc, mopts, mcts) = runTask "preloading" mloc $
				case alreadyScanned ^? ix mloc of
					Nothing -> do
						p <- liftIO $ preload (mloc ^?! moduleFile) defines (opts ++ mopts) mloc mcts
						return (p, True)
					Just m' -> return (Preloaded (m' ^. moduleId) H.defaultParseMode (fmap dropScope $ m' ^?! moduleSource . _Just) mempty, False)

		ploaded <- runTasks (map pload ms')
		mlocs' <- forM (map fst $ filter snd ploaded) $ \p -> do
			let
				mloc = p ^. preloadedId . moduleLocation
			insp <- liftIO $ inspectionInfos ^?! ix mloc
			let
				inspectedMod = Inspected insp mloc (tag OnlyHeaderTag) $ Right $ p ^. asModule
			sendUpdateAction $ Log.scope "scan-modules/preloaded" $ SQLite.updateModule inspectedMod
			return mloc
		updater mlocs'

		let
			pmods = map fst ploaded

		(sqlMods', sqlAenv') <- Log.scope "exp" $ do
			let
				mprojectDeps = SQLite.buildQuery $ SQLite.select_
					["ps.module_id"]
					["projects_modules_scope as ps"]
					["ps.cabal is ?"]
			sqlMods' <- SQLite.loadModules mprojectDeps (SQLite.Only $ mproj ^? _Just . projectCabal)
			return (sqlMods', mconcat (map moduleAnalyzeEnv sqlMods'))

		Log.sendLog Log.Trace $ "resolving environment: {} modules" ~~ length sqlMods'
		case order pmods of
			Left err -> Log.sendLog Log.Error ("failed order dependencies for files: {}" ~~ show err)
			Right ordered -> do
				ms'' <- flip evalStateT sqlAenv' $ runTasks (map inspect' ordered)
				mlocs'' <- forM ms'' $ \m -> do
					let
						mloc = m ^. moduleId . moduleLocation
					insp <- liftIO $ inspectionInfos ^?! ix mloc
					let
						inspectedMod = Inspected insp mloc mempty (Right m)
					sendUpdateAction $ Log.scope "scan-modules/resolved" $ SQLite.updateModule inspectedMod
					Log.scope "update-sources" $ do
						void $ traverse (serverUpdateSources . uncurry M.insert) ((,) <$> (mloc ^? moduleFile) <*> (m ^. moduleSource))
					return mloc
				updater mlocs''
				where
					inspect' pmod = runTask "scanning" (pmod ^. preloadedId . moduleLocation) $ Log.scope "module" $ do
						aenv <- get
						let
							mloc = pmod ^. preloadedId . moduleLocation
						m <- either (hsdevError . InspectError) eval $ case alreadyScanned ^? ix mloc of
							Nothing -> analyzePreloaded aenv pmod
							Just m' -> Right $ analyzeResolve aenv m'
						modify (mappend (moduleAnalyzeEnv m))
						return m
	grouped = M.toList $ M.unionsWith (++) [M.singleton (m ^? _1 . moduleProject . _Just) [m] | m <- ms]
	eval v = handle onError (v `deepseq` liftIO (evaluate v)) where
		onError :: MonadThrow m => ErrorCall -> m a
		onError = hsdevError . OtherError . displayException

-- | Scan source file, resolve dependent modules
scanFile :: UpdateMonad m => [String] -> Path -> m ()
scanFile opts fpath = scanFiles [(FileSource fpath Nothing, opts)]

-- | Scan source files, resolving dependent modules
scanFiles :: UpdateMonad m => [(FileSource, [String])] -> m ()
scanFiles fsrcs = runTask "scanning" ("files" :: String) $ Log.scope "files" $ hsdevLiftIO $ do
	Log.sendLog Log.Trace $ "scanning {} files" ~~ length fsrcs
	fpaths' <- traverse (liftIO . canonicalize) $ map (fileSource . fst) fsrcs
	forM_ fpaths' $ \fpath' -> do
		ex <- liftIO $ fileExists fpath'
		when (not ex) $ hsdevError $ FileNotFound fpath'
	mlocs <- forM fpaths' $ \fpath' -> do
		mids <- SQLite.query (SQLite.toQuery $ SQLite.qModuleId `mappend` SQLite.where_ ["mu.file == ?"]) (SQLite.Only fpath')
		if length mids > 1
			then return (head mids ^. moduleLocation)
			else do
				mproj <- locateProjectInfo fpath'
				return $ FileModule fpath' mproj
	mapM_ (watch . flip watchModule) mlocs
	S.ScanContents dmods _ _ <- fmap mconcat $ mapM (S.enumDependent . view path) fpaths'
	Log.sendLog Log.Trace $ "dependent modules: {}" ~~ length dmods
	scanModules [] ([(mloc, opts, mcts) | (mloc, (FileSource _ mcts, opts)) <- zip mlocs fsrcs] ++ dmods)

-- | Scan source file with contents and resolve dependent modules
scanFileContents :: UpdateMonad m => [String] -> Path -> Maybe Text -> m ()
scanFileContents opts fpath mcts = scanFiles [(FileSource fpath mcts, opts)]

-- | Scan cabal modules, doesn't rescan if already scanned
scanCabal :: UpdateMonad m => [String] -> m ()
scanCabal opts = Log.scope "cabal" $ scanPackageDbStack opts userDb

-- | Prepare sandbox for scanning. This is used for stack project to build & configure.
prepareSandbox :: UpdateMonad m => Sandbox -> m ()
prepareSandbox sbox@(Sandbox StackWork fpath) = Log.scope "prepare" $ runTasks_ [
	runTask "building dependencies" sbox $ void $ Util.withCurrentDirectory dir $ inSessionGhc $ S.buildDeps Nothing,
	runTask "configuring" sbox $ void $ Util.withCurrentDirectory dir $ inSessionGhc $ S.configure Nothing]
	where
		dir = takeDirectory $ view path fpath
prepareSandbox _ = return ()

-- | Scan sandbox modules, doesn't rescan if already scanned
scanSandbox :: UpdateMonad m => [String] -> Sandbox -> m ()
scanSandbox opts sbox = Log.scope "sandbox" $ do
	prepareSandbox sbox
	pdbs <- inSessionGhc $ sandboxPackageDbStack sbox
	scanPackageDbStack opts pdbs

-- | Scan top of package-db stack, usable for rescan
scanPackageDb :: UpdateMonad m => [String] -> PackageDbStack -> m ()
scanPackageDb opts pdbs = runTask "scanning" (topPackageDb pdbs) $ Log.scope "package-db" $ do
	pdbState <- liftIO $ readPackageDb (topPackageDb pdbs)
	let
		packageDbMods = S.fromList $ concat $ M.elems pdbState
		packages' = M.keys pdbState
	Log.sendLog Log.Trace $ "package-db state: {} modules" ~~ length packageDbMods
	watch (\w -> watchPackageDb w pdbs opts)
	mlocs <- liftM
		(filter (`S.member` packageDbMods)) $
		(inSessionGhc $ listModules opts pdbs packages')
	Log.sendLog Log.Trace $ "{} modules found" ~~ length mlocs
	let
		packageDbMods' = SQLite.query "select m.id, m.file, m.cabal, m.install_dirs, m.package_name, m.package_version, m.installed_name, m.other_location, m.inspection_time, m.inspection_opts from modules as m, package_dbs as ps where m.package_name == ps.package_name and m.package_version == ps.package_version and ps.package_db == ?;" (SQLite.Only (topPackageDb pdbs))
	scan packageDbMods' ((,,) <$> mlocs <*> pure [] <*> pure Nothing) opts $ \mlocs' -> do
		ms <- inSessionGhc $ browseModules opts pdbs (mlocs' ^.. each . _1)
		docs <- inSessionGhc $ hdocsCabal pdbs opts
		Log.sendLog Log.Trace "docs scanned"
		docsTbl <- newLookupTable
		ms' <- mapMOf (each . inspected) (setModuleDocs docsTbl docs) ms
		sendUpdateAction $ Log.scope "scan-package-db" $ do
			mapM_ SQLite.updateModule ms'
			SQLite.updatePackageDb (topPackageDb pdbs) (M.keys pdbState)
		updater $ ms' ^.. each . inspectedKey

-- | Scan top of package-db stack, usable for rescan
scanPackageDbStack :: UpdateMonad m => [String] -> PackageDbStack -> m ()
scanPackageDbStack opts pdbs = runTask "scanning" pdbs $ Log.scope "package-db-stack" $ do
	pdbStates <- liftIO $ mapM readPackageDb (packageDbs pdbs)
	let
		packageDbMods = S.fromList $ concat $ concatMap M.elems pdbStates
		packages' = ordNub $ concatMap M.keys pdbStates
	Log.sendLog Log.Trace $ "package-db-stack state: {} modules" ~~ length packageDbMods
	watch (\w -> watchPackageDbStack w pdbs opts)
	mlocs <- liftM
		(filter (`S.member` packageDbMods)) $
		(inSessionGhc $ listModules opts pdbs packages')
	Log.sendLog Log.Trace $ "{} modules found" ~~ length mlocs
	let
		packageDbStackMods = liftM concat $ forM (packageDbs pdbs) $ \pdb -> SQLite.query "select m.id, m.file, m.cabal, m.install_dirs, m.package_name, m.package_version, m.installed_name, m.other_location, m.inspection_time, m.inspection_opts from modules as m, package_dbs as ps where m.package_name == ps.package_name and m.package_version == ps.package_version and ps.package_db == ?;" (SQLite.Only pdb)
	scan packageDbStackMods ((,,) <$> mlocs <*> pure [] <*> pure Nothing) opts $ \mlocs' -> do
		ms <- inSessionGhc $ browseModules opts pdbs (mlocs' ^.. each . _1)
		Log.sendLog Log.Trace $ "scanned {} modules" ~~ length ms

		-- BUG: I don't know why, but these steps leads to segfault on my PC:
		-- > hsdev scan --cabal --project .
		-- > hsdev check -f .\src\HsDev\Client\Commands.hs
		-- But it works if docs are scanned, it also works from ghci
		
		-- needDocs <- asks (view updateDocs)
		-- ms' <- if needDocs
		-- 	then do
		-- 		docs <- inSessionGhc $ hdocsCabal pdbs opts
		-- 		return $ map (fmap $ setDocs' docs) ms
		-- 	else return ms

		docs <- inSessionGhc $ hdocsCabal pdbs opts
		Log.sendLog Log.Trace "docs scanned"
		docsTbl <- newLookupTable
		ms' <- mapMOf (each . inspected) (setModuleDocs docsTbl docs) ms

		sendUpdateAction $ Log.scope "scan-package-db-stack" $ do
			mapM_ SQLite.updateModule ms'
			sequence_ [SQLite.updatePackageDb pdb (M.keys pdbState) | (pdb, pdbState) <- zip (packageDbs pdbs) pdbStates]

		updater $ ms' ^.. each . inspectedKey

-- | Scan project file
scanProjectFile :: UpdateMonad m => [String] -> Path -> m Project
scanProjectFile opts cabal = runTask "scanning" cabal $ do
	proj <- S.scanProjectFile opts cabal
	sendUpdateAction $ Log.scope "scan-project-file" $ SQLite.updateProject proj Nothing
	return proj

-- | Refine project info and update if necessary
refineProjectInfo :: UpdateMonad m => Project -> m Project
refineProjectInfo proj = do
	[(SQLite.Only exist)] <- SQLite.query "select count(*) > 1 from projects where cabal == ?;" (SQLite.Only (proj ^. projectCabal))
	if exist
		then SQLite.loadProject (proj ^. projectCabal)
		else runTask "scanning" (proj ^. projectCabal) $ do
			proj' <- liftIO $ loadProject proj
			sendUpdateAction $ Log.scope "refine-project-info" $ SQLite.updateProject proj' Nothing
			return proj'

-- | Get project info for module
locateProjectInfo :: UpdateMonad m => Path -> m (Maybe Project)
locateProjectInfo cabal = liftIO (locateProject (view path cabal)) >>= traverse refineProjectInfo

-- | Scan project and related package-db stack
scanProjectStack :: UpdateMonad m => [String] -> Path -> m ()
scanProjectStack opts cabal = do
	proj <- scanProjectFile opts cabal
	scanProject opts cabal
	sbox <- liftIO $ projectSandbox (view projectPath proj)
	maybe (scanCabal opts) (scanSandbox opts) sbox

-- | Scan project
scanProject :: UpdateMonad m => [String] -> Path -> m ()
scanProject opts cabal = runTask "scanning" (project $ view path cabal) $ Log.scope "project" $ do
	proj <- scanProjectFile opts cabal
	watch (\w -> watchProject w proj opts)
	S.ScanContents _ [(_, sources)] _ <- S.enumProject proj
	scanModules opts sources

-- | Scan directory for source files and projects
scanDirectory :: UpdateMonad m => [String] -> Path -> m ()
scanDirectory opts dir = runTask "scanning" dir $ Log.scope "directory" $ do
	S.ScanContents standSrcs projSrcs pdbss <- S.enumDirectory (view path dir)
	runTasks_ [scanProject opts (view projectCabal p) | (p, _) <- projSrcs]
	runTasks_ $ map (scanPackageDb opts) pdbss -- TODO: Don't rescan
	mapMOf_ (each . _1) (watch . flip watchModule) standSrcs
	let
		standaloneMods = SQLite.query "select m.id, m.file, m.cabal, m.install_dirs, m.package_name, m.package_version, m.installed_name, m.other_location, m.inspection_time, m.inspection_opts from modules as m where m.cabal is null and m.file is not null and m.file like ?;" (SQLite.Only $ dir `T.append` "%")
	scan standaloneMods standSrcs opts $ scanModules opts

scanContents :: UpdateMonad m => [String] -> S.ScanContents -> m ()
scanContents opts (S.ScanContents standSrcs projSrcs pdbss) = do
	projs <- liftM (map SQLite.fromOnly) $ SQLite.query_ "select cabal from projects;"
	pdbs <- liftM (map SQLite.fromOnly) $ SQLite.query_ "select package_db from package_dbs;"
	let
		filesMods = SQLite.query_ "select m.id, m.file, m.cabal, m.install_dirs, m.package_name, m.package_version, m.installed_name, m.other_location, m.inspection_time, m.inspection_opts from modules as m where m.file is not null and m.cabal is null;"
	runTasks_ [scanPackageDb opts pdbs' | pdbs' <- pdbss, topPackageDb pdbs' `notElem` pdbs]
	runTasks_ [scanProject opts (view projectCabal p) | (p, _) <- projSrcs, view projectCabal p `notElem` projs]
	mapMOf_ (each . _1) (watch . flip watchModule) standSrcs
	scan filesMods standSrcs opts $ scanModules opts

-- | Scan docs for inspected modules
scanDocs :: UpdateMonad m => [Module] -> m ()
scanDocs = runTasks_ . map scanDocs' where
	scanDocs' m = runTask "scanning docs" (view (moduleId . moduleLocation) m) $ Log.scope "docs" $ do
		mid <- SQLite.lookupModule (m ^. moduleId)
		mid' <- maybe (hsdevError $ SQLiteError $ "module id not found") return mid
		Log.sendLog Log.Trace $ "Scanning docs for {}" ~~ view (moduleId . moduleLocation) m
		docsMap <- inSessionGhc $ do
			opts' <- getModuleOpts [] m
			haddockSession opts'
			liftGhc $ readDocs opts' (m ^?! moduleId . moduleLocation . moduleFile)
		sendUpdateAction $ Log.scope "scan-docs" $ do
			forM_ (maybe [] M.toList docsMap) $ \(nm, doc) -> do
				SQLite.execute "update symbols set docs = ? where name == ? and module_id == ?;"
					(doc, nm, mid')
			SQLite.execute "update modules set tags = json_set(tags, '$.docs', 1) where id == ?;" (SQLite.Only mid')

-- | Infer types for modules
inferModTypes :: UpdateMonad m => [Module] -> m ()
inferModTypes = runTasks_ . map inferModTypes' where
	inferModTypes' m = runTask "inferring types" (view (moduleId . moduleLocation) m) $ Log.scope "types" $ do
		mid <- SQLite.lookupModule (m ^. moduleId)
		mid' <- maybe (hsdevError $ SQLiteError $ "module id not found") return mid
		Log.sendLog Log.Trace $ "Inferring types for {}" ~~ view (moduleId . moduleLocation) m
		types' <- inSessionGhc $ do
			opts' <- getModuleOpts [] m
			targetSession opts' m
			fileTypes opts' m Nothing
		let
			sortedTypes' =
				nubBy ((==) `on` (view (noteRegion . regionFrom))) $
				sortBy (comparing $ view noteRegion) types'
			typeMap = M.fromList [(t' ^. noteRegion . regionFrom, t' ^. note . typedType) | t' <- sortedTypes']

		sendUpdateAction $ Log.scope "infer-types" $ do
			points <- SQLite.query "select s.line, s.column from symbols as s where (s.module_id == ?) and (s.line is not null) and (s.column is not null);"
				(SQLite.Only mid')
			forM_ points $ \(l, c) -> case M.lookup (Position l c) typeMap of
				Nothing -> return ()
				Just t' -> SQLite.execute "update symbols set type = ? where (line == ?) and (column == ?) and (module_id == ?);"
					(t', l, c, mid')
			SQLite.execute "update modules set tags = json_set(tags, '$.types', 1) where id == ?;" (SQLite.Only mid')

-- | Generic scan function. Removed obsolete modules and calls callback on changed modules.
scan :: UpdateMonad m
	=> m [(SQLite.Only Int) SQLite.:. ModuleLocation SQLite.:. Inspection]
	-- ^ Get affected modules, obsolete will be removed, changed will be updated
	-> [S.ModuleToScan]
	-- ^ Actual modules, other will be removed
	-> [String]
	-- ^ Extra scan options
	-> ([S.ModuleToScan] -> m ())
	-- ^ Update function
	-> m ()
scan part' mlocs opts act = Log.scope "scan" $ do
	mlocs' <- liftM (M.fromList . map (\((SQLite.Only mid) SQLite.:. (m SQLite.:. i)) -> (m, (mid, i)))) part'
	let
		obsolete = M.filterWithKey (\k _ -> k `S.notMember` (S.fromList $ map (^. _1) mlocs)) mlocs'
	changed <- liftIO $ S.changedModules (M.map snd mlocs') opts mlocs
	sendUpdateAction $ Log.scope "scan/remove-obsolete" $ forM_ (M.elems obsolete) $ SQLite.removeModule . fst
	act changed

processEvent :: ([(Watched, Event)] -> IO ()) -> MVar (A.Async ()) -> MVar [(Watched, Event)] -> Watched -> Event -> ClientM IO ()
processEvent handleEvents updaterTask eventsVar w e = Log.scope "event" $ do
	Log.sendLog Log.Trace $ "event received: {}" ~~ view eventPath e
	l <- Log.askLog
	liftIO $ do
		modifyMVar_ eventsVar (return . ((w, e):))
		modifyMVar_ updaterTask $ \task -> do
			done <- fmap isJust $ poll task
			if done
				then do
					Log.withLog l $ Log.sendLog Log.Trace "starting update thread"
					A.async $ fix $ \loop -> do
						updates <- modifyMVar eventsVar (\es -> return ([], es))
						when (not $ null updates) $ handleEvents updates >> loop
				else return task

updateEvents :: ServerMonadBase m => [(Watched, Event)] -> UpdateM m ()
updateEvents updates = Log.scope "updater" $ do
	Log.sendLog Log.Trace $ "prepared to process {} events" ~~ length updates
	files <- fmap concat $ forM updates $ \(w, e) -> case w of
		WatchedProject proj projOpts
			| isSource e -> do
				Log.sendLog Log.Info $ "File '{file}' in project {proj} changed"
					~~ ("file" ~% view eventPath e)
					~~ ("proj" ~% view projectName proj)
				[SQLite.Only mopts] <- SQLite.query "select inspection_opts from modules where file == ?;" (SQLite.Only $ view eventPath e)
				opts <- maybe (return []) (maybe (parseErr' mopts) return . fromJSON') mopts
				return [(FileSource (fromFilePath $ view eventPath e) Nothing, opts)]
			| isCabal e -> do
				Log.sendLog Log.Info $ "Project {proj} changed"
					~~ ("proj" ~% view projectName proj)
				scanProject projOpts $ view projectCabal proj
				return []
			| otherwise -> return []
		WatchedPackageDb pdbs opts
			| isConf e -> do
				Log.sendLog Log.Info $ "Package db {package} changed"
					~~ ("package" ~% topPackageDb pdbs)
				scanPackageDb opts pdbs
				return []
			| otherwise -> return []
		WatchedModule
			| isSource e -> do
				Log.sendLog Log.Info $ "Module {file} changed"
					~~ ("file" ~% view eventPath e)
				[SQLite.Only mopts] <- SQLite.query "select inspection_opts from modules where file == ?;" (SQLite.Only $ view eventPath e)
				opts <- maybe (return []) (maybe (parseErr' mopts) return . fromJSON') mopts
				return [(FileSource (fromFilePath $ view eventPath e) Nothing, opts)]
			| otherwise -> return []
	scanFiles files
	where
		parseErr' mopts' = do
			Log.sendLog Log.Error $ "Error parsing inspection_opts: {}" ~~ show mopts'
			hsdevError $ SQLiteError $ "Error parsing inspection_opts: {}" ~~ show mopts'

applyUpdates :: UpdateOptions -> [(Watched, Event)] -> ClientM IO ()
applyUpdates uopts = runUpdate uopts . updateEvents

watch :: SessionMonad m => (Watcher -> IO ()) -> m ()
watch f = do
	w <- askSession sessionWatcher
	liftIO $ f w
