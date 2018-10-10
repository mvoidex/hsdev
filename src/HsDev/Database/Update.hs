{-# LANGUAGE FlexibleContexts, OverloadedStrings, MultiParamTypeClasses, RankNTypes, TypeOperators, TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Database.Update (
	Status(..), Progress(..), Task(..),
	UpdateOptions(..),

	UpdateM(..),
	runUpdate,

	postStatus, updater, runTask, runTasks, runTasks_,

	scanModules, scanFile, scanFiles, scanFileContents, scanCabal, prepareSandbox, scanSandbox, scanPackageDb, scanPackageDbStack, scanProjectFile, scanProjectStack, scanProject, scanDirectory,
	scanPackageDbStackDocs, scanDocs,
	setModTypes, inferModTypes,
	scan,
	processEvents, updateEvents, applyUpdates,

	cacheGhcWarnings, cachedWarnings,

	module HsDev.Database.Update.Types,

	module HsDev.Watcher,

	module Control.Monad.Except
	) where

import qualified Control.Concurrent.Async as A
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Exception (ErrorCall, evaluate, displayException)
import Control.Lens
import Control.Monad.Catch (catch, handle, MonadThrow, bracket_)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State (get, modify, runStateT)
import Data.Aeson
import Data.List (intercalate)
import Data.String (fromString)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe
import Data.Text (Text)
import Data.Time.Clock.POSIX
import qualified Data.Text as T
import System.FilePath
import qualified System.Log.Simple as Log

import Data.Maybe.JustIf
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
import HsDev.Tools.Ghc.Session hiding (Session, evaluate)
import HsDev.Tools.Ghc.Types (fileTypes, TypedExpr)
import HsDev.Tools.Types
import HsDev.Tools.HDocs
import qualified HsDev.Scan as S
import HsDev.Scan.Browse
import HsDev.Util (ordNub, fromJSON', uniqueBy, timer)
import qualified HsDev.Util as Util (withCurrentDirectory)
import HsDev.Server.Types (commandNotify, inSessionGhc, FileSource(..))
import HsDev.Server.Message
import HsDev.Database.Update.Types
import HsDev.Watcher
import Text.Format
import System.Directory.Paths

onStatus :: UpdateMonad m => m ()
onStatus = asks (view (updateOptions . updateTasks)) >>= commandNotify . Notification . toJSON . reverse

childTask :: UpdateMonad m => Task -> m a -> m a
childTask t = local (over (updateOptions . updateTasks) (t:))

transact :: SessionMonad m => m a -> m a
transact = SQLite.transaction_ SQLite.Immediate

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

data PreloadFailure = PreloadFailure ModuleLocation Inspection HsDevError

instance NFData PreloadFailure where
	rnf (PreloadFailure mloc insp err) = rnf mloc `seq` rnf insp `seq` rnf err

-- | Scan modules
scanModules :: UpdateMonad m => [String] -> [S.ModuleToScan] -> m ()
scanModules opts ms = Log.scope "scan-modules" $ mapM_ (uncurry scanModules') grouped where
	scanModules' mproj ms' = do
		maybe (return ()) (sendUpdateAction . SQLite.updateProject) mproj
		updater $ ms' ^.. each . _1
		defines <- askSession sessionDefines

		let
			pload (mloc, mopts, mcts) = runTask "preloading" mloc $ do
				mfcts <- maybe (S.getFileContents (mloc ^?! moduleFile)) (const $ return Nothing) mcts
				insp <- liftIO $ fileInspection (mloc ^?! moduleFile) (opts ++ mopts)
				case (mfcts ^? _Just . _1) of
					Just tm -> Log.sendLog Log.Trace $ "using edited file contents, mtime = {}" ~~ show tm
					Nothing -> return ()
				let
					inspection' = maybe insp (fileContentsInspection_ (opts ++ mopts)) $ mfcts ^? _Just . _1
					dirtyTag' = maybe id (const $ inspectTag DirtyTag) $ mfcts ^? _Just . _1
					mcts' = mplus mcts (mfcts ^? _Just . _2)
				runInspect mloc $ withInspection (return inspection') $ dirtyTag' $ preload (mloc ^?! moduleFile) defines (opts ++ mopts) mcts'

		ploaded <- runTasks (map pload ms')
		sendUpdateAction $ void $ SQLite.upsertModules $ map (fmap (view asModule)) ploaded
		let
			mlocs' = ploaded ^.. each . inspected . preloadedId . moduleLocation

		updater mlocs'

		let
			mcabal = mproj ^? _Just . projectCabal

		(env, fixities) <- loadEnv mcabal

		Log.sendLog Log.Trace $ "resolved environment: {} modules" ~~ M.size env
		case orderBy (preview inspected) ploaded of
			Left err -> Log.sendLog Log.Error ("failed order dependencies for files: {}" ~~ show err)
			Right ordered -> do
				(ms'', (updEnv, updFixities)) <- flip runStateT (env, fixities) $ runTasks (map inspect' ordered)
				saveEnv mcabal updEnv updFixities
				mlocs'' <- timer "updated scanned modules" $ do
					Log.sendLog Log.Trace $ case mproj of
						Just proj -> "inserting data for resolved modules of project: {}" ~~ proj
						Nothing -> "inserting data for resolved standalone modules"
					sendUpdateAction $ Log.scope "resolved" $ updateResolveds mcabal ms''
					return (ms'' ^.. each . inspectedKey)
				updater mlocs''
				where
					inspect' pmod = runTask "scanning" ploc $ Log.scope "module" $ do
						(env', fixities') <- get
						r <- continueInspect pmod $ \p -> do
							resolved' <- msum [
								resolveModule env' fixities' p,
								do
									lift (Log.sendLog Log.Trace ("error resolving module {}, falling to resolving just imports/scope" ~~ (p ^. preloadedId . moduleLocation)))
									resolvePreloaded env' p]
							eval resolved'
						modify $ mappend (
							maybe mempty resolvedEnv (r ^? inspected),
							maybe mempty resolvedFixitiesTable (r ^? inspected))
						return r
						where
							ploc = pmod ^?! inspected . preloadedId . moduleLocation

	grouped = M.toList $ M.unionsWith (++) [M.singleton (m ^? _1 . moduleProject . _Just) [m] | m <- ms]
	eval v = handle onError (v `deepseq` liftIO (evaluate v)) where
		onError :: MonadThrow m => ErrorCall -> m a
		onError = hsdevError . OtherError . displayException

-- | Scan source file, possibly scanning also related project and installed modules
scanFile :: UpdateMonad m => [String] -> Path -> BuildTool -> Bool -> Bool -> m ()
scanFile opts fpath tool scanProj scanDb = do
	mproj <- fmap (set (_Just . projectBuildTool) tool) $ locateProjectInfo fpath
	sbox <- maybe (return userDb) (inSessionGhc . getProjectPackageDbStack) mproj
	when scanDb $ do
		[SQLite.Only scanned] <- SQLite.query @_ @(SQLite.Only Bool) "select count(*) > 0 from package_dbs as pdbs where pdbs.package_db = ?;" (SQLite.Only (topPackageDb sbox))
		unless scanned $ scanPackageDbStack opts sbox
	case join (mproj `justIf` scanProj) of
		Nothing -> scanFiles [(FileSource fpath Nothing, opts)]
		Just proj -> scanProject opts tool (view projectCabal proj)

-- | Scan source files, resolving dependent modules
scanFiles :: UpdateMonad m => [(FileSource, [String])] -> m ()
scanFiles fsrcs = runTask "scanning" ("files" :: String) $ Log.scope "files" $ hsdevLiftIO $ do
	Log.sendLog Log.Trace $ "scanning {} files" ~~ length fsrcs
	fpaths' <- traverse (liftIO . canonicalize) $ map (fileSource . fst) fsrcs
	forM_ fpaths' $ \fpath' -> do
		ex <- liftIO $ fileExists fpath'
		unless ex $ hsdevError $ FileNotFound fpath'
	mlocs <- forM fpaths' $ \fpath' -> do
		mids <- SQLite.query (SQLite.toQuery $ SQLite.qModuleId `mappend` SQLite.where_ ["mu.file == ?"]) (SQLite.Only fpath')
		if length mids > 1
			then return (head mids ^. moduleLocation)
			else do
				mproj <- locateProjectInfo fpath'
				return $ FileModule fpath' mproj
	let
		filesMods = liftM concat $ forM fpaths' $ \fpath' -> SQLite.query "select m.id, m.file, m.cabal, m.install_dirs, m.package_name, m.package_version, m.installed_name, m.exposed, m.other_location, m.inspection_time, m.inspection_opts from modules as m where m.file == ?;" (SQLite.Only fpath')
	scan filesMods [(mloc, opts, mcts) | (mloc, (FileSource _ mcts, opts)) <- zip mlocs fsrcs] [] $ \mlocs' -> do
		mapM_ ((watch . flip watchModule) . view _1) mlocs'
		S.ScanContents dmods _ _ <- fmap mconcat $ mapM (S.enumDependent . view (_1 . moduleFile . path)) mlocs'
		Log.sendLog Log.Trace $ "dependent modules: {}" ~~ length dmods
		scanModules [] (mlocs' ++ dmods)

-- | Scan source file with contents and resolve dependent modules
scanFileContents :: UpdateMonad m => [String] -> Path -> Maybe Text -> m ()
scanFileContents opts fpath mcts = scanFiles [(FileSource fpath mcts, opts)]

-- | Scan cabal modules, doesn't rescan if already scanned
scanCabal :: UpdateMonad m => [String] -> m ()
scanCabal opts = Log.scope "cabal" $ scanPackageDbStack opts userDb

-- | Prepare sandbox for scanning. This is used for stack project to build & configure.
prepareSandbox :: UpdateMonad m => Sandbox -> m ()
prepareSandbox sbox@(Sandbox StackTool fpath) = Log.scope "prepare" $ runTasks_ [
	runTask "building dependencies" sbox $ void $ Util.withCurrentDirectory dir $ inSessionGhc $ S.buildDeps Nothing]
	where
		dir = takeDirectory $ view path fpath
prepareSandbox _ = return ()

-- | Scan sandbox modules, doesn't rescan if already scanned
scanSandbox :: UpdateMonad m => [String] -> Sandbox -> m ()
scanSandbox opts sbox = Log.scope "sandbox" $ do
	-- prepareSandbox sbox
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

	pkgs <- SQLite.query "select package_name, package_version from package_dbs where package_db == ?;" (SQLite.Only $ topPackageDb pdbs)
	if S.fromList packages' == S.fromList pkgs
		then Log.sendLog Log.Trace "nothing changes, all packages the same"
		else do
			mlocs <- liftM
				(filter (`S.member` packageDbMods)) $
				(inSessionGhc $ listModules opts pdbs packages')
			let
				umlocs = uniqueModuleLocations mlocs
			Log.sendLog Log.Trace $ "{} modules found, {} unique" ~~ length mlocs ~~ length umlocs
			let
				packageDbMods' = SQLite.query "select m.id, m.file, m.cabal, m.install_dirs, m.package_name, m.package_version, m.installed_name, m.exposed, m.other_location, m.inspection_time, m.inspection_opts from modules as m, package_dbs as ps where m.package_name == ps.package_name and m.package_version == ps.package_version and ps.package_db == ?;" (SQLite.Only (topPackageDb pdbs))
			scan packageDbMods' ((,,) <$> umlocs <*> pure [] <*> pure Nothing) opts $ \mlocs' -> do
				ms <- inSessionGhc $ browseModules opts pdbs (mlocs' ^.. each . _1)
				Log.sendLog Log.Trace $ "scanned {} modules" ~~ length ms
				sendUpdateAction $ timer "updated package-db modules" $ do
					SQLite.updateModules ms
					SQLite.updatePackageDb (topPackageDb pdbs) (M.keys pdbState)

				when hdocsSupported $ scanPackageDbStackDocs opts pdbs

				updater $ ms ^.. each . inspectedKey

-- | Scan top of package-db stack, usable for rescan
scanPackageDbStack :: UpdateMonad m => [String] -> PackageDbStack -> m ()
scanPackageDbStack opts pdbs = runTask "scanning" pdbs $ Log.scope "package-db-stack" $ do
	pdbStates <- liftIO $ mapM readPackageDb (packageDbs pdbs)
	let
		packageDbMods = S.fromList $ concat $ concatMap M.elems pdbStates
		packages' = ordNub $ concatMap M.keys pdbStates
	Log.sendLog Log.Trace $ "package-db-stack state: {} modules" ~~ length packageDbMods
	watch (\w -> watchPackageDbStack w pdbs opts)

	pkgs <- liftM concat $ forM (packageDbs pdbs) $ \pdb -> SQLite.query "select package_name, package_version from package_dbs where package_db == ?;" (SQLite.Only pdb)
	if S.fromList packages' == S.fromList pkgs
		then Log.sendLog Log.Trace "nothing changes, all packages the same"
		else do
			mlocs <- liftM
				(filter (`S.member` packageDbMods)) $
				(inSessionGhc $ listModules opts pdbs packages')
			let
				umlocs = uniqueModuleLocations mlocs
			Log.sendLog Log.Trace $ "{} modules found, {} unique" ~~ length mlocs ~~ length umlocs
			let
				packageDbStackMods = liftM concat $ forM (packageDbs pdbs) $ \pdb -> SQLite.query "select m.id, m.file, m.cabal, m.install_dirs, m.package_name, m.package_version, m.installed_name, m.exposed, m.other_location, m.inspection_time, m.inspection_opts from modules as m, package_dbs as ps where m.package_name == ps.package_name and m.package_version == ps.package_version and ps.package_db == ?;" (SQLite.Only pdb)
			scan packageDbStackMods ((,,) <$> umlocs <*> pure [] <*> pure Nothing) opts $ \mlocs' -> do
				ms <- inSessionGhc $ browseModules opts pdbs (mlocs' ^.. each . _1)
				Log.sendLog Log.Trace $ "scanned {} modules" ~~ length ms
				sendUpdateAction $ timer "updated package-db-stack modules" $ do
					SQLite.updateModules ms
					sequence_ [SQLite.updatePackageDb pdb (M.keys pdbState) | (pdb, pdbState) <- zip (packageDbs pdbs) pdbStates]

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

				when hdocsSupported $ scanPackageDbStackDocs opts pdbs

				updater $ ms ^.. each . inspectedKey

-- | Scan project file
scanProjectFile :: UpdateMonad m => [String] -> BuildTool -> Path -> m Project
scanProjectFile opts tool cabal = runTask "scanning" cabal $ do
	proj <- fmap (set projectBuildTool tool) $ S.scanProjectFile opts cabal
	pdbs <- inSessionGhc $ getProjectPackageDbStack proj
	let
		proj' = set projectPackageDbStack (Just pdbs) proj
	sendUpdateAction $ Log.scope "scan-project-file" $ SQLite.updateProject proj'
	return proj'

-- | Refine project info and update if necessary
refineProjectInfo :: UpdateMonad m => Project -> m Project
refineProjectInfo proj = do
	[SQLite.Only exist] <- SQLite.query "select count(*) > 0 from projects where cabal == ?;" (SQLite.Only (proj ^. projectCabal))
	if exist
		then SQLite.loadProject (proj ^. projectCabal)
		else runTask "scanning" (proj ^. projectCabal) $ do
			proj' <- liftIO $ loadProject proj
			pdbs <- inSessionGhc $ getProjectPackageDbStack proj'
			let
				proj'' = set projectPackageDbStack (Just pdbs) proj'
			sendUpdateAction $ Log.scope "refine-project-info" $ SQLite.updateProject proj''
			return proj''

-- | Get project info for module
locateProjectInfo :: UpdateMonad m => Path -> m (Maybe Project)
locateProjectInfo cabal = liftIO (locateProject (view path cabal)) >>= traverse refineProjectInfo

-- | Scan project and related package-db stack
scanProjectStack :: UpdateMonad m => [String] -> BuildTool -> Path -> m ()
scanProjectStack opts tool cabal = do
	sbox <- liftIO $ projectSandbox tool cabal
	maybe (scanCabal opts) (scanSandbox opts) sbox
	scanProject opts tool cabal

-- | Scan project
scanProject :: UpdateMonad m => [String] -> BuildTool -> Path -> m ()
scanProject opts tool cabal = runTask "scanning" (project $ view path cabal) $ Log.scope "project" $ do
	proj <- scanProjectFile opts tool cabal
	watch (\w -> watchProject w proj opts)
	S.ScanContents _ [(_, sources)] _ <- S.enumProject proj
	let
		projMods = SQLite.query "select m.id, m.file, m.cabal, m.install_dirs, m.package_name, m.package_version, m.installed_name, m.exposed, m.other_location, m.inspection_time, m.inspection_opts from modules as m where m.file is not null and m.cabal == ?;" (SQLite.Only $ proj ^. projectCabal)
	scan projMods sources opts $ scanModules opts

		-- Scan docs
		-- inSessionGhc $ do
		-- 	currentSession >>= maybe (return ()) (const clearTargets)

		-- 	forM_ (maybe [] targetInfos (proj ^. projectDescription)) $ \tinfo' -> do
		-- 		opts' <- getProjectTargetOpts [] proj (tinfo' ^. targetBuildInfo)
		-- 		files' <- projectTargetFiles proj tinfo'
		-- 		haddockSession opts'
		-- 		docsMap <- liftGhc $ readProjectTargetDocs opts' proj files'
		-- 		Log.sendLog Log.Debug $ "scanned logs for modules: {}, summary docs: {}" ~~ (intercalate "," (M.keys docsMap)) ~~ (sum $ map M.size $ M.elems docsMap)


-- | Scan directory for source files and projects
scanDirectory :: UpdateMonad m => [String] -> Path -> m ()
scanDirectory opts dir = runTask "scanning" dir $ Log.scope "directory" $ do
	S.ScanContents standSrcs projSrcs pdbss <- S.enumDirectory (view path dir)
	runTasks_ [scanProject opts CabalTool (view projectCabal p) | (p, _) <- projSrcs]
	runTasks_ $ map (scanPackageDb opts) pdbss -- TODO: Don't rescan
	mapMOf_ (each . _1) (watch . flip watchModule) standSrcs
	let
		standaloneMods = SQLite.query "select m.id, m.file, m.cabal, m.install_dirs, m.package_name, m.package_version, m.installed_name, m.exposed, m.other_location, m.inspection_time, m.inspection_opts from modules as m where m.cabal is null and m.file is not null and m.file like ? escape '\\';" (SQLite.Only $ SQLite.escapeLike dir `T.append` "%")
	scan standaloneMods standSrcs opts $ scanModules opts

-- | Scan installed docs
scanPackageDbStackDocs :: UpdateMonad m => [String] -> PackageDbStack -> m ()
scanPackageDbStackDocs opts pdbs
	| hdocsSupported = Log.scope "docs" $ do
		docs <- inSessionGhc $ hdocsCabal pdbs opts
		Log.sendLog Log.Trace $ "docs scanned: {} packages, {} modules total"
			~~ length docs ~~ sum (map (M.size . snd) docs)
		sendUpdateAction $ transact $ SQLite.executeMany "update symbols set docs = ? where name == ? and module_id in (select id from modules where name == ? and package_name == ? and package_version == ?);" $ do
			(ModulePackage pname pver, pdocs) <- docs
			(mname, mdocs) <- M.toList pdocs
			(nm, doc) <- M.toList mdocs
			return (doc, nm, mname, pname, pver)
		Log.sendLog Log.Trace "docs set"
	| otherwise = Log.sendLog Log.Warning "hdocs not supported"

-- | Scan docs for inspected modules
scanDocs :: UpdateMonad m => [Module] -> m ()
scanDocs
	| hdocsSupported = runTasks_ . map scanDocs'
	| otherwise = const $ Log.sendLog Log.Warning "hdocs not supported"
	where
		scanDocs' m = runTask "scanning docs" (view (moduleId . moduleLocation) m) $ Log.scope "docs" $ do
			mid <- SQLite.lookupModule (m ^. moduleId)
			mid' <- maybe (hsdevError $ SQLiteError "module id not found") return mid
			m' <- mapMOf (moduleId . moduleLocation . moduleProject . _Just) refineProjectInfo m
			Log.sendLog Log.Trace $ "Scanning docs for {}" ~~ view (moduleId . moduleLocation) m'
			docsMap <- inSessionGhc $ do
				(pdbs, opts') <- getModuleOpts [] m'
				currentSession >>= maybe (return ()) (const clearTargets)
				-- Calling haddock with targets set sometimes cause errors
				haddockSession pdbs opts'
				readModuleDocs opts' m'
			sendUpdateAction $ transact $ do
				SQLite.executeMany "update symbols set docs = ? where name == ? and module_id == ?;"
					[(doc, nm, mid') | (nm, doc) <- maybe [] M.toList docsMap]
				SQLite.execute "update modules set tags = json_set(tags, '$.docs', 1) where id == ?;" (SQLite.Only mid')

-- | Set inferred types for module
setModTypes :: UpdateMonad m => ModuleId -> [Note TypedExpr] -> m ()
setModTypes m ts = Log.scope "set-types" $ do
	mid <- SQLite.lookupModule m
	mid' <- maybe (hsdevError $ SQLiteError "module id not found") return mid
	sendUpdateAction $ transact $ do
		SQLite.execute "delete from types where module_id = ?;" (SQLite.Only mid')
		SQLite.executeMany "insert into types (module_id, line, column, line_to, column_to, expr, type) values (?, ?, ?, ?, ?, ?, ?);" [
			(SQLite.Only mid' SQLite.:. view noteRegion n' SQLite.:. view note n') | n' <- uniqueBy (view noteRegion) ts]
		SQLite.execute "update names set inferred_type = (select type from types as t where t.module_id = ? and names.line = t.line and names.column = t.column and names.line_to = t.line_to and names.column_to = t.column_to) where module_id == ?;"
			(mid', mid')
		SQLite.execute "update symbols set type = (select type from types as t where t.module_id = ? and symbols.line = t.line and symbols.column = t.column order by t.line_to, t.column_to) where module_id == ? and type is null;" (mid', mid')
		SQLite.execute "update modules set tags = json_set(tags, '$.types', 1) where id == ?;" (SQLite.Only mid')

-- | Infer types for modules
inferModTypes :: UpdateMonad m => [Module] -> m ()
inferModTypes = runTasks_ . map inferModTypes' where
	inferModTypes' m = runTask "inferring types" (view (moduleId . moduleLocation) m) $ Log.scope "types" $ do
		mid <- SQLite.lookupModule (m ^. moduleId)
		_ <- maybe (hsdevError $ SQLiteError "module id not found") return mid
		m' <- mapMOf (moduleId . moduleLocation . moduleProject . _Just) refineProjectInfo m
		Log.sendLog Log.Trace $ "Inferring types for {}" ~~ view (moduleId . moduleLocation) m'

		sess <- getSession
		mcts <- fmap (fmap snd) $ S.getFileContents (m' ^?! moduleId . moduleLocation . moduleFile)
		types' <- inSessionGhc $ do
			targetSession [] m'
			cacheGhcWarnings sess (m' ^.. moduleId . moduleLocation) $
				fileTypes m' mcts

		setModTypes (m' ^. moduleId) types'

-- | Generic scan function. Removed obsolete modules and calls callback on changed modules.
scan :: UpdateMonad m
	=> m [SQLite.Only Int SQLite.:. ModuleLocation SQLite.:. Inspection]
	-- ^ Get affected modules, obsolete will be removed, changed will be updated
	-> [S.ModuleToScan]
	-- ^ Actual modules, other will be removed
	-> [String]
	-- ^ Extra scan options
	-> ([S.ModuleToScan] -> m ())
	-- ^ Update function
	-> m ()
scan part' mlocs opts act = Log.scope "scan" $ do
	mlocs' <- liftM (M.fromList . map (\(SQLite.Only mid SQLite.:. (m SQLite.:. i)) -> (m, (mid, i)))) part'
	let
		obsolete = M.filterWithKey (\k _ -> k `S.notMember` S.fromList (map (^. _1) mlocs)) mlocs'
	changed <- S.changedModules (M.map snd mlocs') opts mlocs
	sendUpdateAction $ Log.scope "remove-obsolete" $ transact $
		forM_ (M.elems obsolete) $ SQLite.removeModule . fst
	act changed

processEvents :: ([(Watched, Event)] -> IO ()) -> MVar (A.Async ()) -> MVar [(Watched, Event)] -> [(Watched, Event)] -> ClientM IO ()
processEvents handleEvents updaterTask eventsVar evs = Log.scope "event" $ do
	Log.sendLog Log.Trace $ "events received: {}" ~~ intercalate ", " (evs ^.. each . _2 . eventPath)
	l <- Log.askLog
	liftIO $ do
		modifyMVar_ eventsVar (return . (++evs))
		modifyMVar_ updaterTask $ \task -> do
			done <- fmap isJust $ poll task
			if done
				then do
					Log.withLog l $ Log.sendLog Log.Trace "starting update thread"
					A.async $ fix $ \loop -> do
						updates <- modifyMVar eventsVar (\es -> return ([], es))
						unless (null updates) $ handleEvents updates >> loop
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
				scanProject projOpts (view projectBuildTool proj) (view projectCabal proj)
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

-- Save ghc warnings on loading target, because second loading won't produce any
cacheGhcWarnings :: Session -> [ModuleLocation] -> GhcM a -> GhcM a
cacheGhcWarnings sess mlocs act = Log.scope "cache-warnings" $ do
	tm <- liftIO getPOSIXTime
	(r, msgs) <- collectMessages act
	Log.sendLog Log.Trace $ "collected {} warnings" ~~ length msgs
	_ <- liftIO $ withSession sess $ postSessionUpdater $ refreshCache mlocs tm msgs
	return r
	where
		refreshCache :: [ModuleLocation] -> POSIXTime -> [Note OutputMessage] -> ServerM IO ()
		refreshCache mlocs' tm' msgs' = Log.scope "refresh" $ bracket_ initTemp dropTemp $ do
			fillTemp
			removeOutdated
			insertMessages
			where
				initTemp :: SessionMonad m => m ()
				initTemp = do
					SQLite.execute_ "create temporary table updating_ids (id integer not null unique);"
					SQLite.execute_ "create temporary table updating_messages as select * from messages where 0;"
					SQLite.execute_ "create index update_messages_module_id_index on updating_messages (module_id);"

				dropTemp :: SessionMonad m => m ()
				dropTemp = do
					SQLite.execute_ "drop table if exists updating_ids;"
					SQLite.execute_ "drop table if exists updating_messages;"

				fillTemp :: SessionMonad m => m ()
				fillTemp = do
					SQLite.executeMany "insert into updating_ids select distinct m.id from modules as m where (m.file = ?);" $ (map SQLite.Only $ mlocs' ^.. each . moduleFile)
					SQLite.executeMany "insert into updating_messages select (select m.id from modules as m where (m.file = ?)), ?, ?, ?, ?, ?, ?, ?;" msgs'
					SQLite.execute_ "insert into updating_ids select distinct umsgs.module_id from updating_messages as umsgs where umsgs.module_id not in (select id from updating_ids);"

				removeOutdated :: SessionMonad m => m ()
				removeOutdated = SQLite.execute_ $ fromString $ unlines [
					"delete from messages",
					"where",
					" module_id in (",
					"  select um.id",
					"  from",
					"   updating_ids as um, modules as m",
					"  left outer join",
					"   load_times as lt",
					"  on",
					"   lt.module_id = um.id",
					"  where",
					"   um.id = m.id and (",
					"    lt.load_time is null or",
					"    lt.load_time <= m.inspection_time or",
					"    um.id in (select distinct umsgs.module_id from updating_messages as umsgs)",
					"   )",
					" );"]

				insertMessages :: SessionMonad m => m ()
				insertMessages = SQLite.transaction_ SQLite.Deferred $ do
					SQLite.execute "insert or replace into load_times (module_id, load_time) select um.id, ? from updating_ids as um;" (SQLite.Only tm')
					SQLite.execute_ "insert into messages select distinct * from updating_messages;"

-- | Get cached warnings
cachedWarnings :: SessionMonad m => [ModuleLocation] -> m [Note OutputMessage]
cachedWarnings mlocs = liftM concat $ forM (mlocs ^.. each . moduleFile) $ \f -> SQLite.query @_ @(Note OutputMessage) (SQLite.toQuery $ mconcat [
	SQLite.qNote "m" "n",
	SQLite.from_ ["load_times as lt"],
	SQLite.where_ [
		"lt.module_id = m.id",
		"m.file = ?",
		"lt.load_time >= m.inspection_time"]])
	(SQLite.Only f)

watch :: SessionMonad m => (Watcher -> IO ()) -> m ()
watch f = whenJustM (askSession sessionWatcher) $ liftIO . f
