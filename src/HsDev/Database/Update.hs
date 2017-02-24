{-# LANGUAGE FlexibleContexts, OverloadedStrings, MultiParamTypeClasses, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Database.Update (
	Status(..), Progress(..), Task(..), isStatus,
	UpdateOptions(..),

	UpdateM(..),
	runUpdate,

	postStatus, waiter, updater, loadCache, getCache, runTask, runTasks, runTasks_,
	readDB,

	scanModules, scanFile, scanFileContents, scanCabal, prepareSandbox, scanSandbox, scanPackageDb, scanProjectFile, scanProjectStack, scanProject, scanDirectory, scanContents,
	scanDocs, inferModTypes,
	scan,
	updateEvent, processEvent,

	module HsDev.Database.Update.Types,

	module HsDev.Watcher,

	module Control.Monad.Except
	) where

import Control.Applicative ((<|>))
import Control.Concurrent.Lifted (fork)
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
import Data.Foldable (toList)
import Data.List (find, intercalate, (\\))
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import qualified Data.Text as T (unpack)
import qualified Language.Haskell.Exts as H
import qualified Language.Haskell.Names as N
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath
import qualified System.Log.Simple as Log

import HsDev.Error
import HsDev.Database
import HsDev.Database.Async hiding (Event)
import HsDev.Display
import HsDev.Inspect
import HsDev.Inspect.Order
import HsDev.PackageDb
import HsDev.Project
import HsDev.Sandbox
import HsDev.Stack
import HsDev.Symbols
import HsDev.Symbols.Resolve
import HsDev.Symbols.Util
import HsDev.Tools.Ghc.Session hiding (wait, evaluate)
import HsDev.Tools.Ghc.Types (inferTypes)
import HsDev.Tools.HDocs
import qualified HsDev.Scan as S
import HsDev.Scan.Browse
import HsDev.Util (isParent, ordNub)
import qualified HsDev.Util as Util (withCurrentDirectory)
import HsDev.Server.Types (commandNotify, serverWriteCache, serverReadCache)
import HsDev.Server.Message
import HsDev.Database.Update.Types
import HsDev.Watcher
import Text.Format

onStatus :: UpdateMonad m => m ()
onStatus = asks (view updateTasks) >>= commandNotify . Notification . toJSON . reverse

childTask :: UpdateMonad m => Task -> m a -> m a
childTask t = local (over updateTasks (t:))

isStatus :: Value -> Bool
isStatus = isJust . parseMaybe (parseJSON :: Value -> Parser Task)

runUpdate :: ServerMonadBase m => UpdateOptions -> UpdateM m a -> ClientM m a
runUpdate uopts act = Log.scope "update" $ do
	(r, updatedMods) <- runWriterT (runUpdateM act' `runReaderT` uopts)
	db <- askSession sessionDatabase
	wait db
	dbval <- liftIO $ readAsync db
	-- TODO: serverWriteCache modifiedDb
	return r
	where
		act' = do
			(r, mlocs') <- listen act
			db <- askSession sessionDatabase
			wait db
			dbval <- liftIO $ readAsync db
			let
				invertedIndex = M.unionsWith S.union $ do
					(pdb, pkgs) <- M.toList $ view databasePackageDbs dbval
					pkg <- S.toList pkgs
					mlocs <- dbval ^.. databasePackages . ix pkg
					mloc <- S.toList mlocs
					return $ M.singleton mloc (S.singleton pdb)
				dbs = S.toList $ S.unions $ mapMaybe (`M.lookup` invertedIndex) mlocs'
				-- If some sourced files depends on currently scanned package-dbs
				-- We must resolve them and even rescan if there was errors scanning without
				-- dependencies provided (lack of fixities can cause errors inspecting files)
				sboxes = databaseSandboxes dbval
				sboxOf :: FilePath -> Maybe Sandbox
				sboxOf fpath = find (pathInSandbox fpath) sboxes
				sboxUpdated s = s `elem` map packageDbSandbox dbs

				projs = do
					proj <- dbval ^.. databaseProjects . each
					guard $ sboxUpdated $ sboxOf (proj ^. projectPath)
					guard $ any (`notElem` mlocs') (dbval ^.. projectSlice proj . modules . moduleId . moduleLocation)
					return proj
				stands = do
					sloc <- dbval ^.. standaloneSlice . modules . moduleId . moduleLocation
					guard $ sboxUpdated $ sboxOf (sloc ^?! moduleFile)
					guard (notElem sloc mlocs')
					return (sloc, dbval ^.. databaseModules . ix sloc . inspection . inspectionOpts . each, Nothing)
			Log.log Log.Trace $ "updated package-dbs: {}, have to rescan {} projects and {} files"
				~~ intercalate ", " (map display dbs)
				~~ length projs ~~ length stands
			(_, rlocs') <- listen $ runTasks_ (scanModules [] stands : [scanProject [] (proj ^. projectCabal) | proj <- projs])
			let
				ulocs' = filter (isJust . preview moduleFile) (ordNub $ mlocs' ++ rlocs')
				getMods :: (MonadIO m) => m [InspectedModule]
				getMods = do
					db' <- liftIO $ readAsync db
					return $ filter ((`elem` ulocs') . view inspectedKey) $ toList $ view databaseModules db'
			when (view updateDocs uopts) $ do
				Log.log Log.Trace "forking inspecting source docs"
				void $ fork (getMods >>= waiter . mapM_ scanDocs_)
			when (view updateInfer uopts) $ do
				Log.log Log.Trace "forking inferring types"
				void $ fork (getMods >>= waiter . mapM_ inferModTypes_)
			return r
		scanDocs_ :: UpdateMonad m => InspectedModule -> m ()
		scanDocs_ im = do
			im' <- (S.scanModify (\opts -> liftIO . inspectDocs opts) im) <|> return im
			updater $ fromModule im'
		inferModTypes_ :: UpdateMonad m => InspectedModule -> m ()
		inferModTypes_ im = do
			-- TODO: locate sandbox
			s <- getSession
			im' <- (S.scanModify (infer' s) im) <|> return im
			updater $ fromModule im'
		infer' :: UpdateMonad m => Session -> [String] -> Module -> m Module
		infer' s opts m = case preview (moduleId . moduleLocation . moduleFile) m of
			Nothing -> return m
			Just _ -> liftIO $ inWorker (sessionGhc s) $ do
				targetSession opts m
				inferTypes opts m Nothing

-- | Post status
postStatus :: UpdateMonad m => Task -> m ()
postStatus t = childTask t onStatus

-- | Wait DB to complete actions
waiter :: UpdateMonad m => m () -> m ()
waiter act = do
	db <- askSession sessionDatabase
	act
	wait db

-- | Update task result to database
updater :: UpdateMonad m => Database -> m ()
updater db' = do
	db <- askSession sessionDatabase
	update db $ return $!! db'
	tell $!! db' ^.. modules . moduleId . moduleLocation

-- | Clear obsolete data from database
cleaner :: UpdateMonad m => m Database -> m ()
cleaner act = do
	db <- askSession sessionDatabase
	db' <- act
	clear db $ return $!! db'

-- | Get data from cache without updating DB
loadCache :: UpdateMonad m => (FilePath -> ExceptT String IO Database) -> m Database
loadCache act = do
	mdat <- serverReadCache act
	return $ fromMaybe mempty mdat

-- | Load data from cache if not loaded yet and wait
getCache :: UpdateMonad m => (FilePath -> ExceptT String IO Database) -> (Database -> Database) -> m Database
getCache act check = do
	dbval <- liftM check readDB
	if nullDatabase dbval
		then do
			db <- loadCache act
			waiter $ updater db
			return db
		else
			return dbval

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
		setProgress = set (updateTasks . _head . taskProgress) (Just (Progress n total))
	noErr v = hsdevIgnore Nothing (Just <$> v)

-- | Run many tasks with numeration
runTasks_ :: UpdateMonad m => [m ()] -> m ()
runTasks_ = void . runTasks

-- | Get database value
readDB :: SessionMonad m => m Database
readDB = askSession sessionDatabase >>= liftIO . readAsync

-- | Scan modules
scanModules :: UpdateMonad m => [String] -> [S.ModuleToScan] -> m ()
scanModules opts ms = mapM_ (uncurry scanModules') grouped where
	scanModules' mproj ms' = do
		dbval <- readDB
		defines <- askSession sessionDefines
		-- Make table of already scanned and up to date modules
		let
			scanned' (_, _, Just _) = return Nothing
			scanned' (mloc, mopts, Nothing) = case dbval ^? databaseModules . ix mloc of
				Nothing -> return Nothing
				Just im -> do
					up <- S.upToDate (opts ++ mopts) im
					return $ do
						guard (up && not (hasTag OnlyHeaderTag im))
						im ^? inspected
			toMap = M.fromList . map (\m' -> (m' ^. moduleId . moduleLocation, m'))
		alreadyScanned <- liftIO $ liftM (toMap . catMaybes) $ traverse scanned' ms
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
		db' <- fmap mconcat $ forM (map fst $ filter snd ploaded) $ \p -> do
			let
				mloc = p ^. preloadedId . moduleLocation
			insp <- liftIO $ inspectionInfos ^?! ix mloc
			return $ fromModule $ Inspected insp mloc (tag OnlyHeaderTag) $ Right $ p ^. asModule
		updater db'
		let
			pmods = map fst ploaded
			dbval' = dbval ^. maybe (slice installed) projectSlice mproj
			deps' = sourceDeps dbval'
			dependencies = ordNub (concat [fromMaybe [] (deps' ^? ix f) | f <- ms ^.. each . _1 . moduleFile]) \\ (ms ^.. each . _1 . moduleFile)
			mods' = dbval ^.. unionSlice globalDeps localDeps . modules where
				globalDeps = maybe (slice installed) projectDepsSlice mproj
				localDeps = slice (\m -> or [inFile d m | d <- dependencies])
			aenv' = mconcat (map moduleAnalyzeEnv mods')
		case order pmods of
			Left err -> Log.log Log.Error ("failed order dependencies for files: {}" ~~ show err)
			Right ordered -> do
				ms'' <- flip evalStateT aenv' $ runTasks (map inspect' ordered)
				db'' <- fmap mconcat $ forM ms'' $ \m -> do
					let
						mloc = m ^. moduleId . moduleLocation
					insp <- liftIO $ inspectionInfos ^?! ix mloc
					return $ fromModule $ Inspected insp mloc mempty (Right m)
				updater db''
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
scanFile :: UpdateMonad m => [String] -> FilePath -> m ()
scanFile opts fpath = scanFileContents opts fpath Nothing

-- | Scan source file with contents and resolve dependent modules
scanFileContents :: UpdateMonad m => [String] -> FilePath -> Maybe String -> m ()
scanFileContents opts fpath mcts = runTask "scanning" fpath $ Log.scope "file" $ hsdevLiftIO $ do
	dbval <- readDB
	fpath' <- liftIO $ canonicalizePath fpath
	ex <- liftIO $ doesFileExist fpath'
	when (not ex) $ hsdevError $ FileNotFound fpath
	mloc <- case dbval ^? modules . filtered (inFile fpath') of
		Just m -> return $ view (moduleId . moduleLocation) m
		Nothing -> do
			mproj <- locateProjectInfo fpath'
			return $ FileModule fpath' mproj
	watch $ flip watchModule mloc
	S.ScanContents dmods _ _ <- S.enumDependent fpath'
	scanModules [] ((mloc, opts, mcts) : dmods)

-- | Scan cabal modules, doesn't rescan if already scanned
scanCabal :: UpdateMonad m => [String] -> m ()
scanCabal opts = Log.scope "cabal" $ scanPackageDbStack opts userDb

-- | Prepare sandbox for scanning. This is used for stack project to build & configure.
prepareSandbox :: UpdateMonad m => Sandbox -> m ()
prepareSandbox sbox@(Sandbox StackWork fpath) = Log.scope "prepare" $ runTasks_ [
	runTask "building dependencies" sbox $ void $ Util.withCurrentDirectory dir $ buildDeps Nothing,
	runTask "configuring" sbox $ void $ Util.withCurrentDirectory dir $ configure Nothing]
	where
		dir = takeDirectory fpath
prepareSandbox _ = return ()

-- | Scan sandbox modules, doesn't rescan if already scanned
scanSandbox :: UpdateMonad m => [String] -> Sandbox -> m ()
scanSandbox opts sbox = Log.scope "sandbox" $ do
	dbval <- readDB
	prepareSandbox sbox
	pdbs <- sandboxPackageDbStack sbox
	scanPackageDbStack opts pdbs

-- | Scan top of package-db stack, usable for rescan
scanPackageDb :: UpdateMonad m => [String] -> PackageDbStack -> m ()
scanPackageDb opts pdbs = runTask "scanning" (topPackageDb pdbs) $ Log.scope "package-db" $ do
	pdbState <- liftIO $ readPackageDb (topPackageDb pdbs)
	let
		packageDbMods = S.fromList $ concat $ M.elems pdbState
	watch (\w -> watchPackageDb w pdbs opts)
	dbval <- readDB
	ghcw <- askSession sessionGhc
	mlocs <- liftM
		(filter (`S.member` packageDbMods)) $
		(liftIO $ inWorker ghcw $ listModules opts pdbs)
	scan (const $ return mempty) (packageDbSlice (topPackageDb pdbs)) ((,,) <$> mlocs <*> pure [] <*> pure Nothing) opts $ \mlocs' -> do
		ms <- liftIO $ inWorker ghcw $ browseModules opts pdbs (mlocs' ^.. each . _1)
		docs <- liftIO $ hsdevLiftWith (ToolError "hdocs") $ hdocsCabal pdbs opts
		updater $ mconcat $ map (fromModule . fmap (setDocs' docs)) ms
	where
		setDocs' :: Map String (Map String String) -> Module -> Module
		setDocs' docs m = maybe m (`setDocs` m) $ M.lookup (T.unpack $ view (moduleId . moduleName) m) docs

-- | Scan top of package-db stack, usable for rescan
scanPackageDbStack :: UpdateMonad m => [String] -> PackageDbStack -> m ()
scanPackageDbStack opts pdbs = runTask "scanning" (topPackageDb pdbs) $ Log.scope "package-db" $ do
	pdbStates <- liftIO $ mapM readPackageDb (packageDbs pdbs)
	let
		packageDbMods = S.fromList $ concat $ concatMap M.elems pdbStates
	watch (\w -> watchPackageDbStack w pdbs opts)
	dbval <- readDB
	ghcw <- askSession sessionGhc
	mlocs <- liftM
		(filter (`S.member` packageDbMods)) $
		(liftIO $ inWorker ghcw $ listModules opts pdbs)
	scan (const $ return mempty) (packageDbStackSlice pdbs) ((,,) <$> mlocs <*> pure [] <*> pure Nothing) opts $ \mlocs' -> do
		ms <- liftIO $ inWorker ghcw $ browseModules opts pdbs (mlocs' ^.. each . _1)
		docs <- liftIO $ hsdevLiftWith (ToolError "hdocs") $ hdocsCabal pdbs opts
		updater $ mconcat $ map (fromModule . fmap (setDocs' docs)) ms
	where
		setDocs' :: Map String (Map String String) -> Module -> Module
		setDocs' docs m = maybe m (`setDocs` m) $ M.lookup (T.unpack $ view (moduleId . moduleName) m) docs

-- | Scan project file
scanProjectFile :: UpdateMonad m => [String] -> FilePath -> m Project
scanProjectFile opts cabal = runTask "scanning" cabal $ do
	proj <- S.scanProjectFile opts cabal
	updater $ fromProject proj
	return proj

-- | Refine project info and update if necessary
refineProjectInfo :: UpdateMonad m => Project -> m Project
refineProjectInfo proj = do
	dbval <- readDB
	case refineProject dbval proj of
		Nothing -> runTask "scanning" (proj ^. projectCabal) $ do
			proj' <- liftIO $ loadProject proj
			updater $ fromProject proj'
			return proj'
		Just proj' -> return proj'

-- | Get project info for module
locateProjectInfo :: UpdateMonad m => FilePath -> m (Maybe Project)
locateProjectInfo cabal = liftIO (locateProject cabal) >>= traverse refineProjectInfo

-- | Scan project and related package-db stack
scanProjectStack :: UpdateMonad m => [String] -> FilePath -> m ()
scanProjectStack opts cabal = do
	proj <- scanProjectFile opts cabal
	scanProject opts cabal
	sbox <- liftIO $ searchSandbox (view projectPath proj)
	maybe (scanCabal opts) (scanSandbox opts) sbox

-- | Scan project
scanProject :: UpdateMonad m => [String] -> FilePath -> m ()
scanProject opts cabal = runTask "scanning" (project cabal) $ Log.scope "project" $ do
	proj <- scanProjectFile opts cabal
	watch (\w -> watchProject w proj opts)
	S.ScanContents _ [(_, sources)] _ <- S.enumProject proj
	scanModules opts sources

-- | Scan directory for source files and projects
scanDirectory :: UpdateMonad m => [String] -> FilePath -> m ()
scanDirectory opts dir = runTask "scanning" dir $ Log.scope "directory" $ do
	S.ScanContents standSrcs projSrcs pdbss <- S.enumDirectory dir
	runTasks_ [scanProject opts (view projectCabal p) | (p, _) <- projSrcs]
	runTasks_ $ map (scanPackageDb opts) pdbss -- TODO: Don't rescan
	mapMOf_ (each . _1) (watch . flip watchModule) standSrcs
	scan (const $ return mempty) (standaloneSlice . slice inDir) standSrcs opts $ scanModules opts
	where
		inDir = maybe False (dir `isParent`) . preview (sourcedModule . moduleLocation . moduleFile)

scanContents :: UpdateMonad m => [String] -> S.ScanContents -> m ()
scanContents opts (S.ScanContents standSrcs projSrcs pdbss) = do
	dbval <- readDB
	let
		projs = dbval ^.. databaseProjects . each . projectCabal
		pdbs = M.keys (dbval ^. databasePackageDbs)
		files = dbval ^.. standaloneSlice . modules . moduleId . moduleLocation . moduleFile
		srcs = standSrcs ^.. each . _1 . moduleFile
		inSrcs src = src `elem` srcs && src `notElem` files
		inFiles = maybe False inSrcs . preview (sourcedModule . moduleLocation . moduleFile)
	runTasks_ [scanPackageDb opts pdbs' | pdbs' <- pdbss, topPackageDb pdbs' `notElem` pdbs]
	runTasks_ [scanProject opts (view projectCabal p) | (p, _) <- projSrcs, view projectCabal p `notElem` projs]
	mapMOf_ (each . _1) (watch . flip watchModule) standSrcs
	scan (const $ return mempty) (standaloneSlice . slice inFiles) standSrcs opts $ scanModules opts

-- | Scan docs for inspected modules
scanDocs :: UpdateMonad m => [InspectedModule] -> m ()
scanDocs ims = do
	-- w <- liftIO $ ghcWorker ["-haddock"] (return ())
	-- w <- askSession sessionGhc
	runTasks_ $ map scanDocs' ims
	where
		scanDocs' im
			| not $ hasTag RefinedDocsTag im = runTask "scanning docs" (view inspectedKey im) $ Log.scope "docs" $ do
				Log.log Log.Trace $ "Scanning docs for {}" ~~  view inspectedKey im
				im' <- (liftM (setTag RefinedDocsTag) $ S.scanModify doScan im)
					<|> return im
				Log.log Log.Trace $ "Docs for {} updated: documented {} declarations" ~~
					view inspectedKey im' ~~
					length (im' ^.. inspectionResult . _Right . moduleSymbols . symbolDocs . _Just)
				updater $ fromModule im'
			| otherwise = Log.log Log.Trace $ "Docs for {} already scanned" ~~ view inspectedKey im
		doScan _ m = do
			w <- askSession sessionGhc
			liftIO $ inWorker w $ do
				opts' <- getModuleOpts [] m
				haddockSession opts'
				liftGhc $ inspectDocsGhc opts' m

inferModTypes :: UpdateMonad m => [InspectedModule] -> m ()
inferModTypes = runTasks_ . map inferModTypes' where
	inferModTypes' im
		| not $ hasTag InferredTypesTag im = runTask "inferring types" (view inspectedKey im) $ Log.scope "docs" $ do
			w <- askSession sessionGhc
			Log.log Log.Trace $ "Inferring types for {}" ~~ view inspectedKey im
			im' <- (liftM (setTag InferredTypesTag) $
				S.scanModify (\opts m -> liftIO (inWorker w (targetSession opts m >> inferTypes opts m Nothing))) im)
				<|> return im
			Log.log Log.Trace $ "Types for {} inferred" ~~ view inspectedKey im
			updater $ fromModule im'
		| otherwise = Log.log Log.Trace $ "Types for {} already inferred" ~~ view inspectedKey im

-- | Generic scan function. Reads cache only if data is not already loaded, removes obsolete modules and rescans changed modules.
scan :: UpdateMonad m
	=> (FilePath -> ExceptT String IO Database)
	-- ^ Read data from cache
	-> Slice
	-- ^ Get data from database
	-> [S.ModuleToScan]
	-- ^ Actual modules. Other modules will be removed from database
	-> [String]
	-- ^ Extra scan options
	-> ([S.ModuleToScan] -> m ())
	-- ^ Function to update changed modules
	-> m ()
scan cache' part' mlocs opts act = Log.scope "scan" $ do
	dbval <- getCache cache' (view part')
	let
		obsolete = dbval ^. slice (\m -> view moduleLocation m `notElem` (mlocs ^.. each . _1))
	changed <- liftIO $ S.changedModules dbval opts mlocs
	cleaner $ return obsolete
	act changed

updateEvent :: ServerMonadBase m => Watched -> Event -> UpdateM m ()
updateEvent (WatchedProject proj projOpts) e
	| isSource e = do
		Log.log Log.Info $ "File '{file}' in project {proj} changed"
			~~ ("file" ~% view eventPath e)
			~~ ("proj" ~% view projectName proj)
		dbval <- readDB
		let
			opts = dbval ^.. databaseModules . each . filtered (maybe False (inFile (view eventPath e)) . preview inspected) . inspection . inspectionOpts . each
		scanFile opts $ view eventPath e
	| isCabal e = do
		Log.log Log.Info $ "Project {proj} changed"
			~~ ("proj" ~% view projectName proj)
		scanProject projOpts $ view projectCabal proj
	| otherwise = return ()
updateEvent (WatchedPackageDb pdbs opts) e
	| isConf e = do
		Log.log Log.Info $ "Package db {package} changed"
			~~ ("package" ~% topPackageDb pdbs)
		scanPackageDb opts pdbs
	| otherwise = return ()
updateEvent WatchedModule e
	| isSource e = do
		Log.log Log.Info $ "Module {file} changed"
			~~ ("file" ~% view eventPath e)
		dbval <- readDB
		let
			opts = dbval ^.. databaseModules . each . filtered (maybe False (inFile (view eventPath e)) . preview inspected) . inspection . inspectionOpts . each
		scanFile opts $ view eventPath e
	| otherwise = return ()

processEvent :: UpdateOptions -> Watched -> Event -> ClientM IO ()
processEvent uopts w e = runUpdate uopts $ updateEvent w e

watch :: SessionMonad m => (Watcher -> IO ()) -> m ()
watch f = do
	w <- askSession sessionWatcher
	liftIO $ f w
