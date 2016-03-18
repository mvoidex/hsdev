{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module HsDev.Client.Commands (
	runClient, runCommand
	) where

import Control.Applicative
import Control.Arrow
import Control.Concurrent.MVar
import Control.Exception (displayException)
import Control.Lens (view, preview, _Just)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import qualified Control.Monad.State as State
import Control.Monad.Catch (try, SomeException(..))
import Data.Aeson hiding (Result, Error)
import Data.List
import Data.Foldable (toList)
import Data.Maybe
import qualified Data.Map as M
import Data.String (fromString)
import Data.Text (unpack)
import qualified Data.Text as T (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Text.Lens (packed)
import System.Directory
import System.FilePath
import qualified System.Log.Simple as Log
import Text.Regex.PCRE ((=~))

import qualified System.Directory.Watcher as W
import qualified Data.Async as A
import System.Directory.Paths
import Text.Format
import HsDev.Cache
import HsDev.Commands
import qualified HsDev.Database.Async as DB
import HsDev.Server.Message as M
import HsDev.Server.Types
import HsDev.Sandbox hiding (findSandbox)
import qualified HsDev.Sandbox as S (findSandbox)
import HsDev.Symbols
import HsDev.Symbols.Resolve (resolveOne, scopeModule, exportsModule)
import HsDev.Symbols.Util
import qualified HsDev.Tools.AutoFix as AutoFix
import qualified HsDev.Tools.Cabal as Cabal
import HsDev.Tools.Ghc.Worker
import qualified HsDev.Tools.Ghc.Check as Check
import qualified HsDev.Tools.Ghc.Types as Types
import qualified HsDev.Tools.GhcMod as GhcMod
import qualified HsDev.Tools.Hayoo as Hayoo
import qualified HsDev.Tools.HLint as HLint
import qualified HsDev.Tools.Types as Tools
import HsDev.Util
import HsDev.Watcher

import qualified HsDev.Scan.Browse as Scan
import qualified HsDev.Database.Update as Update

runClient :: (ToJSON a, ServerMonadBase m) => CommandOptions -> ClientM m a -> ServerM m Result
runClient copts = mapServerM toResult . runClientM where
	toResult :: (ToJSON a, ServerMonadBase m) => ExceptT CommandError (ReaderT CommandOptions m) a -> m Result
	toResult act = liftM asResult $ runReaderT (runExceptT act) copts
	asResult :: ToJSON a => Either CommandError a -> Result
	asResult (Left (CommandError e ds)) = Error e $ M.fromList $ map (first unpack) ds
	asResult (Right r') = Result $ toJSON r'
	mapServerM :: (Monad m, Monad n) => (m a -> n b) -> ServerM m a -> ServerM n b
	mapServerM f = ServerM . mapReaderT f . runServerM

toValue :: (ToJSON a, Monad m) => m a -> m Value
toValue = liftM toJSON

runCommand :: ServerMonadBase m => Command -> ClientM m Value
runCommand Ping = toValue $ return $ object ["message" .= ("pong" :: String)]
runCommand Listen = toValue $ do
	serverListen >>= mapM_ (\msg -> commandNotify (Notification $ object ["message" .= msg]))
runCommand (AddData cts) = toValue $ mapM_ updateData cts where
	updateData (AddedDatabase db) = toValue $ serverUpdateDB db
	updateData (AddedModule m) = toValue $ serverUpdateDB $ fromModule m
	updateData (AddedProject p) = toValue $ serverUpdateDB $ fromProject p
runCommand (Scan projs cabal sboxes fs paths' fcts ghcs' docs' infer') = toValue $ do
	sboxes' <- getSandboxes sboxes
	updateProcess (Update.UpdateOptions [] ghcs' docs' infer') $ concat [
		map (\(FileContents f cts) -> Update.scanFileContents ghcs' f (Just cts)) fcts,
		map (Update.scanProject ghcs') projs,
		map (Update.scanFile ghcs') fs,
		map (Update.scanDirectory ghcs') paths',
		[Update.scanCabal ghcs' | cabal],
		map (Update.scanSandbox ghcs') sboxes']
runCommand (RefineDocs projs fs ms) = toValue $ do
	projects <- traverse findProject projs
	dbval <- getDb
	let
		filters = anyOf $ concat [
			map inProject projects,
			map inFile fs,
			map inModule ms]
		mods = selectModules (filters . view moduleId) dbval
	updateProcess (Update.UpdateOptions [] [] False False) [Update.scanDocs $ map (getInspected dbval) mods]
runCommand (InferTypes projs fs ms) = toValue $ do
	projects <- traverse findProject projs
	dbval <- getDb
	let
		filters = anyOf $ concat [
			map inProject projects,
			map inFile fs,
			map inModule ms]
		mods = selectModules (filters . view moduleId) dbval
	updateProcess (Update.UpdateOptions [] [] False False) [Update.inferModTypes $ map (getInspected dbval) mods]
runCommand (Remove projs cabal sboxes files) = toValue $ do
	db <- askSession sessionDatabase
	dbval <- getDb
	w <- askSession sessionWatcher
	projects <- traverse findProject projs
	sboxes' <- getSandboxes sboxes
	forM_ projects $ \proj -> do
		DB.clear db (return $ projectDB proj dbval)
		liftIO $ unwatchProject w proj
	dbPDbs <- liftIO $ mapM restorePackageDbStack $ databasePackageDbs dbval
	flip State.evalStateT dbPDbs $ do
		when cabal $ removePackageDbStack userDb
		forM_ sboxes' $ \sbox -> do
			pdbs <- lift $ mapCommandIO $ sandboxPackageDbStack sbox
			removePackageDbStack pdbs
	forM_ files $ \file -> do
		DB.clear db (return $ filterDB (inFile file) (const False) dbval)
		let
			mloc = fmap (view moduleLocation) $ lookupFile file dbval
		maybe (return ()) (liftIO . unwatchModule w) mloc
	where
		-- We can safely remove package-db from db iff doesn't used by some of other package-dbs
		-- For example, we can't remove global-db if there are any other package-dbs, because all of them uses global-db
		-- We also can't remove stack snapshot package-db if there are some local package-db not yet removed
		canRemove pdbs = do
			from <- State.get
			return $ null $ filter (pdbs `isSubStack`) $ delete pdbs from
		-- Remove top of package-db stack if possible
		removePackageDb pdbs = do
			db <- lift $ askSession sessionDatabase
			dbval <- lift getDb
			w <- lift $ askSession sessionWatcher
			can <- canRemove pdbs
			when can $ do
				State.modify (delete pdbs)
				DB.clear db (return $ packageDbDB (topPackageDb pdbs) dbval)
				liftIO $ unwatchPackageDb w $ topPackageDb pdbs
		-- Remove package-db stack when possible
		removePackageDbStack = mapM_ removePackageDb . packageDbStacks
runCommand RemoveAll = toValue $ do
	db <- askSession sessionDatabase
	liftIO $ A.modifyAsync db A.Clear
	w <- askSession sessionWatcher
	wdirs <- liftIO $ readMVar (W.watcherDirs w)
	liftIO $ forM_ (M.toList wdirs) $ \(dir, (isTree, _)) -> (if isTree then W.unwatchTree else W.unwatchDir) w dir
runCommand (InfoModules fs) = toValue $ do
	dbval <- getDb
	filter' <- targetFilters fs
	return $ map (view moduleId) $ newestPackage $ selectModules (filter' . view moduleId) dbval
runCommand InfoPackages = toValue $ (ordNub . sort . 	mapMaybe (preview (moduleLocation . modulePackage . _Just)) . allModules) <$> getDb
runCommand InfoProjects = toValue $ (toList . databaseProjects) <$> getDb
runCommand InfoSandboxes = toValue $ databasePackageDbs <$> getDb
runCommand (InfoSymbol sq fs locals') = toValue $ do
	dbval <- liftM (localsDatabase locals') $ getDb
	filter' <- targetFilters fs
	return $ newestPackage $ filterMatch sq $
		concatMap moduleModuleDeclarations $ filter (filter' . view moduleId) $ allModules dbval
runCommand (InfoModule sq fs) = toValue $ do
	dbval <- getDb
	filter' <- targetFilters fs
	return $ newestPackage $ filterMatch sq $ filter (filter' . view moduleId) $ allModules dbval
runCommand (InfoResolve fpath exports) = toValue $ do
	dbval <- getSDb fpath
	let
		getScope
			| exports = exportsModule
			| otherwise = scopeModule
	case lookupFile fpath dbval of
		Nothing -> commandError "File not found" []
		Just m -> return $ getScope $ resolveOne dbval m
runCommand (InfoProject (Left projName)) = toValue $ findProject projName
runCommand (InfoProject (Right projPath)) = toValue $ liftIO $ searchProject projPath
runCommand (InfoSandbox sandbox') = toValue $ liftIO $ searchSandbox sandbox'
runCommand (Lookup nm fpath) = toValue $ do
	dbval <- getSDb fpath
	mapCommandIO $ lookupSymbol dbval fpath nm
runCommand (Whois nm fpath) = toValue $ do
	dbval <- getSDb fpath
	mapCommandIO $ whois dbval fpath nm
runCommand (ResolveScopeModules sq fpath) = toValue $ do
	dbval <- getSDb fpath
	liftM (filterMatch sq . map (view moduleId)) $ mapCommandIO $ scopeModules dbval fpath
runCommand (ResolveScope sq global fpath) = toValue $ do
	dbval <- getSDb fpath
	liftM (filterMatch sq) $ mapCommandIO $ scope dbval fpath global
runCommand (Complete input wide fpath) = toValue $ do
	dbval <- getSDb fpath
	mapCommandIO $ completions dbval fpath input wide
runCommand (Hayoo hq p ps) = toValue $ liftM concat $ forM [p .. p + pred ps] $ \i -> liftM
	(mapMaybe Hayoo.hayooAsDeclaration . Hayoo.resultResult) $
	mapCommandIO $ Hayoo.hayoo hq (Just i)
runCommand (CabalList packages) = toValue $ mapCommandIO $ Cabal.cabalList packages
runCommand (Lint fs fcts) = toValue $ do
	mapCommandIO $ liftM2 (++)
		(liftM concat $ mapM HLint.hlintFile fs)
		(liftM concat $ mapM (\(FileContents f c) -> HLint.hlintSource f c) fcts)
runCommand (Check fs fcts ghcs') = toValue $ Log.scope "check" $ do
	dbval <- getDb
	ghc <- askSession sessionGhc
	liftIO $ restartWorker ghc
	let
		checkSome file fn = Log.scope "checkSome" $ do
			pdbs <- liftIO $ searchPackageDbStack file
			m <- maybe
				(commandError_ $ "File '{}' not found" ~~ file)
				return
				(lookupFile file dbval)
			notes <- inWorkerWith (commandError_ . show) ghc
				(runExceptT $ fn pdbs m)
			either commandError_ return notes
	liftM concat $ mapM (uncurry checkSome) $
		[(f, Check.checkFile ghcs') | f <- fs] ++
		[(f, \pdbs m -> Check.checkSource ghcs' pdbs m src) | FileContents f src <- fcts]
runCommand (CheckLint fs fcts ghcs') = toValue $ do
	dbval <- getDb
	ghc <- askSession sessionGhc
	liftIO $ restartWorker ghc
	let
		checkSome file fn = do
			pdbs <- liftIO $ searchPackageDbStack file
			m <- maybe
				(commandError_ $ "File '" ++ file ++ "' not found")
				return
				(lookupFile file dbval)
			notes <- inWorkerWith (commandError_ . show) ghc
				(runExceptT $ fn pdbs m)
			either commandError_ return notes
	checkMsgs <- liftM concat $ mapM (uncurry checkSome) $
		[(f, Check.checkFile ghcs') | f <- fs] ++
		[(f, \pdbs m -> Check.checkSource ghcs' pdbs m src) | FileContents f src <- fcts]
	lintMsgs <- mapCommandIO $ liftM2 (++)
		(liftM concat $ mapM HLint.hlintFile fs)
		(liftM concat $ mapM (\(FileContents f src) -> HLint.hlintSource f src) fcts)
	return $ checkMsgs ++ lintMsgs
runCommand (Types fs fcts ghcs') = toValue $ do
	dbval <- getDb
	ghc <- askSession sessionGhc
	let
		cts = [(f, Nothing) | f <- fs] ++ [(f, Just src) | FileContents f src <- fcts]
	liftM concat $ forM cts $ \(file, msrc) -> do
		pdbs <- liftIO $ searchPackageDbStack file
		m <- maybe
			(commandError_ $ "File '" ++ file ++ "' not found")
			return
			(lookupFile file dbval)
		notes <- inWorkerWith (commandError_ . show) ghc
			(runExceptT $ Types.fileTypes ghcs' pdbs m msrc)
		either commandError_ return notes
runCommand (GhcMod GhcModLang) = toValue $ mapCommandIO GhcMod.langs
runCommand (GhcMod GhcModFlags) = toValue $ mapCommandIO GhcMod.flags
runCommand (GhcMod (GhcModType (Position line column) fpath ghcs')) = toValue $ do
	ghcmod <- askSession sessionGhcMod
	dbval <- getDb
	pdbs <- liftIO $ searchPackageDbStack fpath
	pkgs <- mapCommandIO $ Scan.browsePackages ghcs' pdbs
	(fpath', m', _) <- mapCommandIO $ fileCtx dbval fpath
	mapCommandIO $ GhcMod.waitMultiGhcMod ghcmod fpath' $
		GhcMod.typeOf (ghcs' ++ moduleOpts pkgs m') pdbs fpath' line column
runCommand (GhcMod (GhcModLint fs hlints')) = toValue $ do
	ghcmod <- askSession sessionGhcMod
	mapCommandIO $ liftM concat $ forM fs $ \file ->
		GhcMod.waitMultiGhcMod ghcmod file $
			GhcMod.lint hlints' file
runCommand (GhcMod (GhcModCheck fs ghcs')) = toValue $ do
	ghcmod <- askSession sessionGhcMod
	dbval <- getDb
	mapCommandIO $ liftM concat $ forM fs $ \file -> do
		mproj <- liftIO $ locateProject file
		pdbs <- liftIO $ searchPackageDbStack file
		pkgs <- Scan.browsePackages ghcs' pdbs
		(_, m', _) <- fileCtx dbval file
		GhcMod.waitMultiGhcMod ghcmod file $
			GhcMod.check (ghcs' ++ moduleOpts pkgs m') pdbs [file] mproj
runCommand (GhcMod (GhcModCheckLint fs ghcs' hlints')) = toValue $ do
	ghcmod <- askSession sessionGhcMod
	dbval <- getDb
	mapCommandIO $ liftM concat $ forM fs $ \file -> do
		mproj <- liftIO $ locateProject file
		pdbs <- liftIO $ searchPackageDbStack file
		pkgs <- Scan.browsePackages ghcs' pdbs
		(_, m', _) <- fileCtx dbval file
		GhcMod.waitMultiGhcMod ghcmod file $ do
			checked <- GhcMod.check (ghcs' ++ moduleOpts pkgs m') pdbs [file] mproj
			linted <- GhcMod.lint hlints' file
			return $ checked ++ linted
runCommand (AutoFix (AutoFixShow ns)) = toValue $ return $ AutoFix.corrections ns
runCommand (AutoFix (AutoFixFix ns rest isPure)) = toValue $ do
	files <- liftM (ordNub . sort) $ mapM findPath $ mapMaybe (preview $ Tools.noteSource . moduleFile) ns
	let
		doFix :: FilePath -> String -> ([Tools.Note AutoFix.Correction], String)
		doFix file cts = AutoFix.edit cts fUpCorrs $ do
			AutoFix.autoFix fCorrs
			State.gets (view AutoFix.regions)
			where
				findCorrs :: FilePath -> [Tools.Note AutoFix.Correction] -> [Tools.Note AutoFix.Correction]
				findCorrs f = filter ((== Just f) . preview (Tools.noteSource . moduleFile))
				fCorrs = map (view Tools.note) $ findCorrs file ns
				fUpCorrs = findCorrs file rest
		runFix file
			| isPure = return $ fst $ doFix file ""
			| otherwise = do
				(corrs', cts') <- liftM (doFix file) $ liftE $ readFileUtf8 file
				liftE $ writeFileUtf8 file cts'
				return corrs'
	mapCommandIO $ liftM concat $ mapM runFix files
runCommand (GhcEval exprs) = toValue $ do
	ghci <- askSession sessionGhci
	async' <- liftIO $ pushTask ghci $ mapM (try . evaluate) exprs
	res <- waitAsync async'
	return $ map toValue' res
	where
		waitAsync :: CommandMonad m => Async a -> m a
		waitAsync a = liftIO (waitCatch a) >>= either (commandError_ . displayException) return
		toValue' :: ToJSON a => Either SomeException a -> Value
		toValue' (Left (SomeException e)) = object ["fail" .= show e]
		toValue' (Right s) = toJSON s
runCommand (Link hold) = toValue $ commandLink >> when hold commandHold
runCommand Exit = toValue serverExit

targetFilters :: CommandMonad m => [TargetFilter] -> m (ModuleId -> Bool)
targetFilters fs = do
	fs_ <- mapM targetFilter fs
	return $ foldr (liftM2 (&&)) (const True) fs_

targetFilter :: CommandMonad m => TargetFilter -> m (ModuleId -> Bool)
targetFilter f = case f of
	TargetProject proj -> liftM inProject $ findProject proj
	TargetFile file -> return $ inFile file
	TargetModule mname -> return $ inModule mname
	TargetDepsOf dep -> liftM inDeps $ findDep dep
	TargetPackageDb pdb -> return $ inPackageDb pdb
	TargetCabal -> return $ inPackageDbStack userDb
	TargetSandbox sbox -> liftM inPackageDbStack $ findSandbox sbox >>= mapCommandIO . sandboxPackageDbStack
	TargetPackage pack -> return $ inPackage pack
	TargetSourced -> return byFile
	TargetStandalone -> return standalone

-- Helper functions

-- | Canonicalize paths
findPath :: (CommandMonad m, Paths a) => a -> m a
findPath = paths findPath' where
	findPath' :: CommandMonad m => FilePath -> m FilePath
	findPath' f = do
		r <- commandRoot
		liftIO $ canonicalizePath (normalise $ if isRelative f then r </> f else f)

-- | Find sandbox by path
findSandbox :: (CommandMonad m, Functor m) => FilePath -> m Sandbox
findSandbox fpath = do
	fpath' <- findPath fpath
	sbox <- liftIO $ S.findSandbox fpath'
	maybe
		(commandError ("Sandbox {} not found" ~~ fpath') ["sandbox" .= fpath'])
		return
		sbox

-- | Get list of enumerated sandboxes
getSandboxes :: (CommandMonad m, Functor m) => [FilePath] -> m [Sandbox]
getSandboxes = traverse (findPath >=> findSandbox)

-- | Find project by name or path
findProject :: CommandMonad m => String -> m Project
findProject proj = do
	db' <- getDb
	proj' <- liftM addCabal $ findPath proj
	let
		resultProj =
			refineProject db' (project proj') <|>
			find ((== proj) . view projectName) (databaseProjects db')
	maybe (commandError_ $ "Project {} not found" ~~ proj) return resultProj
	where
		addCabal p
			| takeExtension p == ".cabal" = p
			| otherwise = p </> (takeBaseName p <.> "cabal")

-- | Find dependency: it may be source, project file or project name, also returns sandbox found
findDep :: CommandMonad m => String -> m (Project, Maybe FilePath, PackageDbStack)
findDep depName = do
	depPath <- findPath depName
	proj <- msum [
		do
			p <- liftIO (locateProject depPath)
			p' <- maybe (commandError_ $ "Project {} not found" ~~ depName) return p
			r <- liftIO $ runExceptT $ loadProject p'
			either commandError_ return r,
		findProject depName]
	let
		src
			| takeExtension depPath == ".hs" = Just depPath
			| otherwise = Nothing
	pdbs <- liftIO $ searchPackageDbStack $ view projectPath proj
	return (proj, src, pdbs)

-- FIXME: Doesn't work for file without project
-- | Check if project or source depends from this module
inDeps :: (Project, Maybe FilePath, PackageDbStack) -> ModuleId -> Bool
inDeps (proj, src, pdbs) = liftM2 (&&) (restrictPackageDbStack pdbs) deps' where
	deps' = case src of
		Nothing -> inDepsOfProject proj
		Just src' -> inDepsOfFile proj src'

-- | Bring locals to top scope to search within them if 'locals' flag set
localsDatabase :: Bool -> Database -> Database
localsDatabase True = databaseLocals
localsDatabase False = id

-- | Get actual DB state
getDb :: SessionMonad m => m Database
getDb = askSession sessionDatabase >>= liftIO . DB.readAsync

-- | Get DB with filtered sanxboxes for file
getSDb :: SessionMonad m => FilePath -> m Database
getSDb fpath = do
	dbval <- getDb
	pdbs <- liftIO $ searchPackageDbStack fpath
	return $ filterDB (restrictPackageDbStack pdbs) (const True) dbval

mapCommandIO :: CommandMonad m => ExceptT String IO a -> m a
mapCommandIO act = liftIO (runExceptT act) >>= either commandError_ return

-- | Run DB update action
updateProcess :: ServerMonadBase m => Update.UpdateOptions -> [Update.UpdateM m ()] -> ClientM m ()
updateProcess uopts acts = Update.runUpdate uopts $ sequence_ [act `catchError` (Log.log Log.Error . view (commandErrorMsg . packed)) | act <- acts]

-- | Filter declarations with prefix and infix
filterMatch :: Symbol a => SearchQuery -> [a] -> [a]
filterMatch (SearchQuery q st) = filter match' where
	match' m = case st of
		SearchExact -> fromString q == symbolName m
		SearchPrefix -> fromString q `T.isPrefixOf` symbolName m
		SearchInfix -> fromString q `T.isInfixOf` symbolName m
		SearchSuffix -> fromString q `T.isSuffixOf` symbolName m
		SearchRegex -> unpack (symbolName m) =~ q
