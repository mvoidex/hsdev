{-# LANGUAGE OverloadedStrings #-}

module HsDev.Client.Commands (
	runCommand
	) where

import Control.Applicative
import Control.Arrow
import Control.Lens (view, preview, each, _Just, from)
import Control.Monad
import Control.Monad.Except
import Control.Monad.State (gets)
import Control.Monad.Catch (try, SomeException(..))
import Control.Monad.Trans.Maybe
import Data.Aeson hiding (Result, Error)
import Data.List
import Data.Foldable (toList, find)
import Data.Maybe
import qualified Data.Map as M
import Data.String (fromString)
import Data.Text (unpack)
import Data.Text.Lens (packed)
import qualified Data.Text as T (isInfixOf, isPrefixOf, isSuffixOf)
import System.Directory
import System.FilePath
import qualified System.Log.Simple.Base as Log
import Text.Read (readMaybe)
import Text.Regex.PCRE ((=~))

import HsDev.Cache
import qualified HsDev.Cache.Structured as SC
import HsDev.Commands
import qualified HsDev.Database.Async as DB
import HsDev.Server.Message as M
import HsDev.Server.Types hiding (cmd)
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

import Control.Concurrent.Util

import qualified HsDev.Database.Update as Update

runCommandM :: ToJSON a => CommandM a -> IO Result
runCommandM = liftM toResult . runExceptT where
	toResult (Left (CommandError e ds)) = Error e $ M.fromList $ map (first unpack) ds
	toResult (Right r') = Result $ toJSON r'

-- | Run command
runCommand :: CommandOptions -> Command -> IO Result
runCommand copts Ping = runCommandM $ return $ object ["message" .= ("pong" :: String)]
runCommand copts Listen = runCommandM $ liftIO $ commandListenLog copts $
	mapM_ (\msg -> commandNotify copts (Notification $ object ["message" .= msg]))
runCommand copts (AddData cts) = runCommandM $ mapM_ updateData cts where
	updateData (AddedDatabase db) = DB.update (dbVar copts) $ return db
	updateData (AddedModule m) = DB.update (dbVar copts) $ return $ fromModule m
	updateData (AddedProject p) = DB.update (dbVar copts) $ return $ fromProject p
runCommand copts (Scan projs cabals fs paths fcts ghcs' docs' infer') = runCommandM $ do
	sboxes <- getSandboxes copts cabals
	updateProcess copts ghcs' docs' infer' $ concat [
		map (\(FileContents f cts) -> Update.scanFileContents ghcs' f (Just cts)) fcts,
		map (\proj -> findPath copts proj >>= Update.scanProject ghcs') projs,
		map (\f -> findPath copts f >>= Update.scanFile ghcs') fs,
		map (\path -> findPath copts path >>= Update.scanDirectory ghcs') paths,
		map (Update.scanCabal ghcs') sboxes]
runCommand copts (RefineDocs projs fs ms) = runCommandM $ do
	files <- traverse (findPath copts) fs
	projects <- traverse (findProject copts) projs
	dbval <- getDb copts
	let
		filters = anyOf $ concat [
			map inProject projects,
			map inFile files,
			map inModule ms]
		mods = selectModules (filters . view moduleId) dbval
	updateProcess copts [] False False [Update.scanDocs $ map (getInspected dbval) mods]
runCommand copts (InferTypes projs fs ms) = runCommandM $ do
	files <- traverse (findPath copts) fs
	projects <- traverse (findProject copts) projs
	dbval <- getDb copts
	let
		filters = anyOf $ concat [
			map inProject projects,
			map inFile files,
			map inModule ms]
		mods = selectModules (filters . view moduleId) dbval
	updateProcess copts [] False False [Update.inferModTypes $ map (getInspected dbval) mods]
runCommand copts (Remove projs packages cabals fs) = undefined
runCommand copts (InfoModules fs) = runCommandM $ do
	dbval <- getDb copts
	projs <- traverse (findProject copts) [proj | TargetProject proj <- fs]
	deps <- traverse (findDep copts) [dep | TargetDepsOf dep <- fs]
	cabals <- getSandboxes copts [sbox | TargetCabal sbox <- fs]
	let
		packages = [package | TargetPackage package <- fs]
		mods = [m' | TargetModule m' <- fs]
		files = [file | TargetFile file <- fs]
		hasFilters = not $ null projs && null packages && null cabals && null deps
		filters = allOf $ catMaybes [
			if hasFilters
				then Just $ anyOf $ catMaybes [
					if null projs then Nothing else Just (\m -> any (`inProject` m) projs),
					if null deps then Nothing else Just (\m -> any (`inDeps` m) deps),
					if null mods then Nothing else Just (\m -> any (`inModule` m) mods),
					if null files then Nothing else Just (\m -> any (`inFile` m) files),
					if null packages && null cabals then Nothing
						else Just (\m -> (any (`inPackage` m) packages || null packages) && (any (`inCabal` m) cabals || null cabals))]
				else Nothing,
			if TargetSourced `elem` fs then Just byFile else Nothing,
			if TargetStandalone `elem` fs then Just standalone else Nothing]
	return $ map (view moduleId) $ newest (TargetOldToo `notElem` fs) $ selectModules (filters . view moduleId) dbval
runCommand copts InfoPackages = runCommandM $
	(ordNub . sort . 	mapMaybe (preview (moduleLocation . modulePackage . _Just)) . allModules) <$> getDb copts
runCommand copts InfoProjects = runCommandM $ (toList . databaseProjects) <$> getDb copts
runCommand copts InfoSandboxes = runCommandM $
	(ordNub . sort . mapMaybe (cabalOf . view moduleId) . allModules) <$> getDb copts
runCommand copts (InfoSymbol sq fs) = runCommandM $ do
	dbval <- liftM (localsDatabase False) $ getDb copts -- FIXME: Where is arg locals?
	proj <- traverse (findProject copts) $ listToMaybe [p | TargetProject p <- fs]
	file <- traverse (findPath copts) $ listToMaybe [f | TargetFile f <- fs]
	deps <- traverse (findDep copts) $ listToMaybe [d | TargetDepsOf d <- fs]
	cabal <- traverse (findSandbox copts) $ listToMaybe [c | TargetCabal c <- fs]
	let
		filters = checkModule $ allOf $ catMaybes [
			fmap inProject proj,
			fmap inFile file,
			fmap inModule $ listToMaybe [m | TargetModule m <- fs],
			fmap inPackage $ listToMaybe [p | TargetPackage p <- fs],
			fmap inDeps deps,
			-- fmap inVersion (arg "version" as), -- FIXME: Where is version argument?
			fmap inCabal cabal,
			if TargetSourced `elem` fs then Just byFile else Nothing,
			if TargetStandalone `elem` fs then Just standalone else Nothing]
		toResult = newest (TargetOldToo `notElem` fs) . filterMatch sq . filter filters
	return $ toResult $ allDeclarations dbval
runCommand copts (InfoModule fs) = runCommandM $ do
	dbval <- liftM (localsDatabase False) $ getDb copts -- FIXME: Where is arg locals?
	proj <- traverse (findProject copts) $ listToMaybe [p | TargetProject p <- fs]
	cabal <- traverse (findSandbox copts) $ listToMaybe [c | TargetCabal c <- fs]
	file' <- mapExceptT (fmap $ left commandStrMsg) $ traverse (findPath copts) $ listToMaybe [f | TargetFile f <- fs]
	deps <- traverse (findDep copts) $ listToMaybe [d | TargetDepsOf d <- fs]
	let
		filters = allOf $ catMaybes [
			fmap inProject proj,
			fmap inCabal cabal,
			fmap inFile file',
			fmap inModule $ listToMaybe [m | TargetModule m <- fs],
			fmap inPackage $ listToMaybe [p | TargetPackage p <- fs],
			fmap inDeps deps,
			-- fmap inVersion (arg "version" as), -- FIXME: Where is version argument?
			if TargetSourced `elem` fs then Just byFile else Nothing,
			if TargetStandalone `elem` fs then Just standalone else Nothing]
	rs <- (newest (TargetOldToo `notElem` fs) . filter (filters . view moduleId)) <$> maybe
		(return $ allModules dbval)
		(mapCommandErrorStr . findModule dbval)
		(listToMaybe [m | TargetModule m <- fs])
	case rs of
		[] -> commandError "Module not found" []
		[m] -> return m
		ms' -> commandError "Ambiguous modules" ["modules" .= map (view moduleId) ms']
runCommand copts (InfoResolve fs exports) = runCommandM $ do
	dbval <- liftM (localsDatabase False) $ getDb copts -- FIXME: Where is arg locals?
	proj <- traverse (findProject copts) $ listToMaybe [p | TargetProject p <- fs]
	cabal <- traverse (findSandbox copts) $ listToMaybe [c | TargetCabal c <- fs]
	file' <- mapExceptT (fmap $ left commandStrMsg) $ traverse (findPath copts) $ listToMaybe [f | TargetFile f <- fs]
	let
		filters = allOf $ catMaybes [
			fmap inProject proj,
			fmap inFile file',
			fmap inModule $ listToMaybe [m | TargetModule m <- fs],
			Just byFile]
	rs <- (newest (TargetOldToo `notElem` fs) . filter (filters . view moduleId)) <$> maybe
		(return $ allModules dbval)
		(mapCommandErrorStr . findModule dbval)
		(listToMaybe [m | TargetModule m <- fs])
	let
		cabaldb = filterDB (restrictCabal $ fromMaybe Cabal cabal) (const True) dbval
		getScope = if exports then exportsModule else scopeModule
	case rs of
		 [] -> commandError "Module not found" []
		 [m] -> return $ getScope $ resolveOne cabaldb m
		 ms' -> commandError "Ambiguous modules" ["modules" .= map (view moduleId) ms']
runCommand copts (InfoProject (Left projName)) = runCommandM $ findProject copts projName
runCommand copts (InfoProject (Right projPath)) = runCommandM $ liftIO $ searchProject projPath
runCommand copts (InfoSandbox sandbox) = runCommandM $ liftIO $ searchSandbox sandbox
runCommand copts (Lookup nm fpath) = runCommandM $ do
	dbval <- getDb copts
	srcFile <- findPath copts fpath
	cabal <- liftIO $ getSandbox srcFile
	mapCommandErrorStr $ lookupSymbol dbval cabal srcFile nm
runCommand copts (Whois nm fpath) = runCommandM $ do
	dbval <- getDb copts
	srcFile <- findPath copts fpath
	cabal <- liftIO $ getSandbox srcFile
	mapCommandErrorStr $ whois dbval cabal srcFile nm
runCommand copts (ResolveScopeModules fpath) = runCommandM $ do
	dbval <- getDb copts
	srcFile <- findPath copts fpath
	cabal <- liftIO $ getSandbox srcFile
	liftM (map (view moduleId)) $ mapCommandErrorStr $ scopeModules dbval cabal srcFile
runCommand copts (ResolveScope sq global fpath) = runCommandM $ do
	dbval <- getDb copts
	srcFile <- findPath copts fpath
	cabal <- liftIO $ getSandbox srcFile
	liftM (filterMatch sq) $ mapCommandErrorStr $ scope dbval cabal srcFile global
runCommand copts (Complete input wide fpath) = runCommandM $ do
	dbval <- getDb copts
	srcFile <- findPath copts fpath
	cabal <- liftIO $ getSandbox srcFile
	mapCommandErrorStr $ completions dbval cabal srcFile input wide
runCommand copts (Hayoo hq p ps) = runCommandM $ liftM concat $ forM [p .. p + pred ps] $ \i -> liftM
	(mapMaybe Hayoo.hayooAsDeclaration . Hayoo.resultResult) $
	mapCommandErrorStr $ Hayoo.hayoo hq (Just i)
runCommand copts (CabalList packages) = runCommandM $ mapCommandErrorStr $ Cabal.cabalList packages
runCommand copts (Lint fs fcts) = runCommandM $ do
	files <- mapM (findPath copts) fs
	cts <- forM fcts $ \(FileContents f c) -> either
		(\err -> commandError "Unable to decode data" ["why" .= err, "file" .= f, "contents" .= c])
		(\d -> liftM2 (,) (findPath copts f) (return d))
		(eitherDecode (toUtf8 c))
	mapCommandErrorStr $ liftM2 (++)
		(liftM concat $ mapM HLint.hlintFile files)
		(liftM concat $ mapM (uncurry HLint.hlintSource) cts)
runCommand copts (Check fs fcts ghcs') = runCommandM $ do
	db <- getDb copts
	files <- mapM (findPath copts) fs
	cts <- mapM (\(FileContents f c) -> liftM (`FileContents` c) (findPath copts f)) fcts
	let
		checkSome file fn = do
			cabal <- liftIO $ getSandbox file
			m <- maybe
				(commandError_ $ "File '" ++ file ++ "' not found")
				return
				(lookupFile file db)
			notes <- inWorkerWith (commandError_ . show) (commandGhc copts) $
				(runExceptT $ fn cabal m)
			either commandError_ return notes
	liftM concat $ mapM (uncurry checkSome) $
		[(f, Check.checkFile ghcs') | f <- files] ++
		[(f, \cabal m -> Check.checkSource ghcs' cabal m src) | FileContents f src <- cts]
runCommand copts (CheckLint fs fcts ghcs') = runCommandM $ do
	db <- getDb copts
	files <- mapM (findPath copts) fs
	cts <- mapM (\(FileContents f c) -> liftM (`FileContents` c) (findPath copts f)) fcts
	let
		checkSome file fn = do
			cabal <- liftIO $ getSandbox file
			m <- maybe
				(commandError_ $ "File '" ++ file ++ "' not found")
				return
				(lookupFile file db)
			notes <- inWorkerWith (commandError_ . show) (commandGhc copts) $
				(runExceptT $ fn cabal m)
			either commandError_ return notes
	checkMsgs <- liftM concat $ mapM (uncurry checkSome) $
		[(f, Check.checkFile ghcs') | f <- files] ++
		[(f, \cabal m -> Check.checkSource ghcs' cabal m src) | FileContents f src <- cts]
	lintMsgs <- mapCommandErrorStr $ liftM2 (++)
		(liftM concat $ mapM HLint.hlintFile files)
		(liftM concat $ mapM (\(FileContents f src) -> HLint.hlintSource f src) cts)
	return $ checkMsgs ++ lintMsgs
runCommand copts (Types fs fcts ghcs') = runCommandM $ do
	db <- getDb copts
	files <- mapM (findPath copts) fs
	cts <- mapM (\(FileContents f c) -> liftM (`FileContents` c) (findPath copts f)) fcts
	let
		fcts = [(f, Nothing) | f <- files] ++ [(f, Just src) | FileContents f src <- cts]
	liftM concat $ forM fcts $ \(file, msrc) -> do
		cabal <- liftIO $ getSandbox file
		m <- maybe
			(commandError_ $ "File '" ++ file ++ "' not found")
			return
			(lookupFile file db)
		notes <- inWorkerWith (commandError_ . show) (commandGhc copts) $
			(runExceptT $ Types.fileTypes ghcs' cabal m msrc)
		either commandError_ return notes
runCommand copts (GhcMod GhcModLang) = runCommandM $ mapCommandErrorStr GhcMod.langs
runCommand copts (GhcMod GhcModFlags) = runCommandM $ mapCommandErrorStr GhcMod.flags
runCommand copts (GhcMod (GhcModType (Position line column) fpath ghcs')) = runCommandM $ do
	dbval <- getDb copts
	srcFile <- findPath copts fpath
	cabal <- liftIO $ getSandbox srcFile
	(srcFile', m', _) <- mapCommandErrorStr $ fileCtx dbval srcFile
	mapCommandErrorStr $ GhcMod.waitMultiGhcMod (commandGhcMod copts) srcFile' $
		GhcMod.typeOf (ghcs' ++ moduleOpts (allPackages dbval) m') cabal srcFile' line column
runCommand copts (GhcMod (GhcModLint fs hlints')) = runCommandM $ do
	files <- mapM (findPath copts) fs
	mapCommandErrorStr $ liftM concat $ forM files $ \file ->
		GhcMod.waitMultiGhcMod (commandGhcMod copts) file $
			GhcMod.lint hlints' file
runCommand copts (GhcMod (GhcModCheck fs ghcs')) = runCommandM $ do
	files <- mapM (findPath copts) fs
	dbval <- getDb copts
	mapCommandErrorStr $ liftM concat $ forM files $ \file -> do
		mproj <- liftIO $ locateProject file
		cabal <- liftIO $ getSandbox file
		(_, m', _) <- fileCtx dbval file
		GhcMod.waitMultiGhcMod (commandGhcMod copts) file $
			GhcMod.check (ghcs' ++ moduleOpts (allPackages dbval) m') cabal [file] mproj
runCommand copts (GhcMod (GhcModCheckLint fs ghcs' hlints')) = runCommandM $ do
	files <- mapM (findPath copts) fs
	dbval <- getDb copts
	mapCommandErrorStr $ liftM concat $ forM files $ \file -> do
		mproj <- liftIO $ locateProject file
		cabal <- liftIO $ getSandbox file
		(_, m', _) <- fileCtx dbval file
		GhcMod.waitMultiGhcMod (commandGhcMod copts) file $ do
			checked <- GhcMod.check (ghcs' ++ moduleOpts (allPackages dbval) m') cabal [file] mproj
			linted <- GhcMod.lint hlints' file
			return $ checked ++ linted
runCommand copts (AutoFix (AutoFixShow ns)) = runCommandM $ return $ AutoFix.corrections ns
runCommand copts (AutoFix (AutoFixFix ns rest isPure)) = runCommandM $ do
	files <- liftM (ordNub . sort) $ mapM (findPath copts) $ mapMaybe (preview $ Tools.noteSource . moduleFile) ns
	let
		doFix :: FilePath -> String -> ([Tools.Note AutoFix.Correction], String)
		doFix file cts = AutoFix.edit cts fUpCorrs $ do
			AutoFix.autoFix fCorrs
			gets (view AutoFix.regions)
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
	mapCommandErrorStr $ liftM concat $ mapM runFix files
runCommand copts (GhcEval exprs) = runCommandM $ mapCommandErrorStr $ liftM (map toValue) $ liftAsync $
	pushTask (commandGhci copts) $ mapM (try . evaluate) exprs
	where
		toValue :: Either SomeException String -> Value
		toValue (Left (SomeException e)) = object ["fail" .= show e]
		toValue (Right s) = toJSON s
runCommand copts (Link hold) = runCommandM $ liftIO $ commandLink copts >> when hold (commandHold copts)
runCommand copts Exit = runCommandM $ liftIO $ commandExit copts

-- Helper functions

commandStrMsg :: String -> CommandError
commandStrMsg m = CommandError m []

-- | Find sandbox by path
findSandbox :: MonadIO m => CommandOptions -> Cabal -> ExceptT CommandError m Cabal
findSandbox copts Cabal = return Cabal
findSandbox copts (Sandbox f) = (findPath copts >=> mapCommandErrorStr . liftIO . getSandbox) f

-- | Canonicalize path
findPath :: MonadIO m => CommandOptions -> FilePath -> ExceptT e m FilePath
findPath copts f = liftIO $ canonicalizePath (normalise f') where
	f'
		| isRelative f = commandRoot copts </> f
		| otherwise = f

-- | Get list of enumerated sandboxes
getSandboxes :: (MonadIO m, Functor m) => CommandOptions -> [Cabal] -> ExceptT CommandError m [Cabal]
getSandboxes copts cs = traverse (findSandbox copts) cs

-- | Find project by name of path
findProject :: MonadIO m => CommandOptions -> String -> ExceptT CommandError m Project
findProject copts proj = do
	db' <- getDb copts
	proj' <- liftM addCabal $ findPath copts proj
	let
		resultProj =
			refineProject db' (project proj') <|>
			find ((== proj) . view projectName) (databaseProjects db')
	maybe (throwError $ commandStrMsg $ "Projects " ++ proj ++ " not found") return resultProj
	where
		addCabal p
			| takeExtension p == ".cabal" = p
			| otherwise = p </> (takeBaseName p <.> "cabal")

-- | Find dependency: it may be source, project file or project name, also returns sandbox found
findDep :: MonadIO m => CommandOptions -> String -> ExceptT CommandError m (Project, Maybe FilePath, Cabal)
findDep copts depName = do
	depPath <- findPath copts depName
	proj <- msum [
		mapCommandErrorStr $ do
			p <- liftIO (locateProject depPath)
			maybe (throwError $ "Project " ++ depName ++ " not found") (mapExceptT liftIO . loadProject) p,
		findProject copts depName]
	let
		src
			| takeExtension depPath == ".hs" = Just depPath
			| otherwise = Nothing
	sbox <- liftIO $ searchSandbox $ view projectPath proj
	return (proj, src, sbox)

-- | Check if project or source depends from this module
inDeps :: (Project, Maybe FilePath, Cabal) -> ModuleId -> Bool
inDeps (proj, src, cabal) = liftM2 (&&) (restrictCabal cabal) deps' where
	deps' = case src of
		Nothing -> inDepsOfProject proj
		Just src' -> inDepsOfFile proj src'

cacheLoad :: CommandOptions -> IO (Either String Database) -> CommandM ()
cacheLoad copts act = liftIO $ do
	db' <- act
	case db' of
		Left e -> commandLog copts Log.Error e
		Right database -> DB.update (dbVar copts) (return database)

-- | Bring locals to top scope to search within them if 'locals' flag set
localsDatabase :: Bool -> Database -> Database
localsDatabase True = databaseLocals
localsDatabase False = id

-- | Select newest packages if 'no-last' flag not set
newest :: Symbol a => Bool -> [a] -> [a]
newest True = newestPackage
newest False = id

-- | Convert from just of throw
forceJust :: MonadIO m => String -> ExceptT CommandError m (Maybe a) -> ExceptT CommandError m a
forceJust msg act = act >>= maybe (throwError $ commandStrMsg msg) return

-- | Get actual DB state
getDb :: MonadIO m => CommandOptions -> m Database
getDb = liftIO . DB.readAsync . commandDatabase

-- | Get DB async var
dbVar :: CommandOptions -> DB.Async Database
dbVar = commandDatabase

mapCommandErrorStr :: (Monad m) => ExceptT String m a -> ExceptT CommandError m a
mapCommandErrorStr = mapExceptT (liftM $ left commandStrMsg)

-- | Run DB update action
updateProcess :: CommandOptions -> [String] -> Bool -> Bool -> [ExceptT String (Update.UpdateDB IO) ()] -> CommandM ()
updateProcess copts ghcOpts' docs' infer' acts = lift $ Update.updateDB (Update.settings copts ghcOpts' docs' infer') $ sequence_ [act `catchError` logErr | act <- acts] where
	logErr :: String -> ExceptT String (Update.UpdateDB IO) ()
	logErr e = liftIO $ commandLog copts Log.Error e

-- | Filter declarations with prefix and infix
filterMatch :: SearchQuery -> [ModuleDeclaration] -> [ModuleDeclaration]
filterMatch (SearchQuery q st) = filter match' where
	match' m = case st of
		SearchExact -> fromString q == view (moduleDeclaration . declarationName) m
		SearchPrefix -> fromString q `T.isPrefixOf` view (moduleDeclaration . declarationName) m
		SearchInfix -> fromString q `T.isInfixOf` view (moduleDeclaration . declarationName) m
		SearchSuffix -> fromString q `T.isSuffixOf` view (moduleDeclaration . declarationName) m
		SearchRegex -> view (moduleDeclaration . declarationName . from packed) m =~ q
