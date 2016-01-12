{-# LANGUAGE OverloadedStrings #-}

module HsDev.Client.Commands (
	runCommand
	) where

import Control.Applicative
import Control.Arrow
import Control.Lens (view, preview, _Just, from, each)
import Control.Monad
import Control.Monad.Except
import Control.Monad.State (gets)
import Control.Monad.Catch (try, SomeException(..))
import Data.Aeson hiding (Result, Error)
import Data.List
import Data.Foldable (toList)
import Data.Maybe
import qualified Data.Map as M
import Data.String (fromString)
import Data.Text (unpack)
import Data.Text.Lens (packed)
import qualified Data.Text as T (isInfixOf, isPrefixOf, isSuffixOf)
import System.Directory
import System.FilePath
import qualified System.Log.Simple.Base as Log
import Text.Regex.PCRE ((=~))

import System.Directory.Paths
import HsDev.Cache
import HsDev.Commands
import qualified HsDev.Database.Async as DB
import HsDev.Server.Message as M
import HsDev.Server.Types
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
		map (Update.scanProject ghcs') projs,
		map (Update.scanFile ghcs') fs,
		map (Update.scanDirectory ghcs') paths,
		map (Update.scanCabal ghcs') sboxes]
runCommand copts (RefineDocs projs fs ms) = runCommandM $ do
	projects <- traverse (findProject copts) projs
	dbval <- getDb copts
	let
		filters = anyOf $ concat [
			map inProject projects,
			map inFile fs,
			map inModule ms]
		mods = selectModules (filters . view moduleId) dbval
	updateProcess copts [] False False [Update.scanDocs $ map (getInspected dbval) mods]
runCommand copts (InferTypes projs fs ms) = runCommandM $ do
	projects <- traverse (findProject copts) projs
	dbval <- getDb copts
	let
		filters = anyOf $ concat [
			map inProject projects,
			map inFile fs,
			map inModule ms]
		mods = selectModules (filters . view moduleId) dbval
	updateProcess copts [] False False [Update.inferModTypes $ map (getInspected dbval) mods]
runCommand copts (Remove projs packages cabals fs) = undefined
runCommand copts (InfoModules f) = runCommandM $ do
	dbval <- getDb copts
	filter' <- targetFilter copts f
	return $ map (view moduleId) $ newestPackage $ selectModules (filter' . view moduleId) dbval
runCommand copts InfoPackages = runCommandM $
	(ordNub . sort . 	mapMaybe (preview (moduleLocation . modulePackage . _Just)) . allModules) <$> getDb copts
runCommand copts InfoProjects = runCommandM $ (toList . databaseProjects) <$> getDb copts
runCommand copts InfoSandboxes = runCommandM $
	(ordNub . sort . mapMaybe (cabalOf . view moduleId) . allModules) <$> getDb copts
runCommand copts (InfoSymbol sq f) = runCommandM $ do
	dbval <- liftM (localsDatabase False) $ getDb copts -- FIXME: Where is arg locals?
	filter' <- targetFilter copts f
	return $ newestPackage $ filterMatch sq $ filter (checkModule filter') $ allDeclarations dbval
runCommand copts (InfoModule sq f) = runCommandM $ do
	dbval <- liftM (localsDatabase False) $ getDb copts -- FIXME: Where is arg locals?
	filter' <- targetFilter copts f
	return $ newestPackage $ filterMatch sq $ filter (filter' . view moduleId) $ allModules dbval
runCommand copts (InfoResolve fpath exports) = runCommandM $ do
	dbval <- liftM (localsDatabase False) $ getDb copts -- FIXME: Where is arg locals?
	cabal <- liftIO $ getSandbox fpath
	let
		cabaldb = filterDB (restrictCabal cabal) (const True) dbval
		getScope
			| exports = exportsModule
			| otherwise = scopeModule
	case lookupFile fpath dbval of
		Nothing -> commandError "File not found" []
		Just m -> return $ getScope $ resolveOne cabaldb m
runCommand copts (InfoProject (Left projName)) = runCommandM $ findProject copts projName
runCommand copts (InfoProject (Right projPath)) = runCommandM $ liftIO $ searchProject projPath
runCommand copts (InfoSandbox sandbox) = runCommandM $ liftIO $ searchSandbox sandbox
runCommand copts (Lookup nm fpath) = runCommandM $ do
	dbval <- getDb copts
	cabal <- liftIO $ getSandbox fpath
	mapCommandErrorStr $ lookupSymbol dbval cabal fpath nm
runCommand copts (Whois nm fpath) = runCommandM $ do
	dbval <- getDb copts
	cabal <- liftIO $ getSandbox fpath
	mapCommandErrorStr $ whois dbval cabal fpath nm
runCommand copts (ResolveScopeModules fpath) = runCommandM $ do
	dbval <- getDb copts
	cabal <- liftIO $ getSandbox fpath
	liftM (map (view moduleId)) $ mapCommandErrorStr $ scopeModules dbval cabal fpath
runCommand copts (ResolveScope sq global fpath) = runCommandM $ do
	dbval <- getDb copts
	cabal <- liftIO $ getSandbox fpath
	liftM (filterMatch sq) $ mapCommandErrorStr $ scope dbval cabal fpath global
runCommand copts (Complete input wide fpath) = runCommandM $ do
	dbval <- getDb copts
	cabal <- liftIO $ getSandbox fpath
	mapCommandErrorStr $ completions dbval cabal fpath input wide
runCommand copts (Hayoo hq p ps) = runCommandM $ liftM concat $ forM [p .. p + pred ps] $ \i -> liftM
	(mapMaybe Hayoo.hayooAsDeclaration . Hayoo.resultResult) $
	mapCommandErrorStr $ Hayoo.hayoo hq (Just i)
runCommand copts (CabalList packages) = runCommandM $ mapCommandErrorStr $ Cabal.cabalList packages
runCommand copts (Lint fs fcts) = runCommandM $ do
	mapCommandErrorStr $ liftM2 (++)
		(liftM concat $ mapM HLint.hlintFile fs)
		(liftM concat $ mapM (\(FileContents f c) -> HLint.hlintSource f c) fcts)
runCommand copts (Check fs fcts ghcs') = runCommandM $ do
	db <- getDb copts
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
		[(f, Check.checkFile ghcs') | f <- fs] ++
		[(f, \cabal m -> Check.checkSource ghcs' cabal m src) | FileContents f src <- fcts]
runCommand copts (CheckLint fs fcts ghcs') = runCommandM $ do
	db <- getDb copts
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
		[(f, Check.checkFile ghcs') | f <- fs] ++
		[(f, \cabal m -> Check.checkSource ghcs' cabal m src) | FileContents f src <- fcts]
	lintMsgs <- mapCommandErrorStr $ liftM2 (++)
		(liftM concat $ mapM HLint.hlintFile fs)
		(liftM concat $ mapM (\(FileContents f src) -> HLint.hlintSource f src) fcts)
	return $ checkMsgs ++ lintMsgs
runCommand copts (Types fs fcts ghcs') = runCommandM $ do
	db <- getDb copts
	let
		cts = [(f, Nothing) | f <- fs] ++ [(f, Just src) | FileContents f src <- fcts]
	liftM concat $ forM cts $ \(file, msrc) -> do
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
	cabal <- liftIO $ getSandbox fpath
	(fpath', m', _) <- mapCommandErrorStr $ fileCtx dbval fpath
	mapCommandErrorStr $ GhcMod.waitMultiGhcMod (commandGhcMod copts) fpath' $
		GhcMod.typeOf (ghcs' ++ moduleOpts (allPackages dbval) m') cabal fpath' line column
runCommand copts (GhcMod (GhcModLint fs hlints')) = runCommandM $ do
	mapCommandErrorStr $ liftM concat $ forM fs $ \file ->
		GhcMod.waitMultiGhcMod (commandGhcMod copts) file $
			GhcMod.lint hlints' file
runCommand copts (GhcMod (GhcModCheck fs ghcs')) = runCommandM $ do
	dbval <- getDb copts
	mapCommandErrorStr $ liftM concat $ forM fs $ \file -> do
		mproj <- liftIO $ locateProject file
		cabal <- liftIO $ getSandbox file
		(_, m', _) <- fileCtx dbval file
		GhcMod.waitMultiGhcMod (commandGhcMod copts) file $
			GhcMod.check (ghcs' ++ moduleOpts (allPackages dbval) m') cabal [file] mproj
runCommand copts (GhcMod (GhcModCheckLint fs ghcs' hlints')) = runCommandM $ do
	dbval <- getDb copts
	mapCommandErrorStr $ liftM concat $ forM fs $ \file -> do
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

targetFilter :: MonadIO m => CommandOptions -> TargetFilter -> ExceptT CommandError m (ModuleId -> Bool)
targetFilter copts f = case f of
	TargetProject proj -> liftM inProject $ findProject copts proj
	TargetFile file -> return $ inFile file
	TargetModule mname -> return $ inModule mname
	TargetDepsOf dep -> liftM inDeps $ findDep copts dep
	TargetCabal cabal -> liftM inCabal $ findSandbox copts cabal
	TargetPackage pack -> return $ inPackage pack
	TargetSourced -> return byFile
	TargetStandalone -> return standalone
	TargetAny -> return (const True)

-- Helper functions

commandStrMsg :: String -> CommandError
commandStrMsg m = CommandError m []

-- | Find sandbox by path
findSandbox :: MonadIO m => CommandOptions -> Cabal -> ExceptT CommandError m Cabal
findSandbox copts Cabal = return Cabal
findSandbox copts (Sandbox f) = (findPath copts >=> mapCommandErrorStr . liftIO . getSandbox) f

-- | Canonicalize paths
findPath :: (MonadIO m, Paths a) => CommandOptions -> a -> ExceptT e m a
findPath copts = paths findPath' where
	findPath' :: MonadIO m => FilePath -> ExceptT e m FilePath
	findPath' f = liftIO $ canonicalizePath (normalise f') where
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
filterMatch :: Symbol a => SearchQuery -> [a] -> [a]
filterMatch (SearchQuery q st) = filter match' where
	match' m = case st of
		SearchExact -> fromString q == symbolName m
		SearchPrefix -> fromString q `T.isPrefixOf` symbolName m
		SearchInfix -> fromString q `T.isInfixOf` symbolName m
		SearchSuffix -> fromString q `T.isSuffixOf` symbolName m
		SearchRegex -> unpack (symbolName m) =~ q
