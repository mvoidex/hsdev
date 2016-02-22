{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module HsDev.Client.Commands (
	runClient, runCommand
	) where

import Control.Applicative
import Control.Arrow
import Control.Exception (displayException)
import Control.Lens (view, preview, _Just)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State (gets)
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

import System.Directory.Paths
import Text.Format
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
runCommand (Scan projs cabals fs paths' fcts ghcs' docs' infer') = toValue $ do
	sboxes <- getSandboxes cabals
	updateProcess (Update.UpdateOptions [] ghcs' docs' infer') $ concat [
		map (\(FileContents f cts) -> Update.scanFileContents ghcs' f (Just cts)) fcts,
		map (Update.scanProject ghcs') projs,
		map (Update.scanFile ghcs') fs,
		map (Update.scanDirectory ghcs') paths',
		map (Update.scanCabal ghcs') sboxes]
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
runCommand (Remove _ _ _ _) = toValue $ return $ object ["message" .= ("not implemented" :: String)]
runCommand (InfoModules fs) = toValue $ do
	dbval <- getDb
	filter' <- targetFilters fs
	return $ map (view moduleId) $ newestPackage $ selectModules (filter' . view moduleId) dbval
runCommand InfoPackages = toValue $ (ordNub . sort . 	mapMaybe (preview (moduleLocation . modulePackage . _Just)) . allModules) <$> getDb
runCommand InfoProjects = toValue $ (toList . databaseProjects) <$> getDb
runCommand InfoSandboxes = toValue $ (ordNub . sort . mapMaybe (cabalOf . view moduleId) . allModules) <$> getDb
runCommand (InfoSymbol sq fs locals') = toValue $ do
	dbval <- liftM (localsDatabase locals') $ getDb
	filter' <- targetFilters fs
	return $ newestPackage $ filterMatch sq $ filter (checkModule filter') $ allDeclarations dbval
runCommand (InfoModule sq fs) = toValue $ do
	dbval <- getDb
	filter' <- targetFilters fs
	return $ newestPackage $ filterMatch sq $ filter (filter' . view moduleId) $ allModules dbval
runCommand (InfoResolve fpath exports) = toValue $ do
	dbval <- getDb
	cabal <- liftIO $ getSandbox fpath
	let
		cabaldb = filterDB (restrictCabal cabal) (const True) dbval
		getScope
			| exports = exportsModule
			| otherwise = scopeModule
	case lookupFile fpath dbval of
		Nothing -> commandError "File not found" []
		Just m -> return $ getScope $ resolveOne cabaldb m
runCommand (InfoProject (Left projName)) = toValue $ findProject projName
runCommand (InfoProject (Right projPath)) = toValue $ liftIO $ searchProject projPath
runCommand (InfoSandbox sandbox') = toValue $ liftIO $ searchSandbox sandbox'
runCommand (Lookup nm fpath) = toValue $ do
	dbval <- getDb
	cabal <- liftIO $ getSandbox fpath
	mapCommandIO $ lookupSymbol dbval cabal fpath nm
runCommand (Whois nm fpath) = toValue $ do
	dbval <- getDb
	cabal <- liftIO $ getSandbox fpath
	mapCommandIO $ whois dbval cabal fpath nm
runCommand (ResolveScopeModules sq fpath) = toValue $ do
	dbval <- getDb
	cabal <- liftIO $ getSandbox fpath
	liftM (filterMatch sq . map (view moduleId)) $ mapCommandIO $ scopeModules dbval cabal fpath
runCommand (ResolveScope sq global fpath) = toValue $ do
	dbval <- getDb
	cabal <- liftIO $ getSandbox fpath
	liftM (filterMatch sq) $ mapCommandIO $ scope dbval cabal fpath global
runCommand (Complete input wide fpath) = toValue $ do
	dbval <- getDb
	cabal <- liftIO $ getSandbox fpath
	mapCommandIO $ completions dbval cabal fpath input wide
runCommand (Hayoo hq p ps) = toValue $ liftM concat $ forM [p .. p + pred ps] $ \i -> liftM
	(mapMaybe Hayoo.hayooAsDeclaration . Hayoo.resultResult) $
	mapCommandIO $ Hayoo.hayoo hq (Just i)
runCommand (CabalList packages) = toValue $ mapCommandIO $ Cabal.cabalList packages
runCommand (Lint fs fcts) = toValue $ do
	mapCommandIO $ liftM2 (++)
		(liftM concat $ mapM HLint.hlintFile fs)
		(liftM concat $ mapM (\(FileContents f c) -> HLint.hlintSource f c) fcts)
runCommand (Check fs fcts ghcs') = toValue $ do
	db <- getDb
	ghc <- askSession sessionGhc
	let
		checkSome file fn = do
			cabal <- liftIO $ getSandbox file
			m <- maybe
				(commandError_ $ "File '" ++ file ++ "' not found")
				return
				(lookupFile file db)
			notes <- inWorkerWith (commandError_ . show) ghc
				(runExceptT $ fn cabal m)
			either commandError_ return notes
	liftM concat $ mapM (uncurry checkSome) $
		[(f, Check.checkFile ghcs') | f <- fs] ++
		[(f, \cabal m -> Check.checkSource ghcs' cabal m src) | FileContents f src <- fcts]
runCommand (CheckLint fs fcts ghcs') = toValue $ do
	db <- getDb
	ghc <- askSession sessionGhc
	let
		checkSome file fn = do
			cabal <- liftIO $ getSandbox file
			m <- maybe
				(commandError_ $ "File '" ++ file ++ "' not found")
				return
				(lookupFile file db)
			notes <- inWorkerWith (commandError_ . show) ghc
				(runExceptT $ fn cabal m)
			either commandError_ return notes
	checkMsgs <- liftM concat $ mapM (uncurry checkSome) $
		[(f, Check.checkFile ghcs') | f <- fs] ++
		[(f, \cabal m -> Check.checkSource ghcs' cabal m src) | FileContents f src <- fcts]
	lintMsgs <- mapCommandIO $ liftM2 (++)
		(liftM concat $ mapM HLint.hlintFile fs)
		(liftM concat $ mapM (\(FileContents f src) -> HLint.hlintSource f src) fcts)
	return $ checkMsgs ++ lintMsgs
runCommand (Types fs fcts ghcs') = toValue $ do
	db <- getDb
	ghc <- askSession sessionGhc
	let
		cts = [(f, Nothing) | f <- fs] ++ [(f, Just src) | FileContents f src <- fcts]
	liftM concat $ forM cts $ \(file, msrc) -> do
		cabal <- liftIO $ getSandbox file
		m <- maybe
			(commandError_ $ "File '" ++ file ++ "' not found")
			return
			(lookupFile file db)
		notes <- inWorkerWith (commandError_ . show) ghc
			(runExceptT $ Types.fileTypes ghcs' cabal m msrc)
		either commandError_ return notes
runCommand (GhcMod GhcModLang) = toValue $ mapCommandIO $ GhcMod.langs
runCommand (GhcMod GhcModFlags) = toValue $ mapCommandIO $ GhcMod.flags
runCommand (GhcMod (GhcModType (Position line column) fpath ghcs')) = toValue $ do
	ghcmod <- askSession sessionGhcMod
	dbval <- getDb
	cabal <- liftIO $ getSandbox fpath
	(fpath', m', _) <- mapCommandIO $ fileCtx dbval fpath
	mapCommandIO $ GhcMod.waitMultiGhcMod ghcmod fpath' $
		GhcMod.typeOf (ghcs' ++ moduleOpts (allPackages dbval) m') cabal fpath' line column
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
		cabal <- liftIO $ getSandbox file
		(_, m', _) <- fileCtx dbval file
		GhcMod.waitMultiGhcMod ghcmod file $
			GhcMod.check (ghcs' ++ moduleOpts (allPackages dbval) m') cabal [file] mproj
runCommand (GhcMod (GhcModCheckLint fs ghcs' hlints')) = toValue $ do
	ghcmod <- askSession sessionGhcMod
	dbval <- getDb
	mapCommandIO $ liftM concat $ forM fs $ \file -> do
		mproj <- liftIO $ locateProject file
		cabal <- liftIO $ getSandbox file
		(_, m', _) <- fileCtx dbval file
		GhcMod.waitMultiGhcMod ghcmod file $ do
			checked <- GhcMod.check (ghcs' ++ moduleOpts (allPackages dbval) m') cabal [file] mproj
			linted <- GhcMod.lint hlints' file
			return $ checked ++ linted
runCommand (AutoFix (AutoFixShow ns)) = toValue $ return $ AutoFix.corrections ns
runCommand (AutoFix (AutoFixFix ns rest isPure)) = toValue $ do
	files <- liftM (ordNub . sort) $ mapM findPath $ mapMaybe (preview $ Tools.noteSource . moduleFile) ns
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
runCommand Exit = toValue $ serverExit

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
	TargetCabal cabal -> liftM inCabal $ findSandbox cabal
	TargetPackage pack -> return $ inPackage pack
	TargetSourced -> return byFile
	TargetStandalone -> return standalone

-- Helper functions

-- | Find sandbox by path
findSandbox :: CommandMonad m => Cabal -> m Cabal
findSandbox Cabal = return Cabal
findSandbox (Sandbox f) = (findPath >=> mapCommandErrorStr . liftIO . getSandbox) f

-- | Canonicalize paths
findPath :: (CommandMonad m, Paths a) => a -> m a
findPath = paths findPath' where
	findPath' :: CommandMonad m => FilePath -> m FilePath
	findPath' f = do
		r <- commandRoot
		liftIO $ canonicalizePath (normalise $ if isRelative f then r </> f else f)

-- | Get list of enumerated sandboxes
getSandboxes :: (CommandMonad m, Functor m) => [Cabal] -> m [Cabal]
getSandboxes = traverse findSandbox

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
findDep :: CommandMonad m => String -> m (Project, Maybe FilePath, Cabal)
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
	sbox <- liftIO $ searchSandbox $ view projectPath proj
	return (proj, src, sbox)

-- FIXME: Doesn't work for file without project
-- | Check if project or source depends from this module
inDeps :: (Project, Maybe FilePath, Cabal) -> ModuleId -> Bool
inDeps (proj, src, cabal) = liftM2 (&&) (restrictCabal cabal) deps' where
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

mapCommandErrorStr :: CommandMonad m => ExceptT String m a -> m a
mapCommandErrorStr act = runExceptT act >>= either commandError_ return

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
