{-# LANGUAGE CPP, TypeSynonymInstances, ImplicitParams, TemplateHaskell #-}

module HsDev.Inspect (
	preload,
	AnalyzeEnv(..), analyzeEnv, analyzeFixities, analyzeRefine, moduleAnalyzeEnv,
	analyzeResolve, analyzePreloaded,
	inspectDocs, inspectDocsGhc,
	inspectContents, contentsInspection,
	inspectFile, sourceInspection, fileInspection, fileContentsInspection, fileContentsInspection_, installedInspection, moduleInspection,
	projectDirs, projectSources,
	getDefines,
	preprocess, preprocess_,

	module HsDev.Inspect.Types,
	module HsDev.Inspect.Resolve,
	module Control.Monad.Except
	) where

import Control.DeepSeq
import qualified Control.Exception as E
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Except
import Data.List
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Semigroup
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, getPOSIXTime, POSIXTime)
import qualified Data.Map.Strict as M
import qualified Language.Haskell.Exts as H
import Language.Haskell.Exts.Fixity
import qualified Language.Haskell.Names as N
import qualified Language.Haskell.Names.Annotated as N
import qualified Language.Haskell.Names.SyntaxUtils as N
import qualified Language.Haskell.Names.Exports as N
import qualified Language.Haskell.Names.Imports as N
import qualified Language.Haskell.Names.ModuleSymbols as N
import qualified Language.Haskell.Names.Open as N
import qualified Language.Preprocessor.Cpphs as Cpphs
import qualified System.Directory as Dir
import System.FilePath
import Text.Format

import HsDev.Display ()
import HsDev.Error
import HsDev.Inspect.Definitions
import HsDev.Inspect.Types
import HsDev.Inspect.Resolve
import HsDev.Sandbox (searchPackageDbStack)
import HsDev.Symbols
import HsDev.Symbols.Resolve (refineSymbol, refineTable, RefineTable)
import qualified HsDev.Symbols.HaskellNames as HN
import HsDev.Tools.Base
import HsDev.Tools.Ghc.Worker (GhcM)
import HsDev.Tools.HDocs (hdocs, hdocsProcess, readModuleDocs)
import HsDev.Util
import System.Directory.Paths

-- | Preload module - load head and imports to get actual extensions and dependencies
preload :: (MonadIO m, MonadCatch m) => Text -> [(String, String)] -> [String] -> Maybe Text -> InspectM ModuleLocation ModuleTag m Preloaded
preload name defines opts mcts = inspectTag OnlyHeaderTag $ case mcts of
	Nothing -> do
		mloc <- ask
		case mloc of
			FileModule fpath mproj -> do
				inspect_ (liftIO $ fileInspection fpath opts) $ do
					cts <- liftIO $ readFileUtf8 (view path fpath)
					let
						srcExts = fromMaybe (takeDir fpath `withExtensions` mempty) $ do
							proj <- mproj
							findSourceDir proj fpath
					liftIO $ preload' name defines (opts ++ extensionsOpts srcExts) mloc cts
			_ -> throwError $ InspectError $ format "preload called on non-sourced module: {}" ~~ mloc
	Just cts -> inspect (liftIO $ fileContentsInspection opts) $ \mloc ->
		liftIO $ preload' name defines opts mloc cts
	where
		preload' name' defines' opts' mloc' cts' = do
			cts'' <- preprocess_ defines' exts fpath $ T.map untab cts'
			pragmas <- parseOk $ H.getTopPragmas (T.unpack cts'')
			let
				fileExts = [H.parseExtension (T.unpack $ fromName_ $ void lang) | H.LanguagePragma _ langs <- pragmas, lang <- langs]
				pmode = H.ParseMode {
					H.parseFilename = view path fpath,
					H.baseLanguage = H.Haskell2010,
					H.extensions = ordNub (map H.parseExtension exts ++ fileExts),
					H.ignoreLanguagePragmas = False,
					H.ignoreLinePragmas = True,
					H.fixities = Nothing,
					H.ignoreFunctionArity = False }
			H.ModuleHeadAndImports l mpragmas mhead mimps <- parseOk $ fmap H.unNonGreedy $ H.parseWithMode pmode (T.unpack cts'')
			let
				mname = case mhead of
					Just (H.ModuleHead _ (H.ModuleName _ nm) _ _) -> nm
					_ -> "Main"
			return $ Preloaded {
				_preloadedId = ModuleId (fromString mname) mloc',
				_preloadedMode = pmode,
				_preloadedModule = H.Module l mhead mpragmas mimps [],
				_preloaded = cts'' }
			where
				fpath = fromMaybe name' (mloc' ^? moduleFile)
				parseOk :: H.ParseResult a -> IO a
				parseOk (H.ParseOk v) = return v
				parseOk (H.ParseFailed loc err) = hsdevError $ InspectError $
					format "Parse {} failed at {} with: {}" ~~ fpath ~~ show loc ~~ err
				untab '\t' = ' '
				untab ch = ch
				exts = mapMaybe flagExtension opts'

data AnalyzeEnv = AnalyzeEnv {
	_analyzeEnv :: N.Environment,
	_analyzeFixities :: M.Map Name H.Fixity,
	_analyzeRefine :: RefineTable }

instance Semigroup AnalyzeEnv where
	AnalyzeEnv lenv lf lt <> AnalyzeEnv renv rf rt = AnalyzeEnv
		(lenv <> renv)
		(lf <> rf)
		(lt <> rt)

instance Monoid AnalyzeEnv where
	mempty = AnalyzeEnv mempty mempty mempty
	mappend l r = l <> r

moduleAnalyzeEnv :: Module -> AnalyzeEnv
moduleAnalyzeEnv m = AnalyzeEnv
	(environment m)
	(m ^. fixitiesMap)
	(refineTable (m ^.. exportedSymbols))

-- | Resolve module imports/exports/scope
analyzeResolve :: AnalyzeEnv -> Module -> Module
analyzeResolve (AnalyzeEnv env _ rtable) m = case m ^. moduleSource of
	Nothing -> m
	Just msrc -> over moduleSymbols (refineSymbol stbl) $ m {
		_moduleImports = map (toImport . dropScope) idecls',
		_moduleExports = map HN.fromSymbol $ N.exportedSymbols tbl msrc,
		_moduleFixities = [Fixity (void assoc) (fromMaybe 0 pr) (fixName opName)
			| H.InfixDecl _ assoc pr ops <- decls', opName <- map getOpName ops],
		_moduleScope = M.map (map HN.fromSymbol) tbl,
		_moduleSource = Just annotated }
		where
			getOpName (H.VarOp _ nm) = nm
			getOpName (H.ConOp _ nm) = nm
			fixName o = H.Qual () (H.ModuleName () (T.unpack $ m ^. moduleId . moduleName)) (void o)
			itbl = N.importTable env msrc
			tbl = N.moduleTable itbl msrc
			syms = set (each . symbolId . symbolModule) (m ^. moduleId) $
				getSymbols decls'
			stbl = refineTable syms `mappend` rtable
			-- Not using 'annotate' because we already computed needed tables
			annotated = H.Module l mhead' mpragmas idecls' decls'
			H.Module l mhead mpragmas idecls decls = fmap (\(N.Scoped _ v) -> N.Scoped N.None v) msrc
			mhead' = fmap scopeHead mhead
			scopeHead (H.ModuleHead lh mname mwarns mexports) = H.ModuleHead lh mname mwarns $
				fmap (N.annotateExportSpecList tbl . dropScope) mexports
			idecls' = N.annotateImportDecls mn env (fmap dropScope idecls)
			decls' = map (N.annotateDecl (N.initialScope (N.dropAnn mn) tbl) . dropScope) decls
			mn = dropScope $ N.getModuleName msrc

-- | Inspect preloaded module
analyzePreloaded :: AnalyzeEnv -> Preloaded -> Either String Module
analyzePreloaded aenv@(AnalyzeEnv env gfixities _) p = case H.parseFileContentsWithMode (_preloadedMode p') (T.unpack $ _preloaded p') of
	H.ParseFailed loc reason -> Left $ "Parse failed at " ++ show loc ++ ": " ++ reason
	H.ParseOk m -> Right $ analyzeResolve aenv $ Module {
		_moduleId = _preloadedId p',
		_moduleDocs = Nothing,
		_moduleImports = mempty,
		_moduleExports = mempty,
		_moduleFixities = mempty,
		_moduleScope = mempty,
		_moduleSource = Just $ fmap (N.Scoped N.None) m }
	where
		qimps = M.keys $ N.importTable env (_preloadedModule p)
		p' = p { _preloadedMode = (_preloadedMode p) { H.fixities = Just (mapMaybe (`M.lookup` gfixities) qimps) } }

-- | Adds documentation to declaration
addDoc :: Map String String -> Symbol -> Symbol
addDoc docsMap sym' = set symbolDocs (preview (ix (view (symbolId . symbolName) sym')) docsMap') sym' where
	docsMap' = M.mapKeys fromString . M.map fromString $ docsMap

-- | Adds documentation to all declarations in module
addDocs :: Map String String -> Module -> Module
addDocs docsMap = over moduleSymbols (addDoc docsMap)

-- | Extract file docs and set them to module declarations
inspectDocs :: [String] -> Module -> GhcM Module
inspectDocs opts m = do
	let
		hdocsWorkaround = False
	pdbs <- case view (moduleId . moduleLocation) m of
		FileModule fpath mproj -> searchPackageDbStack (maybe CabalTool (view projectBuildTool) mproj) fpath
		InstalledModule{} -> return userDb
		_ -> return userDb
	docsMap <- if hdocsWorkaround
		then liftIO $ hdocsProcess (fromMaybe (T.unpack $ view (moduleId . moduleName) m) (preview (moduleId . moduleLocation . moduleFile . path) m)) opts
		else liftM Just $ hdocs pdbs (view (moduleId . moduleLocation) m) opts
	return $ maybe id addDocs docsMap m

-- | Like @inspectDocs@, but in @Ghc@ monad
inspectDocsGhc :: [String] -> Module -> GhcM Module
inspectDocsGhc opts m = do
	docsMap <- readModuleDocs opts m
	return $ maybe id addDocs docsMap m

-- | Inspect contents
inspectContents :: Text -> [(String, String)] -> [String] -> Text -> IO InspectedModule
inspectContents name defines opts cts = runInspect (OtherLocation name) $ withInspection (contentsInspection cts opts) $ do
	p <- preload name defines opts (Just cts)
	analyzed <- lift $ either (hsdevError . InspectError) return $ analyzePreloaded mempty p
	inspectUntag OnlyHeaderTag $
		return $ set (moduleId . moduleLocation) (OtherLocation name) analyzed

contentsInspection :: Text -> [String] -> IO Inspection
contentsInspection _ _ = return InspectionNone -- crc or smth

-- | Inspect file
inspectFile :: [(String, String)] -> [String] -> Path -> Maybe Project -> Maybe Text -> IO InspectedModule
inspectFile defines opts file mproj mcts = hsdevLiftIO $ do
	absFilename <- canonicalize file
	ex <- fileExists absFilename
	unless ex $ hsdevError $ FileNotFound absFilename
	runInspect (FileModule absFilename mproj) $ withInspection (sourceInspection absFilename mcts opts) $ do
		p <- preload absFilename defines opts mcts
		forced <- liftIO (E.handle onErr (return $!! analyzePreloaded mempty p)) >>= either (hsdevError . InspectError) return
		return $ set (moduleId . moduleLocation) (FileModule absFilename mproj) forced
	where
		onErr :: E.ErrorCall -> IO (Either String Module)
		onErr = return . Left . show

-- | Source inspection data, differs whether there are contents provided
sourceInspection :: Path -> Maybe Text -> [String] -> IO Inspection
sourceInspection f Nothing = fileInspection f
sourceInspection _ (Just _) = fileContentsInspection

-- | File inspection data
fileInspection :: Path -> [String] -> IO Inspection
fileInspection f opts = do
	tm <- Dir.getModificationTime (view path f)
	return $ InspectionAt (utcTimeToPOSIXSeconds tm) $ map fromString $ sort $ ordNub opts

-- | File contents inspection data
fileContentsInspection :: [String] -> IO Inspection
fileContentsInspection opts = fileContentsInspection_ opts <$> getPOSIXTime

-- | File contents inspection data
fileContentsInspection_ :: [String] -> POSIXTime -> Inspection
fileContentsInspection_ opts tm = InspectionAt tm $ map fromString $ sort $ ordNub opts

-- | Installed module inspection data, just opts
installedInspection :: [String] -> IO Inspection
installedInspection opts = return $ InspectionAt 0 $ map fromString $ sort $ ordNub opts

-- | Inspection by module location
moduleInspection :: ModuleLocation -> [String] -> IO Inspection
moduleInspection (FileModule fpath _) = fileInspection fpath
moduleInspection _ = installedInspection

-- | Enumerate project dirs
projectDirs :: Project -> IO [Extensions Path]
projectDirs p = do
	p' <- loadProject p
	return $ ordNub $ map (fmap (normPath . (view projectPath p' `subPath`))) $ maybe [] sourceDirs $ view projectDescription p'

-- | Enumerate project source files
projectSources :: Project -> IO [Extensions Path]
projectSources p = do
	dirs <- projectDirs p
	let
		enumCabals = liftM (map takeDirectory . filter cabalFile) . traverseDirectory
		dirs' = map (view (entity . path)) dirs
	-- enum inner projects and dont consider them as part of this project
	subProjs <- liftM (map fromFilePath . delete (view (projectPath . path) p) . ordNub . concat) $ triesMap (enumCabals) dirs'
	let
		enumHs = liftM (filter thisProjectSource) . traverseDirectory
		thisProjectSource h = haskellSource h && not (any (`isParent` fromFilePath h) subProjs)
	liftM (ordNub . concat) $ triesMap (liftM sequenceA . traverse (liftM (map fromFilePath) . enumHs . view path)) dirs

-- | Get actual defines
getDefines :: IO [(String, String)]
getDefines = E.handle onIO $ do
	tmp <- Dir.getTemporaryDirectory
	writeFile (tmp </> "defines.hs") ""
	_ <- runWait "ghc" ["-E", "-optP-dM", "-cpp", tmp </> "defines.hs"] ""
	cts <- readFileUtf8 (tmp </> "defines.hspp")
	Dir.removeFile (tmp </> "defines.hs")
	Dir.removeFile (tmp </> "defines.hspp")
	return $ mapMaybe (\g -> (,) <$> g 1 <*> g 2) $ mapMaybe (matchRx rx . T.unpack) $ T.lines cts
	where
		rx = "#define ([^\\s]+) (.*)"
		onIO :: E.IOException -> IO [(String, String)]
		onIO _ = return []

preprocess :: [(String, String)] -> Path -> Text -> IO Text
preprocess defines fpath cts = do
	cts' <- E.catch (Cpphs.cppIfdef (view path fpath) defines [] cppOpts (T.unpack cts)) onIOError
	return $ T.unlines $ map (fromString . snd) cts'
	where
		onIOError :: E.IOException -> IO [(Cpphs.Posn, String)]
		onIOError _ = return []

		cppOpts = Cpphs.defaultBoolOptions {
			Cpphs.locations = False,
			Cpphs.hashline = False
		}

preprocess_ :: [(String, String)] -> [String] -> Path -> Text -> IO Text
preprocess_ defines exts fpath cts
	| hasCPP = preprocess defines fpath cts
	| otherwise = return cts
	where
		exts' = map H.parseExtension exts ++ maybe [] snd (H.readExtensions $ T.unpack cts)
		hasCPP = H.EnableExtension H.CPP `elem` exts'

makeLenses ''AnalyzeEnv
