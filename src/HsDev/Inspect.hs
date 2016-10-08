{-# LANGUAGE TypeSynonymInstances, ImplicitParams, TemplateHaskell #-}

module HsDev.Inspect (
	Preloaded(..), preloadedId, preloadedMode, preloadedModule, asModule, preloaded, preloadedImports, preload,
	AnalyzeEnv(..), analyzeEnv, analyzeFixities, analyzeRefine, moduleAnalyzeEnv,
	analyzeResolve, analyzePreloaded, inspectPreloaded, inspectDocsChunk, inspectDocs, inspectDocsGhc,
	inspectContents, contentsInspection,
	inspectFile, sourceInspection, fileInspection, fileContentsInspection,
	projectDirs, projectSources,
	getDefines,
	preprocess, preprocess_,

	module Control.Monad.Except
	) where

import Control.DeepSeq
import qualified Control.Exception as E
import Control.Lens hiding ((%=), (.=), universe)
import Control.Lens.At (ix)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Data (Data)
import Data.List
import Data.Map (Map)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
import Data.String (IsString, fromString)
import qualified Data.Text as T (unpack)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, getPOSIXTime)
import qualified Data.Map as M
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
import Data.Generics.Uniplate.Data
import HDocs.Haddock

import HsDev.Display ()
import HsDev.Error
import HsDev.Symbols
import HsDev.Symbols.Resolve (refineSymbol, refineTable, RefineTable)
import HsDev.Symbols.Parsed hiding (file)
import qualified HsDev.Symbols.HaskellNames as HN
import HsDev.Tools.Base
import HsDev.Tools.Ghc.Worker ()
import HsDev.Tools.HDocs (hdocsy, hdocs, hdocsProcess)
import HsDev.Util

-- | Preloaded module with contents and extensions
data Preloaded = Preloaded {
	_preloadedId :: ModuleId,
	_preloadedMode :: H.ParseMode,
	_preloadedModule :: H.Module H.SrcSpanInfo,
	-- ^ Loaded module head without declarations
	_preloaded :: String }

instance NFData Preloaded where
	rnf (Preloaded mid _ _ cts) = rnf mid `seq` rnf cts

asModule :: Lens' Preloaded Module
asModule = lens g' s' where
	g' p = Module {
		_moduleId = _preloadedId p,
		_moduleDocs = Nothing,
		_moduleExports = mempty,
		_moduleFixities = mempty,
		_moduleScope = mempty,
		_moduleSource = Just $ fmap (N.Scoped N.None) $ _preloadedModule p }
	s' p m = p {
		_preloadedId = _moduleId m,
		_preloadedModule = maybe (_preloadedModule p) dropScope (_moduleSource m) }

preloadedImports :: Preloaded -> [String]
preloadedImports p = [nm | H.ModuleName _ nm <- map H.importModule mimps] where
	H.Module _ _ _ mimps _ = _preloadedModule p

-- | Preload module - load head and imports to get actual extensions and dependencies
preload :: String -> [(String, String)] -> [String] -> ModuleLocation -> Maybe String -> IO Preloaded
preload name defines opts mloc@(FileModule fpath mproj) Nothing = do
	cts <- readFileUtf8 fpath
	let
		srcExts = fromMaybe (takeDirectory fpath `withExtensions` mempty) $ do
			proj <- mproj
			findSourceDir proj fpath
	preload name defines (opts ++ extensionsOpts srcExts) mloc (Just cts)
preload _ _ _ mloc Nothing = hsdevError $ InspectError $
	format "preload called non-sourced module: {}" ~~ mloc
preload name defines opts mloc (Just cts) = do
	cts' <- preprocess_ defines exts fpath $ map untab cts
	pragmas <- parseOk $ H.getTopPragmas cts'
	let
		fileExts = [H.parseExtension (T.unpack $ fromName_ $ fvoid lang) | H.LanguagePragma _ langs <- pragmas, lang <- langs]
		pmode = H.ParseMode {
			H.parseFilename = fpath,
			H.baseLanguage = H.Haskell2010,
			H.extensions = ordNub (map H.parseExtension exts ++ fileExts),
			H.ignoreLanguagePragmas = False,
			H.ignoreLinePragmas = True,
			H.fixities = Nothing,
			H.ignoreFunctionArity = False }
	H.ModuleHeadAndImports l mpragmas mhead mimps <- parseOk $ fmap H.unNonGreedy $ H.parseWithMode pmode cts'
	let
		mname = case mhead of
			Just (H.ModuleHead _ (H.ModuleName _ nm) _ _) -> nm
			_
				| haskellSource fpath -> moduleNameByFile fpath (mloc ^? moduleProject . _Just)
				| otherwise -> name
	return $ Preloaded {
		_preloadedId = ModuleId (fromString mname) mloc,
		_preloadedMode = pmode,
		_preloadedModule = H.Module l mhead mpragmas mimps [],
		_preloaded = cts' }
	where
		fpath = fromMaybe name (mloc ^? moduleFile)
		parseOk :: H.ParseResult a -> IO a
		parseOk (H.ParseOk v) = return v
		parseOk (H.ParseFailed loc err) = hsdevError $ InspectError $
			format "Parse {} failed at {} with: {}" ~~ fpath ~~ show loc ~~ err
		untab '\t' = ' '
		untab ch = ch
		exts = mapMaybe flagExtension opts

data AnalyzeEnv = AnalyzeEnv {
	_analyzeEnv :: N.Environment,
	_analyzeFixities :: M.Map Name H.Fixity,
	_analyzeRefine :: RefineTable }

instance Monoid AnalyzeEnv where
	mempty = AnalyzeEnv mempty mempty mempty
	AnalyzeEnv lenv lf lt `mappend` AnalyzeEnv renv rf rt = AnalyzeEnv
		(mappend lenv renv)
		(mappend lf rf)
		(mappend lt rt)

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
		_moduleExports = map HN.fromSymbol $ N.exportedSymbols tbl msrc,
		_moduleFixities = [Fixity (fvoid assoc) (fromMaybe 0 pr) (fixName opName)
			| H.InfixDecl _ assoc pr ops <- universeBi annotated :: [H.Decl Ann], opName <- childrenBi ops :: [H.Name Ann]],
		_moduleScope = M.map (map HN.fromSymbol) tbl,
		_moduleSource = Just annotated }
		where
			fixName o = H.Qual () (H.ModuleName () (T.unpack $ m ^. moduleId . moduleName)) (fvoid o)
			itbl = N.importTable env msrc
			defs = N.moduleSymbols itbl msrc
			tbl = N.moduleTable itbl msrc
			syms = set (each . symbolId . symbolModule) (m ^. moduleId) $
				flip execState (map HN.fromSymbol defs) $ examine annotated
			stbl = refineTable syms `mappend` rtable
			-- Not using 'annotate' because we already computed needed tables
			annotated = H.Module l mhead' mpragmas idecls' decls' where
				H.Module l mhead mpragmas idecls decls = fmap (\(N.Scoped _ v) -> N.Scoped N.None v) msrc
				mhead' = fmap scopeHead mhead
				scopeHead (H.ModuleHead lh mname mwarns mexports) = H.ModuleHead lh mname mwarns $
					fmap (N.annotateExportSpecList tbl . dropScope) mexports
				idecls' = N.annotateImportDecls mn env (fmap dropScope idecls)
				decls' = map (N.annotateDecl (N.initialScope (N.dropAnn mn) tbl) . dropScope) decls
				mn = dropScope $ N.getModuleName msrc

-- | Inspect preloaded module
analyzePreloaded :: AnalyzeEnv -> Preloaded -> Either String Module
analyzePreloaded aenv@(AnalyzeEnv env gfixities _) p = case H.parseFileContentsWithMode (_preloadedMode p') (_preloaded p') of
	H.ParseFailed loc reason -> Left $ "Parse failed at " ++ show loc ++ ": " ++ reason
	H.ParseOk m -> Right $ analyzeResolve aenv $ Module {
		_moduleId = _preloadedId p',
		_moduleDocs = Nothing,
		_moduleExports = mempty,
		_moduleFixities = mempty,
		_moduleScope = mempty,
		_moduleSource = Just $ fmap (N.Scoped N.None) m }
	where
		qimps = M.keys $ N.importTable env (_preloadedModule p)
		p' = p { _preloadedMode = (_preloadedMode p) { H.fixities = Just (mapMaybe (`M.lookup` gfixities) qimps) } }

-- | Same as 'analyzePreloaded', but throws on error
inspectPreloaded :: AnalyzeEnv -> Preloaded -> IO Module
inspectPreloaded aenv = either (hsdevError . InspectError) return . analyzePreloaded aenv

-- | Try extract some additional info and set it to symbol
examine :: H.Module Ann -> State [Symbol] ()
examine (H.Module _ _ _ _ decls) = mapM_ (ex examineDecl) decls where
	examineDecl :: H.Decl Ann -> State [Symbol] (SymbolInfo -> SymbolInfo)
	examineDecl (H.TypeSig _ _ t) = return $ set functionType (Just $ oneLinePrint t)
	examineDecl (H.PatSyn _ p _ _) = return $ set typeArgs as where
		as = map oneLinePrint [n | H.PVar _ n <- universe p]
	examineDecl (H.TypeDecl _ h _) = return $ set typeArgs (map oneLinePrint $ tyArgs h)
	examineDecl (H.DataDecl _ _ mctx h cns _) = do
		mapM_ (ex examineQualCon) cns
		return $
			set typeArgs (map oneLinePrint $ tyArgs h) .
			set typeContext (map oneLinePrint $ maybe [] tyAssts mctx)
	examineDecl (H.GDataDecl _ _ mctx h _ gcons _) = do
		mapM_ (ex examineGadtDecl) gcons
		return $
			set typeArgs (map oneLinePrint $ tyArgs h) .
			set typeContext (map oneLinePrint $ maybe [] tyAssts mctx)
	examineDecl (H.ClassDecl _ mctx h _ _) = return $
		set typeArgs (map oneLinePrint $ tyArgs h) .
		set typeContext (map oneLinePrint $ maybe [] tyAssts mctx)
	examineDecl _ = return id

	examineQualCon :: H.QualConDecl Ann -> State [Symbol] (SymbolInfo -> SymbolInfo)
	examineQualCon (H.QualConDecl _ _ mctx cdecl) = do
		ex examineConDecl cdecl
		return $ set typeContext (map oneLinePrint $ maybe [] tyAssts mctx)

	examineGadtDecl :: H.GadtDecl Ann -> State [Symbol] (SymbolInfo -> SymbolInfo)
	examineGadtDecl (H.GadtDecl _ _ mfields t) = do
		mapM_ (ex examineFieldDecl) $ fromMaybe [] mfields
		let
			as = maybe (args t) (\fs -> [ft | H.FieldDecl _ _ ft <- fs]) mfields
			args :: H.Type Ann -> [H.Type Ann]
			args (H.TyFun _ x y) = x : args y
			args _ = []
		return $ set typeArgs (map oneLinePrint as)

	examineConDecl :: H.ConDecl Ann -> State [Symbol] (SymbolInfo -> SymbolInfo)
	examineConDecl (H.ConDecl _ _ ts) = return $ set typeArgs (map oneLinePrint ts)
	examineConDecl (H.InfixConDecl _ lt _ rt) = return $ set typeArgs (map oneLinePrint [lt, rt])
	examineConDecl (H.RecDecl _ _ fields) = do
		mapM_ (ex examineFieldDecl) fields
		return $ set typeArgs (map oneLinePrint [t | H.FieldDecl _ _ t <- fields])

	examineFieldDecl :: H.FieldDecl Ann -> State [Symbol] (SymbolInfo -> SymbolInfo)
	examineFieldDecl (H.FieldDecl _ _ t) = return $ \s -> set functionType
		(Just $ format "{} -> {}" ~~ view parentType s ~~ (oneLinePrint t :: String)) s

	tyArgs :: H.DeclHead Ann -> [H.TyVarBind Ann]
	tyArgs = universeBi
	tyAssts :: H.Context Ann -> [H.Asst Ann]
	tyAssts = universeBi

	ex :: (H.Annotated ast, Data (ast Ann)) => (ast Ann -> State [Symbol] (SymbolInfo -> SymbolInfo)) -> ast Ann -> State [Symbol] ()
	ex fn node = do
		modifier <- fn node
		let
			upd = set symbolPosition (node ^? defPos) . over symbolInfo modifier
		case node ^? binders of
			Nothing -> return ()
			Just n' -> modify $ over (each . filtered (((oneLinePrint n') ==) . view sourcedName)) upd

examine _ = return ()

-- | Print something in one line
oneLinePrint :: (H.Pretty a, IsString s) => a -> s
oneLinePrint = fromString . H.prettyPrintStyleMode (H.style { H.mode = H.OneLineMode }) H.defaultMode

-- | Adds documentation to declaration
addDoc :: Map String String -> Symbol -> Symbol
addDoc docsMap sym' = set symbolDocs (preview (ix (view (symbolId . symbolName) sym')) docsMap') sym' where
	docsMap' = M.mapKeys fromString . M.map fromString $ docsMap

-- | Adds documentation to all declarations in module
addDocs :: Map String String -> Module -> Module
addDocs docsMap = over moduleSymbols (addDoc docsMap)

-- | Extract files docs and set them to declarations
inspectDocsChunk :: [String] -> [Module] -> IO [Module]
inspectDocsChunk opts ms = hsdevLiftIOWith (ToolError "hdocs") $ do
	docsMaps <- hdocsy (map (view (moduleId . moduleLocation)) ms) opts
	return $ zipWith addDocs docsMaps ms

-- | Extract file docs and set them to module declarations
inspectDocs :: [String] -> Module -> IO Module
inspectDocs opts m = do
	let
		hdocsWorkaround = False
	docsMap <- hsdevLiftIOWith (ToolError "hdocs") $ if hdocsWorkaround
		then hdocsProcess (fromMaybe (T.unpack $ view (moduleId . moduleName) m) (preview (moduleId . moduleLocation . moduleFile) m)) opts
		else liftM Just $ hdocs (view (moduleId . moduleLocation) m) opts
	return $ maybe id addDocs docsMap m

-- | Like @inspectDocs@, but in @Ghc@ monad
inspectDocsGhc :: [String] -> Module -> Ghc Module
inspectDocsGhc opts m = case view (moduleId . moduleLocation) m of
	FileModule fpath _ -> do
		docsMap <- liftM (fmap (formatDocs . snd) . listToMaybe) $ hsdevLift $ readSourcesGhc opts [fpath]
		return $ maybe id addDocs docsMap m
	_ -> hsdevError $ ModuleNotSource (view (moduleId . moduleLocation) m)

-- | Inspect contents
inspectContents :: String -> [(String, String)] -> [String] -> String -> ExceptT String IO InspectedModule
inspectContents name defines opts cts = inspect (OtherLocation name) (contentsInspection cts opts) $ do
	p <- lift $ preload name defines opts (OtherLocation name) (Just cts)
	analyzed <- ExceptT $ return $ analyzePreloaded mempty p
	return $ set (moduleId . moduleLocation) (OtherLocation name) analyzed

contentsInspection :: String -> [String] -> ExceptT String IO Inspection
contentsInspection _ _ = return InspectionNone -- crc or smth

-- | Inspect file
inspectFile :: [(String, String)] -> [String] -> FilePath -> Maybe Project -> Maybe String -> IO InspectedModule
inspectFile defines opts file mproj mcts = hsdevLiftIO $ do
	absFilename <- Dir.canonicalizePath file
	ex <- Dir.doesFileExist absFilename
	unless ex $ hsdevError $ FileNotFound absFilename
	inspect (FileModule absFilename mproj) (sourceInspection absFilename mcts opts) $ do
		-- docsMap <- liftE $ if hdocsWorkaround
		-- 	then hdocsProcess absFilename opts
		-- 	else liftM Just $ hdocs (FileModule absFilename Nothing) opts
		forced <- hsdevLiftWith InspectError $ ExceptT $ E.handle onError $ do
			p <- preload absFilename defines opts (FileModule absFilename mproj) mcts
			return $!! analyzePreloaded mempty p
		-- return $ setLoc absFilename mproj . maybe id addDocs docsMap $ forced
		return $ set (moduleId . moduleLocation) (FileModule absFilename mproj) forced
	where
		onError :: E.ErrorCall -> IO (Either String Module)
		onError = return . Left . show

-- | Source inspection data, differs whether there are contents provided
sourceInspection :: FilePath -> Maybe String -> [String] -> IO Inspection
sourceInspection f Nothing = fileInspection f
sourceInspection f (Just _) = fileContentsInspection f

-- | File inspection data
fileInspection :: FilePath -> [String] -> IO Inspection
fileInspection f opts = do
	tm <- Dir.getModificationTime f
	return $ InspectionAt (utcTimeToPOSIXSeconds tm) $ sort $ ordNub opts

-- | File contents inspection data
fileContentsInspection :: FilePath -> [String] -> IO Inspection
fileContentsInspection _ opts = do
	tm <- getPOSIXTime
	return $ InspectionAt tm $ sort $ ordNub opts

-- | Enumerate project dirs
projectDirs :: Project -> IO [Extensions FilePath]
projectDirs p = do
	p' <- loadProject p
	return $ ordNub $ map (fmap (normalise . (view projectPath p' </>))) $ maybe [] sourceDirs $ view projectDescription p'

-- | Enumerate project source files
projectSources :: Project -> IO [Extensions FilePath]
projectSources p = do
	dirs <- projectDirs p
	let
		enumCabals = liftM (map takeDirectory . filter cabalFile) . traverseDirectory
		dirs' = map (view entity) dirs
	-- enum inner projects and dont consider them as part of this project
	subProjs <- liftM (delete (view projectPath p) . ordNub . concat) $ triesMap (enumCabals) dirs'
	let
		enumHs = liftM (filter thisProjectSource) . traverseDirectory
		thisProjectSource h = haskellSource h && not (any (`isParent` h) subProjs)
	liftM (ordNub . concat) $ triesMap (liftM sequenceA . traverse (enumHs)) dirs

-- | Get actual defines
getDefines :: IO [(String, String)]
getDefines = E.handle onIO $ do
	tmp <- Dir.getTemporaryDirectory
	writeFile (tmp </> "defines.hs") ""
	_ <- runWait "ghc" ["-E", "-optP-dM", "-cpp", tmp </> "defines.hs"] ""
	cts <- readFileUtf8 (tmp </> "defines.hspp")
	Dir.removeFile (tmp </> "defines.hs")
	Dir.removeFile (tmp </> "defines.hspp")
	return $ mapMaybe (\g -> (,) <$> g 1 <*> g 2) $ mapMaybe (matchRx rx) $ lines cts
	where
		rx = "#define ([^\\s]+) (.*)"
		onIO :: E.IOException -> IO [(String, String)]
		onIO _ = return []

preprocess :: [(String, String)] -> FilePath -> String -> IO String
preprocess defines fpath cts = do
	cts' <- E.catch (Cpphs.cppIfdef fpath defines [] Cpphs.defaultBoolOptions cts) onIOError
	return $ unlines $ map snd cts'
	where
		onIOError :: E.IOException -> IO [(Cpphs.Posn, String)]
		onIOError _ = return []

preprocess_ :: [(String, String)] -> [String] -> FilePath -> String -> IO String
preprocess_ defines exts fpath cts
	| hasCPP = preprocess defines fpath cts
	| otherwise = return cts
	where
		exts' = map H.parseExtension exts ++ maybe [] snd (H.readExtensions cts)
		hasCPP = H.EnableExtension H.CPP `elem` exts'

fvoid :: Functor f => f a -> f ()
fvoid = fmap (const ())

dropScope :: Functor f => f (N.Scoped l) -> f l
dropScope = fmap (\(N.Scoped _ a) -> a)

makeLenses ''Preloaded
makeLenses ''AnalyzeEnv
