{-# LANGUAGE TypeSynonymInstances, ImplicitParams, TemplateHaskell #-}

module HsDev.Inspect (
	Preloaded(..), preloadedId, preloadedMode, preloadedModule, asModule, preloaded, preloadedImports, preload,
	AnalyzeEnv(..), analyzeEnv, analyzeFixities, analyzeRefine, moduleAnalyzeEnv,
	analyzeResolve, analyzePreloaded, inspectPreloaded, inspectDocs, inspectDocsGhc,
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
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Data (Data)
import Data.Function (on)
import Data.List
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
import Data.Ord (comparing)
import Data.String (IsString, fromString)
import qualified Data.Text as T (unpack, Text)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, getPOSIXTime)
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
import Data.Generics.Uniplate.Data
import HDocs.Haddock

import HsDev.Display ()
import HsDev.Error
import HsDev.Symbols
import HsDev.Symbols.Resolve (refineSymbol, refineTable, RefineTable, symbolUniqId)
import HsDev.Symbols.Parsed hiding (file)
import qualified HsDev.Symbols.HaskellNames as HN
import HsDev.Tools.Base
import HsDev.Tools.Ghc.Worker (GhcM)
import HsDev.Tools.HDocs (hdocs, hdocsProcess)
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
		fileExts = [H.parseExtension (T.unpack $ fromName_ $ void lang) | H.LanguagePragma _ langs <- pragmas, lang <- langs]
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


-- | Get top symbols
getSymbols :: [H.Decl Ann] -> [Symbol]
getSymbols decls =
	map mergeSymbols .
	groupBy ((==) `on` symbolUniqId) .
	sortBy (comparing symbolUniqId) $
	concatMap getDecl decls
	where
		mergeSymbols :: [Symbol] -> Symbol
		mergeSymbols [] = error "impossible"
		mergeSymbols [s] = s
		mergeSymbols ss@(s:_) = Symbol
			(view symbolId s)
			(msum $ map (view symbolDocs) ss)
			(msum $ map (view symbolPosition) ss)
			(foldr1 mergeInfo $ map (view symbolInfo) ss)

		mergeInfo :: SymbolInfo -> SymbolInfo -> SymbolInfo
		mergeInfo (Function lt) (Function rt) = Function $ lt `mplus` rt
		mergeInfo (PatConstructor las lt) (PatConstructor ras rt) = PatConstructor (if null las then ras else las) (lt `mplus` rt)
		mergeInfo (Selector lt lp lc) (Selector rt rp rc)
			| lt == rt && lp == rp = Selector lt lp (nub $ lc ++ rc)
			| otherwise = Selector lt lp lc
		mergeInfo l _ = l


-- | Get symbols from declarations
getDecl :: H.Decl Ann -> [Symbol]
getDecl decl' = case decl' of
	H.TypeDecl _ h _ -> [mkSymbol (tyName h) (Type (tyArgs h) [])]
	H.TypeFamDecl _ h _ _ -> [mkSymbol (tyName h) (TypeFam (tyArgs h) [] Nothing)]
	H.ClosedTypeFamDecl _ h _ _ _ -> [mkSymbol (tyName h) (TypeFam (tyArgs h) [] Nothing)]
	H.DataDecl _ dt mctx h dcons _ -> mkSymbol nm ((getCtor dt) (tyArgs h) (getCtx mctx)) : concatMap (getConDecl nm) dcons where
		nm = tyName h
	H.GDataDecl _ dt mctx h _ gcons _ -> mkSymbol nm ((getCtor dt) (tyArgs h) (getCtx mctx)) : concatMap (getGConDecl nm) gcons where
		nm = tyName h
	H.DataFamDecl _ mctx h _ -> [mkSymbol (tyName h) (DataFam (tyArgs h) (getCtx mctx) Nothing)]
	H.ClassDecl _ mctx h _ _ -> [mkSymbol (tyName h) (Class (tyArgs h) (getCtx mctx))]
	H.TypeSig _ ns tsig -> [mkSymbol n (Function (Just $ oneLinePrint tsig)) | n <- ns]
	H.PatSynSig _ n mas _ _ t -> [mkSymbol n (PatConstructor (maybe [] (map prp) mas) (Just $ oneLinePrint t))]
	H.FunBind _ ms -> [mkSymbol (matchName m) (Function Nothing) | m <- ms] where
		matchName (H.Match _ n _ _ _) = n
		matchName (H.InfixMatch _ _ n _ _ _) = n
	H.PatBind _ p _ _ -> [mkSymbol n (Function Nothing) | n <- patNames p] where
		patNames :: H.Pat Ann -> [H.Name Ann]
		patNames = childrenBi
	H.PatSyn _ p _ _ -> case p of
		H.PInfixApp _ _ qn _ -> [mkSymbol (qToName qn) (PatConstructor [] Nothing)]
		H.PApp _ qn _ -> [mkSymbol (qToName qn) (PatConstructor [] Nothing)]
		H.PRec _ qn fs -> mkSymbol (qToName qn) (PatConstructor [] Nothing) :
			[mkSymbol (qToName n) (PatSelector Nothing Nothing (prp $ qToName qn)) | n <- (universeBi fs :: [H.QName Ann])]
		_ -> []
		where
			qToName (H.Qual _ _ n) = n
			qToName (H.UnQual _ n) = n
			qToName _ = error "invalid qname"
	_ -> []
	where
		tyName :: H.DeclHead Ann -> H.Name Ann
		tyName = head . universeBi
		tyArgs :: Data (ast Ann) => ast Ann -> [T.Text]
		tyArgs n = map prp (universeBi n :: [H.TyVarBind Ann])
		getCtx :: Maybe (H.Context Ann) -> [T.Text]
		getCtx mctx = map prp (universeBi mctx :: [H.Asst Ann])
		getCtor (H.DataType _) = Data
		getCtor (H.NewType _) = NewType

getConDecl :: H.Name Ann -> H.QualConDecl Ann -> [Symbol]
getConDecl ptype (H.QualConDecl _ _ _ cdecl) = case cdecl of
	H.ConDecl _ n ts -> [mkSymbol n (Constructor (map prp ts) (prp ptype))]
	H.InfixConDecl _ lt n rt -> [mkSymbol n (Constructor (map prp [lt, rt]) (prp ptype))]
	H.RecDecl _ n fs -> mkSymbol n (Constructor [prp t | H.FieldDecl _ _ t <- fs] (prp ptype)) :
		[mkSymbol fn (Selector (Just $ prp ft) (prp ptype) [prp n]) | H.FieldDecl _ fns ft <- fs, fn <- fns]

getGConDecl :: H.Name Ann -> H.GadtDecl Ann -> [Symbol]
getGConDecl _ (H.GadtDecl _ n Nothing t) = [mkSymbol n (Constructor (map prp as) (prp res))] where
	(as, res) = tyFunSplit t
	tyFunSplit = go [] where
		go as' (H.TyFun _ arg' res') = go (arg' : as') res'
		go as' t' = (reverse as', t')
getGConDecl ptype (H.GadtDecl _ n (Just fs) t) = mkSymbol n (Constructor [prp ft | H.FieldDecl _ _ ft <- fs] (prp t)) :
	[mkSymbol fn (Selector (Just $ prp ft) (prp ptype) [prp n]) | H.FieldDecl _ fns ft <- fs, fn <- fns]


prp :: H.Pretty a => a -> T.Text
prp = fromString . H.prettyPrint


mkSymbol :: H.Name Ann -> SymbolInfo -> Symbol
mkSymbol nm = Symbol (SymbolId (fromName_ $ void nm) (ModuleId (fromString "") noLocation)) Nothing (nm ^? binders . defPos)


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

-- | Extract file docs and set them to module declarations
inspectDocs :: [String] -> Module -> GhcM Module
inspectDocs opts m = do
	let
		hdocsWorkaround = False
	docsMap <- if hdocsWorkaround
		then liftIO $ hdocsProcess (fromMaybe (T.unpack $ view (moduleId . moduleName) m) (preview (moduleId . moduleLocation . moduleFile) m)) opts
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

dropScope :: Functor f => f (N.Scoped l) -> f l
dropScope = fmap (\(N.Scoped _ a) -> a)

makeLenses ''Preloaded
makeLenses ''AnalyzeEnv
