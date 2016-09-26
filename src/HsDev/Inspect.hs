{-# LANGUAGE TypeSynonymInstances #-}

module HsDev.Inspect (
	analyzeModule, inspectDocsChunk, inspectDocs, inspectDocsGhc,
	inspectContents, contentsInspection,
	inspectFile, fileInspection,
	projectDirs, projectSources,
	inspectProject,
	getDefines,
	preprocess, preprocess_,

	module Control.Monad.Except
	) where

import Control.Arrow
import Control.Applicative
import Control.DeepSeq
import qualified Control.Exception as E
import Control.Lens (view, preview, set, over)
import Control.Lens.At (ix)
import Control.Monad
import Control.Monad.Except
import Data.Char (isSpace)
import Data.Function (on)
import Data.List
import Data.Map (Map)
import Data.Maybe (fromMaybe, mapMaybe, catMaybes, listToMaybe, isJust)
import Data.Ord (comparing)
import Data.String (IsString, fromString)
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, getPOSIXTime)
import qualified Data.Map as M
import qualified Language.Haskell.Exts as H
import qualified Language.Preprocessor.Cpphs as Cpphs
import qualified System.Directory as Dir
import System.FilePath
import Data.Generics.Uniplate.Data
import HDocs.Haddock

import HsDev.Error
import HsDev.Symbols
import HsDev.Tools.Base
import HsDev.Tools.Ghc.Worker ()
import HsDev.Tools.HDocs (hdocsy, hdocs, hdocsProcess)
import HsDev.Util

-- | Analize source contents
analyzeModule :: [String] -> Maybe FilePath -> String -> Either String Module
analyzeModule exts file source = case H.parseFileContentsWithMode (parseMode file exts) source' of
	H.ParseFailed loc reason -> Left $ "Parse failed at " ++ show loc ++ ": " ++ reason
	H.ParseOk (H.Module _ (Just (H.ModuleHead _ (H.ModuleName _ mname) _ mexports)) _ imports declarations) -> Right Module {
		_moduleName = fromString mname,
		_moduleDocs =  Nothing,
		_moduleLocation = ModuleSource Nothing,
		_moduleExports = fmap (concatMap getExports . getSpec) mexports,
		_moduleImports = map getImport imports,
		_moduleDeclarations = sortDeclarations $ getDecls declarations }
	_ -> Left "Unknown module"
	where
		-- Replace all tabs to spaces to make SrcLoc valid, otherwise it treats tab as 8 spaces
		source' = map untab source
		untab '\t' = ' '
		untab ch = ch
		getSpec (H.ExportSpecList _ es) = es

-- | Analize source contents
analyzeModule_ :: [String] -> Maybe FilePath -> String -> Either String Module
analyzeModule_ exts file source = do
	mname <- parseModuleName source'
	return Module {
		_moduleName = fromString mname,
		_moduleDocs = Nothing,
		_moduleLocation = ModuleSource Nothing,
		_moduleExports = do
			H.PragmasAndModuleHead _ _ mhead <- parseModuleHead' source'
			H.ModuleHead _ _ _ mexports <- mhead
			H.ExportSpecList _ exports <- mexports
			return $ concatMap getExports exports,
		_moduleImports = map getImport $ mapMaybe (uncurry parseImport') parts,
		_moduleDeclarations = sortDeclarations $ getDecls $ mapMaybe (uncurry parseDecl') parts }
	where
		parts :: [(Int, String)]
		parts = zip offsets (map unlines parts') where
			parts' :: [[String]]
			parts' = unfoldr break' $ lines source'
			offsets = scanl (+) 0 $ map length parts'
		break' :: [String] -> Maybe ([String], [String])
		break' [] = Nothing
		break' (l:ls) = Just $ first (l:) $ span (maybe True isSpace . listToMaybe) ls

		parseModuleName :: String -> Either String String
		parseModuleName cts = maybe (Left "match fail") Right $ do
			g <- matchRx "^module\\s+([\\w\\.]+)" cts
			g 1

		parseDecl' :: Int -> String -> Maybe (H.Decl H.SrcSpanInfo)
		parseDecl' offset cts = maybeResult $ fmap (transformBi $ addOffset offset) $
			H.parseDeclWithMode (parseMode file exts) cts

		parseImport' :: Int -> String -> Maybe (H.ImportDecl H.SrcSpanInfo)
		parseImport' offset cts = maybeResult $ fmap (transformBi $ addOffset offset) $
			H.parseImportDeclWithMode (parseMode file exts) cts

		parseModuleHead' :: String -> Maybe (H.PragmasAndModuleHead H.SrcSpanInfo)
		parseModuleHead' = maybeResult . fmap H.unNonGreedy . H.parseWithMode (parseMode file exts)

		maybeResult :: H.ParseResult a -> Maybe a
		maybeResult (H.ParseFailed _ _) = Nothing
		maybeResult (H.ParseOk r) = Just r

		addOffset :: Int -> H.SrcSpan -> H.SrcSpan
		addOffset offset src = src { H.srcSpanStartLine = H.srcSpanStartLine src + offset, H.srcSpanEndLine = H.srcSpanEndLine src + offset }

		-- Replace all tabs to spaces to make SrcLoc valid, otherwise it treats tab as 8 spaces
		source' = map untab source
		untab '\t' = ' '
		untab ch = ch

parseMode :: Maybe FilePath -> [String] -> H.ParseMode
parseMode file exts = H.defaultParseMode {
	H.parseFilename = fromMaybe (H.parseFilename H.defaultParseMode) file,
	H.baseLanguage = H.Haskell2010,
	H.extensions = H.glasgowExts ++ map H.parseExtension exts,
	H.fixities = Just H.baseFixities }

-- | Get exports
getExports :: H.ExportSpec H.SrcSpanInfo -> [Export]
getExports (H.EModuleContents _ (H.ModuleName _ m)) = [ExportModule $ fromString m]
getExports (H.EVar _ n) = [uncurry ExportName (identOfQName n) ThingNothing]
getExports (H.EAbs _ _ n) = [uncurry ExportName (identOfQName n) ThingNothing]
getExports (H.EThingWith _ _ n ns) = [uncurry ExportName (identOfQName n) $ ThingWith (map toStr ns)] where
	toStr :: H.CName H.SrcSpanInfo -> Text
	toStr (H.VarName _ cn) = identOfName cn
	toStr (H.ConName _ cn) = identOfName cn

-- | Get import
getImport :: H.ImportDecl H.SrcSpanInfo -> Import
getImport d = Import
	(mname (H.importModule d))
	(H.importQualified d)
	(mname <$> H.importAs d)
	(importLst <$> H.importSpecs d)
	(Just $ toPosition $ H.ann d)
	where
		mname (H.ModuleName _ n) = fromString n
		importLst (H.ImportSpecList _ hiding specs) = ImportList hiding $ map impSpec specs
		impSpec (H.IVar _ n) = ImportSpec (identOfName n) ThingNothing
		impSpec (H.IAbs _ _ n) = ImportSpec (identOfName n) ThingNothing
		impSpec (H.IThingAll _ n) = ImportSpec (identOfName n) ThingAll
		impSpec (H.IThingWith _ n ns) = ImportSpec (identOfName n) $ ThingWith $ map identOfName (concatMap childrenBi ns :: [H.Name H.SrcSpanInfo])

-- | Decl declarations
getDecls :: [H.Decl H.SrcSpanInfo] -> [Declaration]
getDecls decls =
	map mergeDecls .
	groupBy ((==) `on` view declarationName) .
	sortBy (comparing (view declarationName)) $
	concatMap getDecl decls ++ concatMap getDef decls
	where
		mergeDecls :: [Declaration] -> Declaration
		mergeDecls [] = error "Impossible"
		mergeDecls ds = Declaration
			(view declarationName $ head ds)
			Nothing
			Nothing
			(msum $ map (view declarationDocs) ds)
			(minimum <$> mapM (view declarationPosition) ds)
			(foldr1 mergeInfos $ map (view declaration) ds)

		mergeInfos :: DeclarationInfo -> DeclarationInfo -> DeclarationInfo
		mergeInfos (Function ln ld lr) (Function rn rd rr) = Function (ln `mplus` rn) (ld ++ rd) (lr `mplus` rr)
		mergeInfos l _ = l

-- | Get local binds
getLocalDecls :: H.Decl H.SrcSpanInfo -> [Declaration]
getLocalDecls decl' = concatMap getDecls' binds' where
	binds' :: [H.Binds H.SrcSpanInfo]
	binds' = universeBi decl'
	getDecls' :: H.Binds H.SrcSpanInfo -> [Declaration]
	getDecls' (H.BDecls _ decls) = getDecls decls
	getDecls' _ = []

-- | Get declaration and child declarations
getDecl :: H.Decl H.SrcSpanInfo -> [Declaration]
getDecl decl' = case decl' of
	H.TypeSig loc names typeSignature -> [mkFun loc n (Function (Just $ oneLinePrint typeSignature) [] Nothing) | n <- names]
	H.TypeDecl loc h _ -> [mkType loc (tyName h) Type (tyArgs h) `withDef` decl']
	H.DataDecl loc dataOrNew mctx h cons _ -> (mkType loc (tyName h) (ctor dataOrNew `withCtx` mctx) (tyArgs h) `withDef` decl') : concatMap (map (addRel $ tyName h) . getConDecl (tyName h) (tyArgs h)) cons
	H.GDataDecl loc dataOrNew mctx h _ gcons _ -> (mkType loc (tyName h) (ctor dataOrNew `withCtx` mctx) (tyArgs h) `withDef` decl') : concatMap (map (addRel $ tyName h) . getGConDecl) gcons
	H.ClassDecl loc ctx h _ _ -> [mkType loc (tyName h) (Class `withCtx` ctx) (tyArgs h) `withDef` decl']
	_ -> []
	where
		mkType :: H.SrcSpanInfo -> H.Name H.SrcSpanInfo -> (TypeInfo -> DeclarationInfo) -> [H.TyVarBind H.SrcSpanInfo] -> Declaration
		mkType loc n ctor' args = setPosition loc $ decl (identOfName n) $ ctor' $ TypeInfo Nothing (map oneLinePrint args) Nothing []

		withDef :: H.Pretty a => Declaration -> a -> Declaration
		withDef tyDecl' tyDef = set (declaration . typeInfo . typeInfoDefinition) (Just $ prettyPrint tyDef) tyDecl'

		withCtx :: (TypeInfo -> DeclarationInfo) -> Maybe (H.Context H.SrcSpanInfo) -> TypeInfo -> DeclarationInfo
		withCtx ctor' mctx = ctor' . set typeInfoContext (fmap makeCtx mctx)

		ctor :: H.DataOrNew H.SrcSpanInfo -> TypeInfo -> DeclarationInfo
		ctor (H.DataType _) = Data
		ctor (H.NewType _) = NewType

		makeCtx ctx = fromString $ oneLinePrint ctx

		addRel :: H.Name H.SrcSpanInfo -> Declaration -> Declaration
		addRel n = set (declaration . related) (Just $ identOfName n)

		tyName :: H.DeclHead H.SrcSpanInfo -> H.Name H.SrcSpanInfo
		tyName (H.DHead _ n) = n
		tyName (H.DHInfix _ _ n) = n
		tyName (H.DHParen _ h) = tyName h
		tyName (H.DHApp _ h _) = tyName h

		tyArgs :: H.DeclHead H.SrcSpanInfo -> [H.TyVarBind H.SrcSpanInfo]
		tyArgs = universeBi

-- | Get constructor and record fields declarations
getConDecl :: H.Name H.SrcSpanInfo -> [H.TyVarBind H.SrcSpanInfo] -> H.QualConDecl H.SrcSpanInfo -> [Declaration]
getConDecl t as (H.QualConDecl loc _ _ cdecl) = case cdecl of
	H.ConDecl _ n cts -> [mkFun loc n (Function (Just $ oneLinePrint $ cts `tyFun` dataRes) [] Nothing)]
	H.InfixConDecl _ ct n cts -> [mkFun loc n (Function (Just $ oneLinePrint $ (ct : [cts]) `tyFun` dataRes) [] Nothing)]
	H.RecDecl _ n fields -> mkFun loc n (Function (Just $ oneLinePrint $ map tyField fields `tyFun` dataRes) [] Nothing) : concatMap (getRec loc dataRes) fields
	where
		dataRes :: H.Type H.SrcSpanInfo
		dataRes = foldr (H.TyApp loc . H.TyVar loc . nameOf) (H.TyCon loc (H.UnQual loc t)) as where
			nameOf :: H.TyVarBind H.SrcSpanInfo -> H.Name H.SrcSpanInfo
			nameOf (H.KindedVar _ n' _) = n'
			nameOf (H.UnkindedVar _ n') = n'

tyField :: H.FieldDecl H.SrcSpanInfo -> H.Type H.SrcSpanInfo
tyField (H.FieldDecl _ _ t) = t

-- | Get GADT constructor and record fields declarations
getGConDecl :: H.GadtDecl H.SrcSpanInfo -> [Declaration]
getGConDecl (H.GadtDecl loc n mfields r) = mkFun loc n (Function (Just $ oneLinePrint $ map tyField (fromMaybe [] mfields) `tyFun` r) [] Nothing) : concatMap (getRec loc r) (fromMaybe [] mfields)

-- | Get record field declaration
getRec :: H.SrcSpanInfo -> H.Type H.SrcSpanInfo -> H.FieldDecl H.SrcSpanInfo -> [Declaration]
getRec loc t (H.FieldDecl _ ns rt) = [mkFun loc n (Function (Just $ oneLinePrint $ H.TyFun loc t rt) [] Nothing) | n <- ns]

-- | Get definitions
getDef :: H.Decl H.SrcSpanInfo -> [Declaration]
getDef (H.FunBind _ []) = []
getDef d@(H.FunBind _ (m : _)) = [setPosition (H.ann m) $ decl (identOfName $ identOfMatch m) fun] where
	identOfMatch (H.Match _ n _ _ _) = n
	identOfMatch (H.InfixMatch _ _ n _ _ _) = n
	fun = Function Nothing (getLocalDecls d) Nothing
getDef d@(H.PatBind loc pat _ _) = map (\name -> setPosition loc (decl (identOfName name) (Function Nothing (getLocalDecls d) Nothing))) (names pat) where
	names :: H.Pat H.SrcSpanInfo -> [H.Name H.SrcSpanInfo]
	names (H.PVar _ n) = [n]
	names (H.PNPlusK _ n _) = [n]
	names (H.PInfixApp _ l _ r) = names l ++ names r
	names (H.PApp _ _ ns) = concatMap names ns
	names (H.PTuple _ _ ns) = concatMap names ns
	names (H.PList _ ns) = concatMap names ns
	names (H.PParen _ n) = names n
	names (H.PRec _ _ pf) = concatMap fieldNames pf
	names (H.PAsPat _ n ns) = n : names ns
	names (H.PWildCard _) = []
	names (H.PIrrPat _ n) = names n
	names (H.PatTypeSig _ n _) = names n
	names (H.PViewPat _ _ n) = names n
	names (H.PBangPat _ n) = names n
	names _ = []

	fieldNames :: H.PatField H.SrcSpanInfo -> [H.Name H.SrcSpanInfo]
	fieldNames (H.PFieldPat _ _ n) = names n
	fieldNames (H.PFieldPun _ n) = case n of
		H.Qual _ _ n' -> [n']
		H.UnQual _ n' -> [n']
		_ -> []
	fieldNames (H.PFieldWildcard _) = []
getDef _ = []

-- | Make function declaration by location, name and function type
mkFun :: H.SrcSpanInfo -> H.Name H.SrcSpanInfo -> DeclarationInfo -> Declaration
mkFun loc n = setPosition loc . decl (identOfName n)

-- | Make function from arguments and result
--
-- @[a, b, c...] `tyFun` r == a `TyFun` b `TyFun` c ... `TyFun` r@
tyFun :: [H.Type H.SrcSpanInfo] -> H.Type H.SrcSpanInfo -> H.Type H.SrcSpanInfo
tyFun as' r' = foldr (H.TyFun H.noSrcSpan) r' as'

-- | Get name of qualified name
identOfQName :: H.QName H.SrcSpanInfo -> (Maybe Text, Text)
identOfQName (H.Qual _ (H.ModuleName _ mname) name) = (Just $ fromString mname, identOfName name)
identOfQName (H.UnQual _ name) = (Nothing, identOfName name)
identOfQName (H.Special _ sname) = (Nothing, fromString $ H.prettyPrint sname)

-- | Get name of @H.Name@
identOfName :: H.Name H.SrcSpanInfo -> Text
identOfName name = fromString $ case name of
	H.Ident _ s -> s
	H.Symbol _ s -> s

-- | Print something in one line
oneLinePrint :: (H.Pretty a, IsString s) => a -> s
oneLinePrint = fromString . H.prettyPrintStyleMode (H.style { H.mode = H.OneLineMode }) H.defaultMode

-- | Print something
prettyPrint :: (H.Pretty a, IsString s) => a -> s
prettyPrint = fromString . H.prettyPrintStyleMode (H.style { H.mode = H.PageMode }) mode' where
	mode' = H.PPHsMode {
		H.classIndent = 4,
		H.doIndent = 4,
		H.multiIfIndent = 4,
		H.caseIndent = 4,
		H.letIndent = 4,
		H.whereIndent = 4,
		H.onsideIndent = 2,
		H.spacing = False,
		H.layout = H.PPOffsideRule,
		H.linePragmas = False }

-- | Convert @H.SrcSpanInfo@ to @Position
toPosition :: H.SrcSpanInfo -> Position
toPosition s = Position (H.startLine s) (H.startColumn s)

-- | Set @Declaration@ position
setPosition :: H.SrcSpanInfo -> Declaration -> Declaration
setPosition loc = set declarationPosition (Just $ toPosition loc)

-- | Adds documentation to declaration
addDoc :: Map String String -> Declaration -> Declaration
addDoc docsMap decl' = set declarationDocs (preview (ix (view declarationName decl')) docsMap') decl' where
	docsMap' = M.mapKeys fromString . M.map fromString $ docsMap

-- | Adds documentation to all declarations in module
addDocs :: Map String String -> Module -> Module
addDocs docsMap = over moduleDeclarations (map $ addDoc docsMap)

-- | Extract files docs and set them to declarations
inspectDocsChunk :: [String] -> [Module] -> IO [Module]
inspectDocsChunk opts ms = hsdevLiftIOWith (ToolError "hdocs") $ do
	docsMaps <- hdocsy (map (view moduleLocation) ms) opts
	return $ zipWith addDocs docsMaps ms

-- | Extract file docs and set them to module declarations
inspectDocs :: [String] -> Module -> IO Module
inspectDocs opts m = do
	let
		hdocsWorkaround = False
	docsMap <- hsdevLiftIOWith (ToolError "hdocs") $ if hdocsWorkaround
		then hdocsProcess (fromMaybe (T.unpack $ view moduleName m) (preview (moduleLocation . moduleFile) m)) opts
		else liftM Just $ hdocs (view moduleLocation m) opts
	return $ maybe id addDocs docsMap m

-- | Like @inspectDocs@, but in @Ghc@ monad
inspectDocsGhc :: [String] -> Module -> Ghc Module
inspectDocsGhc opts m = case view moduleLocation m of
	FileModule fpath _ -> do
		docsMap <- liftM (fmap (formatDocs . snd) . listToMaybe) $ hsdevLift $ readSourcesGhc opts [fpath]
		return $ maybe id addDocs docsMap m
	_ -> hsdevError $ ModuleNotSource (view moduleLocation m)

-- | Inspect contents
inspectContents :: String -> [(String, String)] -> [String] -> String -> ExceptT String IO InspectedModule
inspectContents name defines opts cts = inspect (ModuleSource $ Just name) (contentsInspection cts opts) $ do
	cts' <- lift $ preprocess_ defines exts name cts
	analyzed <- ExceptT $ return $ analyzeModule exts (Just name) cts' <|> analyzeModule_ exts (Just name) cts'
	return $ set moduleLocation (ModuleSource $ Just name) analyzed
	where
		exts = mapMaybe flagExtension opts

contentsInspection :: String -> [String] -> ExceptT String IO Inspection
contentsInspection _ _ = return InspectionNone -- crc or smth

-- | Inspect file
inspectFile :: [(String, String)] -> [String] -> FilePath -> Maybe String -> IO InspectedModule
inspectFile defines opts file mcts = hsdevLiftIO $ do
	proj <- locateProject file
	absFilename <- Dir.canonicalizePath file
	ex <- Dir.doesFileExist absFilename
	unless ex $ hsdevError $ FileNotFound absFilename
	inspect (FileModule absFilename proj) ((if isJust mcts then fileContentsInspection else fileInspection) absFilename opts) $ do
		-- docsMap <- liftE $ if hdocsWorkaround
		-- 	then hdocsProcess absFilename opts
		-- 	else liftM Just $ hdocs (FileModule absFilename Nothing) opts
		forced <- hsdevLiftWith InspectError $ ExceptT $ E.handle onError $ do
			analyzed <- liftM (\s -> analyzeModule exts (Just absFilename) s <|> analyzeModule_ exts (Just absFilename) s) $
				maybe (readFileUtf8 absFilename >>= preprocess_ defines exts file) return mcts
			force analyzed `deepseq` return analyzed
		-- return $ setLoc absFilename proj . maybe id addDocs docsMap $ forced
		return $ set moduleLocation (FileModule absFilename proj) forced
	where
		onError :: E.ErrorCall -> IO (Either String Module)
		onError = return . Left . show

		exts = mapMaybe flagExtension opts

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

-- | Inspect project
inspectProject :: [(String, String)] -> [String] -> Project -> IO (Project, [InspectedModule])
inspectProject defines opts p = hsdevLiftIO $ do
	p' <- loadProject p
	srcs <- projectSources p'
	modules <- mapM inspectFile' srcs
	return (p', catMaybes modules)
	where
		inspectFile' exts = liftM return (inspectFile defines (opts ++ extensionsOpts exts) (view entity exts) Nothing) <|> return Nothing

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
