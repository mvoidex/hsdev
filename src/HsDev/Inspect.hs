{-# LANGUAGE TypeSynonymInstances #-}

module HsDev.Inspect (
	analyzeModule, inspectDocsChunk, inspectDocs, inspectDocsGhc,
	inspectContents, contentsInspection,
	inspectFile, fileInspection,
	projectDirs, projectSources,
	inspectProject,

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
import qualified System.Directory as Dir
import System.FilePath
import Data.Generics.Uniplate.Data
import HDocs.Haddock

import HsDev.Symbols
import HsDev.Tools.Base
import HsDev.Tools.HDocs (hdocsy, hdocs, hdocsProcess)
import HsDev.Util

-- | Analize source contents
analyzeModule :: [String] -> Maybe FilePath -> String -> Either String Module
analyzeModule exts file source = case H.parseFileContentsWithMode (parseMode file exts) source' of
		H.ParseFailed loc reason -> Left $ "Parse failed at " ++ show loc ++ ": " ++ reason
		H.ParseOk (H.Module _ (H.ModuleName mname) _ _ mexports imports declarations) -> Right Module {
			_moduleName = fromString mname,
			_moduleDocs =  Nothing,
			_moduleLocation = ModuleSource Nothing,
			_moduleExports = fmap (concatMap getExports) mexports,
			_moduleImports = map getImport imports,
			_moduleDeclarations = sortDeclarations $ getDecls declarations }
	where
		-- Replace all tabs to spaces to make SrcLoc valid, otherwise it treats tab as 8 spaces
		source' = map untab source
		untab '\t' = ' '
		untab ch = ch

-- | Analize source contents
analyzeModule_ :: [String] -> Maybe FilePath -> String -> Either String Module
analyzeModule_ exts file source = do
	mname <- parseModuleName source'
	return Module {
		_moduleName = fromString mname,
		_moduleDocs = Nothing,
		_moduleLocation = ModuleSource Nothing,
		_moduleExports = Nothing,
		_moduleImports = [],
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

		parseDecl' :: Int -> String -> Maybe H.Decl
		parseDecl' offset cts = fmap (transformBi addOffset) $ case H.parseDeclWithMode (parseMode file exts) cts of
			H.ParseFailed _ _ -> Nothing
			H.ParseOk decl' -> Just decl'
			where
				addOffset :: H.SrcLoc -> H.SrcLoc
				addOffset src = src { H.srcLine = H.srcLine src + offset }

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
getExports :: H.ExportSpec -> [Export]
getExports (H.EModuleContents (H.ModuleName m)) = [ExportModule $ fromString m]
getExports (H.EVar n) = [uncurry ExportName (identOfQName n) ExportNothing]
getExports (H.EAbs _ n) = [uncurry ExportName (identOfQName n) ExportNothing]
getExports (H.EThingAll n) = [uncurry ExportName (identOfQName n) ExportAll]
getExports (H.EThingWith n ns) = [uncurry ExportName (identOfQName n) $ ExportWith (map toStr ns)] where
	toStr :: H.CName -> Text
	toStr (H.VarName cn) = identOfName cn
	toStr (H.ConName cn) = identOfName cn

-- | Get import
getImport :: H.ImportDecl -> Import
getImport d = Import
	(mname (H.importModule d))
	(H.importQualified d)
	(mname <$> H.importAs d)
	(importLst <$> H.importSpecs d)
	(Just $ toPosition $ H.importLoc d)
	where
		mname (H.ModuleName n) = fromString n
		importLst (hiding, specs) = ImportList hiding $ map identOfName (concatMap childrenBi specs :: [H.Name])

-- | Decl declarations
getDecls :: [H.Decl] -> [Declaration]
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

-- | Get definitions
getBinds :: Maybe H.Binds -> [Declaration]
getBinds (Just (H.BDecls decls)) = getDecls decls
getBinds _ = []

-- | Get declaration and child declarations
getDecl :: H.Decl -> [Declaration]
getDecl decl' = case decl' of
	H.TypeSig loc names typeSignature -> [mkFun loc n (Function (Just $ oneLinePrint typeSignature) [] Nothing) | n <- names]
	H.TypeDecl loc n args _ -> [mkType loc n Type args `withDef` decl']
	H.DataDecl loc dataOrNew ctx n args cons _ -> (mkType loc n (ctor dataOrNew `withCtx` ctx) args `withDef` decl') : concatMap (map (addRel n) . getConDecl n args) cons
	H.GDataDecl loc dataOrNew ctx n args _ gcons _ -> (mkType loc n (ctor dataOrNew `withCtx` ctx) args `withDef` decl') : concatMap (map (addRel n) . getGConDecl) gcons
	H.ClassDecl loc ctx n args _ _ -> [mkType loc n (Class `withCtx` ctx) args `withDef` decl']
	_ -> []
	where
		mkType :: H.SrcLoc -> H.Name -> (TypeInfo -> DeclarationInfo) -> [H.TyVarBind] -> Declaration
		mkType loc n ctor' args = setPosition loc $ decl (identOfName n) $ ctor' $ TypeInfo Nothing (map oneLinePrint args) Nothing []

		withDef :: H.Pretty a => Declaration -> a -> Declaration
		withDef tyDecl' tyDef = set (declaration . typeInfo . typeInfoDefinition) (Just $ prettyPrint tyDef) tyDecl'

		withCtx :: (TypeInfo -> DeclarationInfo) -> H.Context -> TypeInfo -> DeclarationInfo
		withCtx ctor' ctx = ctor' . set typeInfoContext (makeCtx ctx)

		ctor :: H.DataOrNew -> TypeInfo -> DeclarationInfo
		ctor H.DataType = Data
		ctor H.NewType = NewType

		makeCtx [] = Nothing
		makeCtx ctx = Just $ fromString $ intercalate ", " $ map oneLinePrint ctx

		addRel :: H.Name -> Declaration -> Declaration
		addRel n = set (declaration . related) (Just $ identOfName n)

-- | Get constructor and record fields declarations
getConDecl :: H.Name -> [H.TyVarBind] -> H.QualConDecl -> [Declaration]
getConDecl t as (H.QualConDecl loc _ _ cdecl) = case cdecl of
	H.ConDecl n cts -> [mkFun loc n (Function (Just $ oneLinePrint $ cts `tyFun` dataRes) [] Nothing)]
	H.InfixConDecl ct n cts -> [mkFun loc n (Function (Just $ oneLinePrint $ (ct : [cts]) `tyFun` dataRes) [] Nothing)]
	H.RecDecl n fields -> mkFun loc n (Function (Just $ oneLinePrint $ map snd fields `tyFun` dataRes) [] Nothing) : concatMap (uncurry (getRec loc dataRes)) fields
	where
		dataRes :: H.Type
		dataRes = foldr (H.TyApp . H.TyVar . nameOf) (H.TyCon (H.UnQual t)) as where
			nameOf :: H.TyVarBind -> H.Name
			nameOf (H.KindedVar n' _) = n'
			nameOf (H.UnkindedVar n') = n'

-- | Get GADT constructor and record fields declarations
getGConDecl :: H.GadtDecl -> [Declaration]
getGConDecl (H.GadtDecl loc n fields r) = mkFun loc n (Function (Just $ oneLinePrint $ map snd fields `tyFun` r) [] Nothing) : concatMap (uncurry (getRec loc r)) fields where

-- | Get record field declaration
getRec :: H.SrcLoc -> H.Type -> [H.Name] -> H.Type -> [Declaration]
getRec loc t ns rt = [mkFun loc n (Function (Just $ oneLinePrint $ t `H.TyFun` rt) [] Nothing) | n <- ns]

-- | Get definitions
getDef :: H.Decl -> [Declaration]
getDef (H.FunBind []) = []
getDef (H.FunBind matches@(H.Match loc n _ _ _ _ : _)) = [setPosition loc $ decl (identOfName n) fun] where
	fun = Function Nothing (concatMap (getBinds . matchBinds) matches) Nothing
	matchBinds (H.Match _ _ _ _ _ binds) = binds
getDef (H.PatBind loc pat _ binds) = map (\name -> setPosition loc (decl (identOfName name) (Function Nothing (getBinds binds) Nothing))) (names pat) where
	names :: H.Pat -> [H.Name]
	names (H.PVar n) = [n]
	names (H.PNPlusK n _) = [n]
	names (H.PInfixApp l _ r) = names l ++ names r
	names (H.PApp _ ns) = concatMap names ns
	names (H.PTuple _ ns) = concatMap names ns
	names (H.PList ns) = concatMap names ns
	names (H.PParen n) = names n
	names (H.PRec _ pf) = concatMap fieldNames pf
	names (H.PAsPat n ns) = n : names ns
	names H.PWildCard = []
	names (H.PIrrPat n) = names n
	names (H.PatTypeSig _ n _) = names n
	names (H.PViewPat _ n) = names n
	names (H.PBangPat n) = names n
	names _ = []

	fieldNames :: H.PatField -> [H.Name]
	fieldNames (H.PFieldPat _ n) = names n
	fieldNames (H.PFieldPun n) = case n of
		H.Qual _ n' -> [n']
		H.UnQual n' -> [n']
		_ -> []
	fieldNames H.PFieldWildcard = []
getDef _ = []

-- | Make function declaration by location, name and function type
mkFun :: H.SrcLoc -> H.Name -> DeclarationInfo -> Declaration
mkFun loc n = setPosition loc . decl (identOfName n)

-- | Make function from arguments and result
--
-- @[a, b, c...] `tyFun` r == a `TyFun` b `TyFun` c ... `TyFun` r@
tyFun :: [H.Type] -> H.Type -> H.Type
tyFun as' r' = foldr H.TyFun r' as'

-- | Get name of qualified name
identOfQName :: H.QName -> (Maybe Text, Text)
identOfQName (H.Qual (H.ModuleName mname) name) = (Just $ fromString mname, identOfName name)
identOfQName (H.UnQual name) = (Nothing, identOfName name)
identOfQName (H.Special sname) = (Nothing, fromString $ H.prettyPrint sname)

-- | Get name of @H.Name@
identOfName :: H.Name -> Text
identOfName name = fromString $ case name of
	H.Ident s -> s
	H.Symbol s -> s

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

-- | Convert @H.SrcLoc@ to @Position
toPosition :: H.SrcLoc -> Position
toPosition (H.SrcLoc _ l c) = Position l c

-- | Set @Declaration@ position
setPosition :: H.SrcLoc -> Declaration -> Declaration
setPosition loc = set declarationPosition (Just $ toPosition loc)

-- | Adds documentation to declaration
addDoc :: Map String String -> Declaration -> Declaration
addDoc docsMap decl' = set declarationDocs (preview (ix (view declarationName decl')) docsMap') decl' where
	docsMap' = M.mapKeys fromString . M.map fromString $ docsMap

-- | Adds documentation to all declarations in module
addDocs :: Map String String -> Module -> Module
addDocs docsMap = over moduleDeclarations (map $ addDoc docsMap)

-- | Extract files docs and set them to declarations
inspectDocsChunk :: [String] -> [Module] -> ExceptT String IO [Module]
inspectDocsChunk opts ms = do
	docsMaps <- liftE $ hdocsy (map (view moduleLocation) ms) opts
	return $ zipWith addDocs docsMaps ms

-- | Extract file docs and set them to module declarations
inspectDocs :: [String] -> Module -> ExceptT String IO Module
inspectDocs opts m = do
	let
		hdocsWorkaround = False
	docsMap <- liftE $ if hdocsWorkaround
		then hdocsProcess (fromMaybe (T.unpack $ view moduleName m) (preview (moduleLocation . moduleFile) m)) opts
		else liftM Just $ hdocs (view moduleLocation m) opts
	return $ maybe id addDocs docsMap m

-- | Like @inspectDocs@, but in @Ghc@ monad
inspectDocsGhc :: [String] -> Module -> ExceptT String Ghc Module
inspectDocsGhc opts m = case view moduleLocation m of
	FileModule fpath _ -> do
		docsMap <- liftM (fmap (formatDocs . snd) . listToMaybe) $ readSourcesGhc opts [fpath]
		return $ maybe id addDocs docsMap m
	_ -> throwError "Can inspect only source file docs"

-- | Inspect contents
inspectContents :: String -> [String] -> String -> ExceptT String IO InspectedModule
inspectContents name opts cts = inspect (ModuleSource $ Just name) (contentsInspection cts opts) $ do
	analyzed <- ExceptT $ return $ analyzeModule exts (Just name) cts <|> analyzeModule_ exts (Just name) cts
	return $ set moduleLocation (ModuleSource $ Just name) analyzed
	where
		exts = mapMaybe flagExtension opts

contentsInspection :: String -> [String] -> ExceptT String IO Inspection
contentsInspection _ _ = return InspectionNone -- crc or smth

-- | Inspect file
inspectFile :: [String] -> FilePath -> Maybe String -> ExceptT String IO InspectedModule
inspectFile opts file mcts = do
	proj <- liftE $ locateProject file
	absFilename <- liftE $ Dir.canonicalizePath file
	ex <- liftE $ Dir.doesFileExist absFilename
	unless ex $ throwError $ "File '" ++ absFilename ++ "' doesn't exist"
	inspect (FileModule absFilename proj) ((if isJust mcts then fileContentsInspection else fileInspection) absFilename opts) $ do
		-- docsMap <- liftE $ if hdocsWorkaround
		-- 	then hdocsProcess absFilename opts
		-- 	else liftM Just $ hdocs (FileModule absFilename Nothing) opts
		forced <- ExceptT $ E.handle onError $ do
			analyzed <- liftM (\s -> analyzeModule exts (Just absFilename) s <|> analyzeModule_ exts (Just absFilename) s) $
				maybe (readFileUtf8 absFilename) return mcts
			force analyzed `deepseq` return analyzed
		-- return $ setLoc absFilename proj . maybe id addDocs docsMap $ forced
		return $ set moduleLocation (FileModule absFilename proj) forced
	where
		onError :: E.ErrorCall -> IO (Either String Module)
		onError = return . Left . show

		exts = mapMaybe flagExtension opts

-- | File inspection data
fileInspection :: FilePath -> [String] -> ExceptT String IO Inspection
fileInspection f opts = do
	tm <- liftE $ Dir.getModificationTime f
	return $ InspectionAt (utcTimeToPOSIXSeconds tm) $ sort $ ordNub opts

-- | File contents inspection data
fileContentsInspection :: FilePath -> [String] -> ExceptT String IO Inspection
fileContentsInspection _ opts = do
	tm <- liftE getPOSIXTime
	return $ InspectionAt tm $ sort $ ordNub opts

-- | Enumerate project dirs
projectDirs :: Project -> ExceptT String IO [Extensions FilePath]
projectDirs p = do
	p' <- loadProject p
	return $ ordNub $ map (fmap (normalise . (view projectPath p' </>))) $ maybe [] sourceDirs $ view projectDescription p'

-- | Enumerate project source files
projectSources :: Project -> ExceptT String IO [Extensions FilePath]
projectSources p = do
	dirs <- projectDirs p
	let
		enumCabals = liftM (map takeDirectory . filter cabalFile) . traverseDirectory
		dirs' = map (view entity) dirs
	-- enum inner projects and dont consider them as part of this project
	subProjs <- liftM (delete (view projectPath p) . ordNub . concat) $ triesMap (liftE . enumCabals) dirs'
	let
		enumHs = liftM (filter thisProjectSource) . traverseDirectory
		thisProjectSource h = haskellSource h && not (any (`isParent` h) subProjs)
	liftM (ordNub . concat) $ triesMap (liftM sequenceA . traverse (liftE . enumHs)) dirs

-- | Inspect project
inspectProject :: [String] -> Project -> ExceptT String IO (Project, [InspectedModule])
inspectProject opts p = do
	p' <- loadProject p
	srcs <- projectSources p'
	modules <- mapM inspectFile' srcs
	return (p', catMaybes modules)
	where
		inspectFile' exts = liftM return (inspectFile (opts ++ extensionsOpts exts) (view entity exts) Nothing) <|> return Nothing
