{-# LANGUAGE TypeSynonymInstances, ViewPatterns #-}

module HsDev.Inspect (
	analyzeModule, inspectDocs,
	inspectContents, contentsInspection,
	inspectFile, fileInspection,
	projectDirs, projectSources,
	inspectProject
	) where

import Control.Arrow
import Control.Applicative
import Control.DeepSeq
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Error
import Data.Function (on)
import Data.List
import Data.Map (Map)
import Data.Maybe (fromMaybe, mapMaybe, catMaybes)
import Data.Ord (comparing)
import Data.String (IsString, fromString)
import qualified Data.Text as T (unpack)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Traversable (traverse, sequenceA)
import qualified Data.Map as M
import qualified Language.Haskell.Exts as H
import qualified System.Directory as Dir
import System.FilePath
import Data.Generics.Uniplate.Data

import HsDev.Symbols
import HsDev.Project
import HsDev.Tools.Base
import HsDev.Tools.HDocs (hdocs, hdocsProcess)
import HsDev.Util

-- | Analize source contents
analyzeModule :: [String] -> Maybe FilePath -> String -> Either String Module
analyzeModule exts file source = case H.parseFileContentsWithMode pmode source' of
		H.ParseFailed loc reason -> Left $ "Parse failed at " ++ show loc ++ ": " ++ reason
		H.ParseOk (H.Module _ (H.ModuleName mname) _ _ mexports imports declarations) -> Right Module {
			moduleName = fromString mname,
			moduleDocs =  Nothing,
			moduleLocation = ModuleSource Nothing,
			moduleExports = fmap (concatMap getExports) mexports,
			moduleImports = map getImport imports,
			moduleDeclarations = sortDeclarations $ getDecls declarations }
	where
		pmode :: H.ParseMode
		pmode = H.defaultParseMode {
			H.parseFilename = fromMaybe (H.parseFilename H.defaultParseMode) file,
			H.baseLanguage = H.Haskell2010,
			H.extensions = H.glasgowExts ++ map H.parseExtension exts,
			H.fixities = Just H.baseFixities }

		-- Replace all tabs to spaces to make SrcLoc valid, otherwise it treats tab as 8 spaces
		source' = map untab source
		untab '\t' = ' '
		untab ch = ch

-- | Get exports
getExports :: H.ExportSpec -> [Export]
getExports (H.EModuleContents (H.ModuleName m)) = [ExportModule $ fromString m]
getExports e = map (uncurry ExportName . (fmap fromString *** fromString) . identOfQName) $ childrenBi e

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
		importLst (hiding, specs) = ImportList hiding $ map (fromString . identOfName) (concatMap childrenBi specs :: [H.Name])

-- | Decl declarations
getDecls :: [H.Decl] -> [Declaration]
getDecls decls =
	map mergeDecls .
	groupBy ((==) `on` declarationName) .
	sortBy (comparing declarationName) $
	concatMap getDecl decls ++ concatMap getDef decls
	where
		mergeDecls :: [Declaration] -> Declaration
		mergeDecls [] = error "Impossible"
		mergeDecls ds = Declaration
			(declarationName $ head ds)
			Nothing
			Nothing
			(msum $ map declarationDocs ds)
			(minimum <$> mapM declarationPosition ds)
			(foldr1 mergeInfos $ map declaration ds)

		mergeInfos :: DeclarationInfo -> DeclarationInfo -> DeclarationInfo
		mergeInfos (Function ln ld) (Function rn rd) = Function (ln `mplus` rn) (ld ++ rd)
		mergeInfos l _ = l

-- | Get definitions
getBinds :: H.Binds -> [Declaration]
getBinds (H.BDecls decls) = getDecls decls
getBinds _ = []

-- | Get declaration and child declarations
getDecl :: H.Decl -> [Declaration]
getDecl decl' = case decl' of
	H.TypeSig loc names typeSignature -> [mkFun loc n (Function (Just $ oneLinePrint typeSignature) []) | n <- names]
	H.TypeDecl loc n args _ -> [mkType loc n Type args]
	H.DataDecl loc dataOrNew ctx n args cons _ -> mkType loc n (ctor dataOrNew `withCtx` ctx) args : concatMap (getConDecl n args) cons
	H.GDataDecl loc dataOrNew ctx n args _ gcons _ -> mkType loc n (ctor dataOrNew `withCtx` ctx) args : concatMap getGConDecl gcons
	H.ClassDecl loc ctx n args _ _ -> [mkType loc n (Class `withCtx` ctx) args]
	_ -> []
	where
		mkType :: H.SrcLoc -> H.Name -> (TypeInfo -> DeclarationInfo) -> [H.TyVarBind] -> Declaration
		mkType loc n ctor' args = setPosition loc $ decl (fromString $ identOfName n) $ ctor' $ TypeInfo Nothing (map oneLinePrint args) Nothing

		withCtx :: (TypeInfo -> DeclarationInfo) -> H.Context -> TypeInfo -> DeclarationInfo
		withCtx ctor' ctx tinfo = ctor' (tinfo { typeInfoContext = makeCtx ctx })

		ctor :: H.DataOrNew -> TypeInfo -> DeclarationInfo
		ctor H.DataType = Data
		ctor H.NewType = NewType

		makeCtx [] = Nothing
		makeCtx ctx = Just $ fromString $ intercalate ", " $ map oneLinePrint ctx

-- | Get constructor and record fields declarations
getConDecl :: H.Name -> [H.TyVarBind] -> H.QualConDecl -> [Declaration]
getConDecl t as (H.QualConDecl loc _ _ cdecl) = case cdecl of
	H.ConDecl n cts -> [mkFun loc n (Function (Just $ oneLinePrint $ cts `tyFun` dataRes) [])]
	H.InfixConDecl ct n cts -> [mkFun loc n (Function (Just $ oneLinePrint $ (ct : [cts]) `tyFun` dataRes) [])]
	H.RecDecl n fields -> mkFun loc n (Function (Just $ oneLinePrint $ map snd fields `tyFun` dataRes) []) : concatMap (uncurry (getRec loc dataRes)) fields
	where
		dataRes :: H.Type
		dataRes = foldr H.TyApp (H.TyCon (H.UnQual t)) $ map (H.TyVar . nameOf) as where
			nameOf :: H.TyVarBind -> H.Name
			nameOf (H.KindedVar n' _) = n'
			nameOf (H.UnkindedVar n') = n'

-- | Get GADT constructor and record fields declarations
getGConDecl :: H.GadtDecl -> [Declaration]
getGConDecl (H.GadtDecl loc n fields r) = mkFun loc n (Function (Just $ oneLinePrint $ map snd fields `tyFun` r) []) : concatMap (uncurry (getRec loc r)) fields where

-- | Get record field declaration
getRec :: H.SrcLoc -> H.Type -> [H.Name] -> H.Type -> [Declaration]
getRec loc t ns rt = [mkFun loc n (Function (Just $ oneLinePrint $ t `H.TyFun` rt) []) | n <- ns]

-- | Get definitions
getDef :: H.Decl -> [Declaration]
getDef (H.FunBind []) = []
getDef (H.FunBind matches@(H.Match loc n _ _ _ _ : _)) = [setPosition loc $ decl (fromString $ identOfName n) fun] where
	fun = Function Nothing $ concatMap (getBinds . matchBinds) matches
	matchBinds (H.Match _ _ _ _ _ binds) = binds
getDef (H.PatBind loc pat _ binds) = map (\name -> setPosition loc (decl (fromString $ identOfName name) (Function Nothing $ getBinds binds))) (names pat) where
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
mkFun loc n = setPosition loc . decl (fromString $ identOfName n)

-- | Make function from arguments and result
--
-- @[a, b, c...] `tyFun` r == a `TyFun` b `TyFun` c ... `TyFun` r@
tyFun :: [H.Type] -> H.Type -> H.Type
tyFun as' r' = foldr H.TyFun r' as'

-- | Get name of qualified name
identOfQName :: H.QName -> (Maybe String, String)
identOfQName (H.Qual (H.ModuleName mname) name) = (Just mname, identOfName name)
identOfQName (H.UnQual name) = (Nothing, identOfName name)
identOfQName (H.Special sname) = (Nothing, H.prettyPrint sname)

-- | Get name of @H.Name@
identOfName :: H.Name -> String
identOfName name = case name of
	H.Ident s -> s
	H.Symbol s -> s

-- | Print something in one line
oneLinePrint :: (H.Pretty a, IsString s) => a -> s
oneLinePrint = fromString . H.prettyPrintStyleMode (H.style { H.mode = H.OneLineMode }) H.defaultMode

-- | Convert @H.SrcLoc@ to @Position
toPosition :: H.SrcLoc -> Position
toPosition (H.SrcLoc _ l c) = Position l c

-- | Set @Declaration@ position
setPosition :: H.SrcLoc -> Declaration -> Declaration
setPosition loc d = d { declarationPosition = Just (toPosition loc) }

-- | Adds documentation to declaration
addDoc :: Map String String -> Declaration -> Declaration
addDoc docsMap decl' = decl' { declarationDocs = M.lookup (declarationName decl') docsMap' } where
	docsMap' = M.mapKeys fromString . M.map fromString $ docsMap

-- | Adds documentation to all declarations in module
addDocs :: Map String String -> Module -> Module
addDocs docsMap m = m { moduleDeclarations = map (addDoc docsMap) (moduleDeclarations m) }

-- | Extract file docs and set them to module declarations
inspectDocs :: [String] -> Module -> ErrorT String IO Module
inspectDocs opts m = do
	let
		hdocsWorkaround = True
	docsMap <- liftE $ if hdocsWorkaround
		then hdocsProcess (fromMaybe (T.unpack $ moduleName m) $ moduleSource $ moduleLocation m) opts
		else liftM Just $ hdocs (moduleLocation m) opts
	return $ maybe id addDocs docsMap $ m

-- | Inspect contents
inspectContents :: String -> [String] -> String -> ErrorT String IO InspectedModule
inspectContents name opts cts = inspect (ModuleSource $ Just name) (contentsInspection cts opts) $ do
	analyzed <- ErrorT $ return $ analyzeModule exts (Just name) cts
	return $ setLoc analyzed
	where
		setLoc m = m { moduleLocation = ModuleSource (Just name) }

		exts = mapMaybe flagExtension opts

contentsInspection :: String -> [String] -> ErrorT String IO Inspection
contentsInspection _ _ = return InspectionNone -- crc or smth

-- | Inspect file
inspectFile :: [String] -> FilePath -> ErrorT String IO InspectedModule
inspectFile opts file = do
	proj <- liftE $ locateProject file
	absFilename <- liftE $ Dir.canonicalizePath file
	inspect (FileModule absFilename proj) (fileInspection absFilename opts) $ do
		-- docsMap <- liftE $ if hdocsWorkaround
		-- 	then hdocsProcess absFilename opts
		-- 	else liftM Just $ hdocs (FileModule absFilename Nothing) opts
		forced <- ErrorT $ E.handle onError $ do
			analyzed <- liftM (analyzeModule exts (Just absFilename)) $ readFileUtf8 absFilename
			force analyzed `deepseq` return analyzed
		-- return $ setLoc absFilename proj . maybe id addDocs docsMap $ forced
		return $ setLoc absFilename proj forced
	where
		setLoc f p m = m { moduleLocation = FileModule f p }
		onError :: E.ErrorCall -> IO (Either String Module)
		onError = return . Left . show

		exts = mapMaybe flagExtension opts

-- | File inspection data
fileInspection :: FilePath -> [String] -> ErrorT String IO Inspection
fileInspection f opts = do
	tm <- liftE $ Dir.getModificationTime f
	return $ InspectionAt (utcTimeToPOSIXSeconds tm) $ sort $ nub opts

-- | Enumerate project dirs
projectDirs :: Project -> ErrorT String IO [Extensions FilePath]
projectDirs p = do
	p' <- loadProject p
	return $ nub $ map (fmap (normalise . (projectPath p' </>))) $ maybe [] sourceDirs $ projectDescription p'

-- | Enumerate project source files
projectSources :: Project -> ErrorT String IO [Extensions FilePath]
projectSources p = do
	dirs <- projectDirs p
	let
		enumCabals = liftM (map takeDirectory . filter cabalFile) . traverseDirectory
		dirs' = map entity dirs
	-- enum inner projects and dont consider them as part of this project
	subProjs <- liftM (delete (projectPath p) . nub . concat) $ triesMap (liftE . enumCabals) dirs'
	let
		enumHs = liftM (filter thisProjectSource) . traverseDirectory
		thisProjectSource h = haskellSource h && not (any (`isParent` h) subProjs)
	liftM (nub . concat) $ triesMap (liftM sequenceA . traverse (liftE . enumHs)) dirs

-- | Inspect project
inspectProject :: [String] -> Project -> ErrorT String IO (Project, [InspectedModule])
inspectProject opts p = do
	p' <- loadProject p
	srcs <- projectSources p'
	modules <- mapM inspectFile' srcs
	return (p', catMaybes modules)
	where
		inspectFile' exts = liftM return (inspectFile (opts ++ extensionsOpts (extensions exts)) (entity exts)) <|> return Nothing
