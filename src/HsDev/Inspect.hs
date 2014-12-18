{-# LANGUAGE TypeSynonymInstances #-}

module HsDev.Inspect (
	analyzeModule,
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
import Data.String (fromString)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Traversable (traverse, sequenceA)
import qualified Data.Map as M
import qualified Language.Haskell.Exts as H
import qualified Documentation.Haddock as Doc
import qualified System.Directory as Dir
import System.IO
import System.FilePath
import Data.Generics.Uniplate.Data

import qualified Name (Name, getOccString, occNameString)
import qualified Module (moduleNameString)
import qualified SrcLoc as Loc
import qualified HsDecls
import qualified HsBinds

import HsDev.Symbols
import HsDev.Project
import HsDev.Tools.Base
import HsDev.Tools.HDocs (hdocsProcess)
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
			moduleDeclarations = M.fromList $ map (declarationName &&& id) $ getDecls declarations }
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

getExports :: H.ExportSpec -> [Export]
getExports (H.EModuleContents (H.ModuleName m)) = [ExportModule $ fromString m]
getExports e = map (ExportName . fromString . identOfName) $ childrenBi e

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
			(msum $ map declarationDocs ds)
			(minimum <$> mapM declarationPosition ds)
			(foldr1 mergeInfos $ map declaration ds)

		mergeInfos :: DeclarationInfo -> DeclarationInfo -> DeclarationInfo
		mergeInfos (Function ln ld) (Function rn rd) = Function (ln `mplus` rn) (ld ++ rd)
		mergeInfos l _ = l

getBinds :: H.Binds -> [Declaration]
getBinds (H.BDecls decls) = getDecls decls
getBinds _ = []

getDecl :: H.Decl -> [Declaration]
getDecl decl' = case decl' of
	H.TypeSig loc names typeSignature -> [mkFun loc n (Function (Just $ fromString $ oneLinePrint typeSignature) []) | n <- names]
	H.TypeDecl loc n args _ -> [mkType loc n Type args]
	H.DataDecl loc dataOrNew ctx n args _ _ -> [mkType loc n (ctor dataOrNew `withCtx` ctx) args]
	H.GDataDecl loc dataOrNew ctx n args _ _ _ -> [mkType loc n (ctor dataOrNew `withCtx` ctx) args]
	H.ClassDecl loc ctx n args _ _ -> [mkType loc n (Class `withCtx` ctx) args]
	_ -> []
	where
		mkFun :: H.SrcLoc -> H.Name -> DeclarationInfo -> Declaration
		mkFun loc n = setPosition loc . decl (fromString $ identOfName n)

		mkType :: H.SrcLoc -> H.Name -> (TypeInfo -> DeclarationInfo) -> [H.TyVarBind] -> Declaration
		mkType loc n ctor' args = setPosition loc $ decl (fromString $ identOfName n) $ ctor' $ TypeInfo Nothing (map (fromString . oneLinePrint) args) Nothing

		withCtx :: (TypeInfo -> DeclarationInfo) -> H.Context -> TypeInfo -> DeclarationInfo
		withCtx ctor' ctx tinfo = ctor' (tinfo { typeInfoContext = makeCtx ctx })

		ctor :: H.DataOrNew -> TypeInfo -> DeclarationInfo
		ctor H.DataType = Data
		ctor H.NewType = NewType

		makeCtx [] = Nothing
		makeCtx ctx = Just $ fromString $ intercalate ", " $ map oneLinePrint ctx

		oneLinePrint :: H.Pretty a => a -> String
		oneLinePrint = H.prettyPrintStyleMode (H.style { H.mode = H.OneLineMode }) H.defaultMode

getDef :: H.Decl -> [Declaration]
getDef (H.FunBind []) = []
getDef (H.FunBind matches@(H.Match loc n _ _ _ _ : _)) = [setPosition loc $ Declaration (fromString $ identOfName n) Nothing Nothing fun] where
	fun = Function Nothing $ concatMap (getBinds . matchBinds) matches
	matchBinds (H.Match _ _ _ _ _ binds) = binds
getDef (H.PatBind loc pat _ binds) = map (\name -> setPosition loc (Declaration (fromString $ identOfName name) Nothing Nothing (Function Nothing $ getBinds binds))) (names pat) where
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

identOfName :: H.Name -> String
identOfName name = case name of
	H.Ident s -> s
	H.Symbol s -> s

toPosition :: H.SrcLoc -> Position
toPosition (H.SrcLoc _ l c) = Position l c

setPosition :: H.SrcLoc -> Declaration -> Declaration
setPosition loc d = d { declarationPosition = Just (toPosition loc) }

-- | Get Map from declaration name to its documentation
documentationMap :: Doc.Interface -> Map String String
documentationMap iface = M.fromList $ concatMap toDoc $ Doc.ifaceExportItems iface where
	toDoc :: Doc.ExportItem Name.Name -> [(String, String)]
	toDoc (Doc.ExportDecl decl' docs _ _ _ _) = maybe [] (zip (extractNames decl') . repeat) $ extractDocs docs
	toDoc _ = []

	extractNames :: HsDecls.LHsDecl Name.Name -> [String]
	extractNames (Loc.L _ d) = case d of
		HsDecls.TyClD ty -> [locatedName $ HsDecls.tcdLName ty]
		HsDecls.SigD sig -> case sig of
			HsBinds.TypeSig names _ -> map locatedName names
			HsBinds.GenericSig names _ -> map locatedName names
			_ -> []
		_ -> []

	extractDocs :: Doc.DocForDecl Name.Name -> Maybe String
	extractDocs (mbDoc, _) = printDoc <$> Doc.documentationDoc mbDoc where
		printDoc :: Doc.Doc Name.Name -> String
		printDoc Doc.DocEmpty = ""
		printDoc (Doc.DocAppend l r) = printDoc l ++ printDoc r
		printDoc (Doc.DocString s) = s
		printDoc (Doc.DocParagraph p) = printDoc p
		printDoc (Doc.DocIdentifier i) = Name.getOccString i
		printDoc (Doc.DocIdentifierUnchecked (m, i)) = Module.moduleNameString m ++ "." ++ Name.occNameString i
		printDoc (Doc.DocModule m) = m
		printDoc (Doc.DocWarning w) = printDoc w
		printDoc (Doc.DocEmphasis e) = printDoc e
		printDoc (Doc.DocMonospaced m) = printDoc m
		printDoc (Doc.DocUnorderedList lst) = concatMap printDoc lst -- Is this right?
		printDoc (Doc.DocOrderedList lst) = concatMap printDoc lst -- And this
		printDoc (Doc.DocDefList defs) = concatMap (\(l, r) -> printDoc l ++ " = " ++ printDoc r) defs -- ?
		printDoc (Doc.DocCodeBlock code) = printDoc code
		printDoc (Doc.DocPic pic) = show pic
		printDoc (Doc.DocAName a) = a
		printDoc (Doc.DocExamples exs) = unlines $ map showExample exs where
			showExample (Doc.Example expr results) = expr ++ " => " ++ intercalate ", " results
		printDoc (Doc.DocHyperlink link) = fromMaybe (Doc.hyperlinkUrl link) (Doc.hyperlinkLabel link)
		printDoc (Doc.DocProperty prop) = prop
		-- Catch all unsupported ones
		printDoc _ = "[unsupported-by-extractDocs]" -- TODO

	locatedName :: Loc.Located Name.Name -> String
	locatedName (Loc.L _ nm) = Name.getOccString nm

-- | Adds documentation to declaration
addDoc :: Map String String -> Declaration -> Declaration
addDoc docsMap decl' = decl' { declarationDocs = M.lookup (declarationName decl') docsMap' } where
	docsMap' = M.mapKeys fromString . M.map fromString $ docsMap

-- | Adds documentation to all declarations in module
addDocs :: Map String String -> Module -> Module
addDocs docsMap m = m { moduleDeclarations = M.map (addDoc docsMap) (moduleDeclarations m) }

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
	let
		noReturn :: E.SomeException -> IO [Doc.Interface]
		noReturn _ = return []

		hdocsWorkaround = True
	proj <- liftIO $ locateProject file
	absFilename <- liftIO $ Dir.canonicalizePath file
	inspect (FileModule absFilename proj) (fileInspection absFilename opts) $ do
		docsMap <- liftIO $ if hdocsWorkaround
			then hdocsProcess absFilename opts
			else fmap (fmap documentationMap . lookup absFilename) $ do
				is <- E.catch (Doc.createInterfaces ([Doc.Flag_Verbosity "0", Doc.Flag_NoWarnings] ++ map Doc.Flag_OptGhc opts) [absFilename]) noReturn
				forM is $ \i -> do
					mfile <- Dir.canonicalizePath $ Doc.ifaceOrigFilename i
					return (mfile, i)
		forced <- ErrorT $ E.handle onError $ do
			analyzed <- liftM (analyzeModule exts (Just absFilename)) $ readFileUtf8 absFilename
			force analyzed `deepseq` return analyzed
			--E.evaluate $ force analyzed
		return $ setLoc absFilename proj . maybe id addDocs docsMap $ forced
	where
		setLoc f p m = m { moduleLocation = FileModule f p }
		onError :: E.ErrorCall -> IO (Either String Module)
		onError = return . Left . show

		exts = mapMaybe flagExtension opts

-- | File inspection data
fileInspection :: FilePath -> [String] -> ErrorT String IO Inspection
fileInspection f opts = do
	tm <- liftIO $ Dir.getModificationTime f
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
	subProjs <- liftIO $ liftM (delete (projectPath p) . nub . concat) $ mapM (liftIO . enumCabals) dirs'
	let
		enumHs = liftM (filter thisProjectSource) . traverseDirectory
		thisProjectSource h = haskellSource h && not (any (`isParent` h) subProjs)
	liftIO $ liftM (nub . concat) $ mapM (liftM sequenceA . traverse (liftIO . enumHs)) dirs

-- | Inspect project
inspectProject :: [String] -> Project -> ErrorT String IO (Project, [InspectedModule])
inspectProject opts p = do
	p' <- loadProject p
	srcs <- projectSources p'
	modules <- mapM inspectFile' srcs
	return (p', catMaybes modules)
	where
		inspectFile' exts = liftM return (inspectFile (opts ++ extensionsOpts (extensions exts)) (entity exts)) <|> return Nothing

-- | Read file in UTF8
readFileUtf8 :: FilePath -> IO String
readFileUtf8 f = withFile f ReadMode $ \h -> do
	hSetEncoding h utf8
	cts <- hGetContents h
	length cts `seq` return cts
