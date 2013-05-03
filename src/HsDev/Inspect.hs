module HsDev.Inspect (
	analyzeModule,
	inspectFile,
	inspectProject
	) where

import Control.Arrow
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Error
import Data.List (intercalate)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Language.Haskell.Exts as H
import qualified Documentation.Haddock as Doc
import qualified System.Directory as Dir
import System.IO
import System.FilePath

import qualified Name (Name, getOccString, occNameString)
import qualified Module (moduleNameString)
import qualified SrcLoc as Loc
import qualified HsDecls
import qualified HsBinds

import HsDev.Symbols
import HsDev.Project
import HsDev.Util

-- | Analize source contents
analyzeModule :: String -> Either String (Symbol Module)
analyzeModule source = fmap setModuleReferences $ case H.parseFileContents source of
	H.ParseFailed loc reason -> Left $ "Parse failed at " ++ show loc ++ ": " ++ reason
	H.ParseOk (H.Module _ (H.ModuleName moduleName) _ _ _ imports declarations) -> Right $ mkSymbol moduleName $ Module {
		moduleExports = [],
		moduleImports = M.fromList $ map ((importModuleName &&& id) . getImport) imports,
		moduleDeclarations = M.fromList $ map (symbolName &&& id) $ getDecls declarations,
		moduleCabal = Nothing }

getImport :: H.ImportDecl -> Import
getImport d = Import (mname (H.importModule d)) (H.importQualified d) (fmap mname $ H.importAs d) where
	mname (H.ModuleName n) = n

getDecls :: [H.Decl] -> [Symbol Declaration]
getDecls decls = infos ++ defs where
	infos = concatMap getDecl decls
	names = map symbolName infos
	defs = filter noInfo $ concatMap getDef decls
	noInfo :: Symbol Declaration -> Bool
	noInfo d = symbolName d `notElem` names

getDecl :: H.Decl -> [Symbol Declaration]
getDecl decl = case decl of
	H.TypeSig loc names typeSignature -> map
		(\n -> setLocation loc (mkSymbol (identOfName n) (Function (Just $ H.prettyPrint typeSignature))))
		names
	H.TypeDecl loc n args _ -> [setLocation loc $ mkSymbol (identOfName n) (Type $ TypeInfo Nothing (map H.prettyPrint args) Nothing)]
	H.DataDecl loc dataOrNew ctx n args _ _ -> [setLocation loc $ mkSymbol (identOfName n) (ctor dataOrNew $ TypeInfo (makeCtx ctx) (map H.prettyPrint args) Nothing)]
	H.GDataDecl loc dataOrNew ctx n args _ _ _ -> [setLocation loc $ mkSymbol (identOfName n) (ctor dataOrNew $ TypeInfo (makeCtx ctx) (map H.prettyPrint args) Nothing)]
	H.ClassDecl loc ctx n args _ _ -> [setLocation loc $ mkSymbol (identOfName n) (Class $ TypeInfo (makeCtx ctx) (map H.prettyPrint args) Nothing)]
	_ -> []
	where
		ctor :: H.DataOrNew -> TypeInfo -> Declaration
		ctor H.DataType = Data
		ctor H.NewType = NewType

		makeCtx [] = Nothing
		makeCtx ctx = Just $ intercalate ", " $ map H.prettyPrint ctx

getDef :: H.Decl -> [Symbol Declaration]
getDef (H.FunBind []) = []
getDef (H.FunBind (H.Match loc n _ _ _ _ : _)) = [setLocation loc $ mkSymbol (identOfName n) (Function Nothing)]
getDef (H.PatBind loc pat _ _ _) = map (\name -> setLocation loc (mkSymbol (identOfName name) (Function Nothing))) $ names pat where
	names :: H.Pat -> [H.Name]
	names (H.PVar n) = [n]
	names (H.PNeg n) = names n
	names (H.PNPlusK n _) = [n]
	names (H.PInfixApp l _ r) = names l ++ names r
	names (H.PApp _ ns) = concatMap names ns
	names (H.PTuple ns) = concatMap names ns
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
	fieldNames (H.PFieldPun n) = [n]
	fieldNames H.PFieldWildcard = []
getDef _ = []

identOfName :: H.Name -> String
identOfName name = case name of
	H.Ident s -> s
	H.Symbol s -> s

toLocation :: H.SrcLoc -> Location
toLocation (H.SrcLoc fname l c) = Location fname l c Nothing

setLocation :: H.SrcLoc -> Symbol a -> Symbol a
setLocation loc s = s { symbolLocation = Just (toLocation loc) }

-- | Get Map from declaration name to its documentation
documentationMap :: Doc.Interface -> Map String String
documentationMap iface = M.fromList $ concatMap toDoc $ Doc.ifaceExportItems iface where
	toDoc :: Doc.ExportItem Name.Name -> [(String, String)]
	toDoc (Doc.ExportDecl decl docs _ _) = maybe [] (zip (extractNames decl) . repeat) $ extractDocs docs
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
	extractDocs (mbDoc, _) = fmap printDoc $ Doc.documentationDoc mbDoc where
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
		printDoc (Doc.DocPic pic) = pic
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
addDoc :: Map String String -> Symbol Declaration -> Symbol Declaration
addDoc docsMap decl = decl { symbolDocs = M.lookup (symbolName decl) docsMap }

-- | Adds documentations to module
addDocs :: Map String String -> Symbol Module -> Symbol Module
addDocs docsMap info = fmap addDocs' info where
	addDocs' :: Module -> Module
	addDocs' m = m { moduleDeclarations = M.map (addDoc docsMap) (moduleDeclarations m) }

-- | Inspect file
inspectFile :: FilePath -> ErrorT String IO (Symbol Module)
inspectFile file = do
	let
		noReturn :: E.SomeException -> IO [Doc.Interface]
		noReturn e = return []
	p <- liftIO $ locateProject file
	source <- liftIO $ readFileUtf8 file
	absFilename <- liftIO $ Dir.canonicalizePath file
	docsMap <- liftIO $ fmap (fmap documentationMap . lookup absFilename) $ do
		is <- E.catch (Doc.createInterfaces [] [file]) noReturn
		forM is $ \i -> do
			moduleFile <- Dir.canonicalizePath $ Doc.ifaceOrigFilename i
			return (moduleFile, i)
	either throwError (return . setModuleReferences . fmap (setLocations absFilename p) . setLoc absFilename p . maybe id addDocs docsMap) $ analyzeModule source
	where
		setLoc f p s = s { symbolLocation = Just ((moduleLocation f) { locationProject = p }) }
		setLocations f p m = m { moduleDeclarations = M.map (\decl -> decl { symbolLocation = fmap (\l -> l { locationFile = f, locationProject = p }) (symbolLocation decl) }) (moduleDeclarations m) }

-- | Inspect project
inspectProject :: Project -> ErrorT String IO (Project, [Symbol Module])
inspectProject p = do
	p' <- readProject (projectCabal p)
	let
		dirs = maybe [] (map (projectPath p' </>) . sourceDirs) $ projectDescription p'
	sourceFiles <- liftIO $ liftM (filter ((== ".hs") . takeExtension) . concat) $ mapM traverseDirectory dirs
	modules <- liftM concat $ mapM inspectFile' sourceFiles
	return (p', modules)
	where
		inspectFile' f = catchError (liftM return $ inspectFile f) (const $ return [])

-- | Read file in UTF8
readFileUtf8 :: FilePath -> IO String
readFileUtf8 f = do
	h <- openFile f ReadMode
	hSetEncoding h utf8
	hGetContents h
