{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Symbols (
	-- * Information
	export,
	passThingSpec, passImportSpec, imported, exported,
	importNames, import_,
	Symbol(..),
	unnamedModuleId,
	sortDeclarations, moduleLocals,
	setDefinedIn, dropExternals, clearDefinedIn,
	moduleLocalDeclarations, moduleModuleDeclarations,
	Locals(..),
	decl, definedIn, declarationLocals, scopes,
	mergeExported,

	-- * Functions
	importQualifier,

	-- * Utility
	locateProject, searchProject,
	locateSourceDir,
	standaloneInfo,
	moduleOpts,

	-- * Modifiers
	addDeclaration,

	-- * Other
	unalias,

	-- * Tags
	setTag, hasTag, removeTag, dropTags,

	-- * Reexportss
	module HsDev.Symbols.Types,
	module HsDev.Symbols.Class,
	module HsDev.Symbols.Documented
	) where

import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad.Trans.Maybe
import Control.Monad.Except
import Data.Function (on)
import Data.List
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import qualified Data.Map as M
import Data.Ord (comparing)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T (concat)
import System.Directory
import System.FilePath

import System.Directory.Paths

import HsDev.Symbols.Types
import HsDev.Symbols.Class
import HsDev.Symbols.Documented (Documented(..))
import HsDev.Util (searchPath, uniqueBy)

-- | Get name of export
export :: Export -> Text
export (ExportName Nothing n _) = n
export (ExportName (Just q) n _) = T.concat [q, ".", n]
export (ExportModule m) = m

-- | Does name pass thing spec
passThingSpec :: Text -> ThingPart -> Bool
passThingSpec _ ThingNothing = False
passThingSpec _ ThingAll = True
passThingSpec n (ThingWith ns) = n `elem` ns

-- | Does declaration pass import-list spec
passImportSpec :: Declaration -> ImportSpec -> Bool
passImportSpec decl' (ImportSpec n p)
	| view declarationName decl' == n = True
	| preview (declaration . related . _Just) decl' == Just n = view declarationName decl' `passThingSpec` p
	| otherwise = False

-- | Check whether declaration passes import list
imported :: Declaration -> ImportList -> Bool
imported decl' (ImportList hiding specs) = invert $ any (decl' `passImportSpec`) specs where
	invert = if hiding then not else id

-- | Check whether declaration passes export
exported :: Declaration -> Export -> Bool
exported decl' (ExportName q n p)
	| view declarationName decl' == n = checkImport
	| preview (declaration . related . _Just) decl' == Just n = view declarationName decl' `passThingSpec` p
	| otherwise = False
	where
		checkImport = case q of
			Nothing -> any (not . view importIsQualified) $ fromMaybe [] $ view declarationImported decl'
			Just q' -> any ((q' `elem`) . importNames) $ fromMaybe [] $ view declarationImported decl'
exported decl' (ExportModule m) = any (unqualBy m) . fromMaybe [] . view declarationImported $ decl' where
	unqualBy :: Text -> Import -> Bool
	unqualBy m' i = m' `elem` importNames i && not (view importIsQualified i)

-- | Get import module names - full and synonym
importNames :: Import -> [Text]
importNames i = view importModuleName i : maybe [] return (view importAs i)

-- | Simple import
import_ :: Text -> Import
import_ n = Import n False Nothing Nothing Nothing

-- | Imported module can be accessed via qualifier
importQualifier :: Maybe Text -> Import -> Bool
importQualifier Nothing i
	| not (view importIsQualified i) = True
	| otherwise = False
importQualifier (Just q) i
	| q == view importModuleName i = True
	| Just q == view importAs i = True
	| otherwise = False

unnamedModuleId :: ModuleLocation -> ModuleId
unnamedModuleId = ModuleId ""

sortDeclarations :: [Declaration] -> [Declaration]
sortDeclarations = sortBy (comparing (view declarationName))

-- | Bring locals to top
moduleLocals :: Module -> Module
moduleLocals m = set moduleDeclarations (moduleLocalDeclarations m) m

-- | Set all declaration `definedIn` to this module
setDefinedIn :: Module -> Module
setDefinedIn m = over moduleDeclarations (map (`definedIn` view moduleId m)) m

-- | Drop all declarations, that not defined in this module
dropExternals :: Module -> Module
dropExternals m = over moduleDeclarations (filter ((/= Just (view moduleId m)) . view declarationDefined)) m

-- | Clear `definedIn` information
clearDefinedIn :: Module -> Module
clearDefinedIn = over moduleDeclarations (map (set declarationDefined Nothing))

-- | Get declarations with locals
moduleLocalDeclarations :: Module -> [Declaration]
moduleLocalDeclarations =
	sortDeclarations .
	concatMap declarationLocals' .
	view moduleDeclarations
	where
		declarationLocals' :: Declaration -> [Declaration]
		declarationLocals' d = d : declarationLocals d

-- | Get list of declarations as ModuleDeclaration
moduleModuleDeclarations :: Module -> [ModuleDeclaration]
moduleModuleDeclarations m = [ModuleDeclaration (view moduleId m) d | d <- view moduleDeclarations m]

class Locals a where
	locals :: a -> [Declaration]
	where_ :: a -> [Declaration] -> a

instance Locals Declaration where
	locals = locals . view declaration
	where_ d ds = over declaration (`where_` ds) d

decl :: Text -> DeclarationInfo -> Declaration
decl n = Declaration n Nothing Nothing Nothing Nothing

definedIn :: Declaration -> ModuleId -> Declaration
definedIn d m = set declarationDefined (Just m) d

declarationLocals :: Declaration -> [Declaration]
declarationLocals d = locals $ view declaration d

-- | Get scopes of @Declaration@, where @Nothing@ is global scope
scopes :: Declaration -> [Maybe Text]
scopes d = globalScope $ concatMap (map Just . importNames) is where
	is = fromMaybe [] $ view declarationImported d
	globalScope
		| any (not . view importIsQualified) is = (Nothing :)
		| otherwise = id

instance Locals DeclarationInfo where
	locals (Function _ ds _) = ds
	locals _ = []
	where_ (Function n s r) ds = Function n (s ++ ds) r
	where_ d _ = d

-- | Merge @ModuleDeclaration@ into @ExportedDeclaration@
mergeExported :: [ModuleDeclaration] -> [ExportedDeclaration]
mergeExported =
	map merge' .
	groupBy ((==) `on` declId) .
	sortBy (comparing declId)
	where
		declId :: ModuleDeclaration -> (Text, Maybe ModuleId)
		declId = view moduleDeclaration >>> (view declarationName &&& view declarationDefined)
		merge' :: [ModuleDeclaration] -> ExportedDeclaration
		merge' [] = error "mergeExported: impossible"
		merge' ds@(d:_) = ExportedDeclaration (map (view declarationModuleId) ds) (view moduleDeclaration d)

instance Paths Project where
	paths f (Project nm p c desc) = Project nm <$> f p <*> f c <*> pure desc

instance Paths ModuleLocation where
	paths f (FileModule fpath p) = FileModule <$> f fpath <*> traverse (paths f) p
	paths f (InstalledModule c p n) = InstalledModule <$> paths f c <*> pure p <*> pure n
	paths _ (ModuleSource m) = pure $ ModuleSource m

-- | Find project file is related to
locateProject :: FilePath -> IO (Maybe Project)
locateProject file = do
	file' <- canonicalizePath file
	isDir <- doesDirectoryExist file'
	if isDir then locateHere file' else locateParent (takeDirectory file')
	where
		locateHere path = do
			cts <- filter (not . null . takeBaseName) <$> getDirectoryContents path
			return $ fmap (project . (path </>)) $ find ((== ".cabal") . takeExtension) cts
		locateParent dir = do
			cts <- filter (not . null . takeBaseName) <$> getDirectoryContents dir
			case find ((== ".cabal") . takeExtension) cts of
				Nothing -> if isDrive dir then return Nothing else locateParent (takeDirectory dir)
				Just cabalf -> return $ Just $ project (dir </> cabalf)

-- | Search project up
searchProject :: FilePath -> IO (Maybe Project)
searchProject file = runMaybeT $ searchPath file (MaybeT . locateProject) <|> mzero

-- | Locate source dir of file
locateSourceDir :: FilePath -> IO (Maybe (Extensions FilePath))
locateSourceDir f = runMaybeT $ do
	file <- liftIO $ canonicalizePath f
	p <- MaybeT $ locateProject file
	proj <- MaybeT $ fmap (either (const Nothing) Just) $ runExceptT $ loadProject p
	MaybeT $ return $ findSourceDir proj file

-- | Make `Info` for standalone `Module`
standaloneInfo :: [PackageConfig] -> Module -> Info
standaloneInfo pkgs m = mempty { _infoDepends = pkgDeps ^.. each . package . packageName } where
	pkgDeps = catMaybes [M.lookup mdep pkgMap >>= listToMaybe | mdep <- m ^.. moduleImports . each . importModuleName]
	pkgMap = M.unionsWith mergePkgs [M.singleton m' [p] | p <- pkgs, m' <- view packageModules p]
	mergePkgs ls rs = if null es then hs else es where
		(es, hs) = partition (view packageExposed) $ uniqueBy (view package) (ls ++ rs)

-- | Options for GHC of module and project
moduleOpts :: [PackageConfig] -> Module -> [String]
moduleOpts pkgs m = case view moduleLocation m of
	FileModule file proj -> concat [
		hidePackages,
		targetOpts info']
		where
			infos' = maybe [standaloneInfo pkgs m] (`fileTargets` file) proj
			info' = over infoDepends (filter validDep) (mconcat $ selfInfo : infos')
			selfInfo
				| proj ^? _Just . projectName `elem` map Just (infos' ^.. each . infoDepends . each) = fromMaybe mempty $
					proj ^? _Just . projectDescription . _Just . projectLibrary . _Just . libraryBuildInfo
				| otherwise = mempty
			-- filter out unavailable packages such as unix under windows
			validDep d = d `elem` pkgs'
			pkgs' = pkgs ^.. each . package . packageName
			hidePackages
				| null (info' ^. infoDepends) = []
				| otherwise = ["-hide-all-packages"]
	_ -> []

-- | Add declaration to module
addDeclaration :: Declaration -> Module -> Module
addDeclaration decl' = over moduleDeclarations (sortDeclarations . (decl' :))

-- | Unalias import name
unalias :: Module -> Text -> [Text]
unalias m alias = [view importModuleName i | i <- view moduleImports m, view importAs i == Just alias]

-- | Set tag to `Inspected`
setTag :: Ord t => t -> Inspected i t a -> Inspected i t a
setTag tag = over inspectionTags (S.insert tag)

-- | Check whether `Inspected` has tag
hasTag :: Ord t => t -> Inspected i t a -> Bool
hasTag tag = has (inspectionTags . ix tag)

-- | Drop tag from `Inspected`
removeTag :: Ord t => t -> Inspected i t a -> Inspected i t a
removeTag tag = over inspectionTags (S.delete tag)

-- | Drop all tags
dropTags :: Inspected i t a -> Inspected i t a
dropTags = set inspectionTags S.empty
