{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Symbols (
	-- * Information
	export,
	passImportList,
	importName, import_,
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
	Canonicalize(..),
	locateProject, searchProject,
	locateSourceDir,
	sourceModuleRoot,
	importedModulePath,

	-- * Modifiers
	addDeclaration,

	-- * Other
	unalias,

	-- * Reexportss
	module HsDev.Symbols.Types,
	module HsDev.Symbols.Class,
	module HsDev.Symbols.Documented
	) where

import Control.Applicative
import Control.Arrow
import Control.Lens (view, set, over)
import Control.Monad.Trans.Maybe
import Control.Monad.Except
import Data.Function (on)
import Data.List
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T (concat, split, unpack)
import System.Directory
import System.FilePath

import HsDev.Symbols.Types
import HsDev.Symbols.Class
import HsDev.Symbols.Documented (Documented(..))
import HsDev.Util (searchPath)

-- | Get name of export
export :: Export -> Text
export (ExportName Nothing n _) = n
export (ExportName (Just q) n _) = T.concat [q, ".", n]
export (ExportModule m) = m

-- | Check whether name pass import list
passImportList :: ImportList -> Text -> Bool
passImportList (ImportList hiding names) n
	| hiding = n `notElem` names
	| otherwise = n `elem` names

-- | Get import module name
importName :: Import -> Text
importName i = fromMaybe (view importModuleName i) $ view importAs i

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
scopes d = globalScope $ map (Just . importName) is where
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

-- | Canonicalize all paths within something
class Canonicalize a where
	canonicalize :: a -> IO a

instance Canonicalize FilePath where
	canonicalize = canonicalizePath

instance Canonicalize Cabal where
	canonicalize Cabal = return Cabal
	canonicalize (Sandbox p) = fmap Sandbox $ canonicalizePath p

instance Canonicalize Project where
	canonicalize (Project nm p c desc) = liftM3 (Project nm) (canonicalizePath p) (canonicalizePath c) (return desc)

instance Canonicalize ModuleLocation where
	canonicalize (FileModule f p) = liftM2 FileModule (canonicalizePath f) (traverse canonicalize p)
	canonicalize (CabalModule c p n) = fmap (\c' -> CabalModule c' p n) $ canonicalize c
	canonicalize (ModuleSource m) = return $ ModuleSource m

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

-- | Get source module root directory, i.e. for "...\src\Foo\Bar.hs" with module 'Foo.Bar' will return "...\src"
sourceModuleRoot :: Text -> FilePath -> FilePath
sourceModuleRoot mname = 
	joinPath .
	reverse . drop (length $ T.split (== '.') mname) . reverse .
	splitDirectories

-- | Get path of imported module
-- >importedModulePath "Foo.Bar" "...\src\Foo\Bar.hs" "Quux.Blah" = "...\src\Quux\Blah.hs"
importedModulePath :: Text -> FilePath -> Text -> FilePath
importedModulePath mname file imp =
	(`addExtension` "hs") . joinPath .
	(++ ipath) . splitDirectories $
	sourceModuleRoot mname file
	where
		ipath = map T.unpack $ T.split (== '.') imp

-- | Add declaration to module
addDeclaration :: Declaration -> Module -> Module
addDeclaration decl' = over moduleDeclarations (sortDeclarations . (decl' :))

-- | Unalias import name
unalias :: Module -> Text -> [Text]
unalias m alias = [view importModuleName i | i <- view moduleImports m, view importAs i == Just alias]
