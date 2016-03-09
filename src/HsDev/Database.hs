{-# LANGUAGE OverloadedStrings #-}

module HsDev.Database (
	Database(..),
	databaseIntersection, nullDatabase, databaseLocals, databaseCabals, allModules, allDeclarations, allPackages,
	fromModule, fromProject,
	filterDB,
	projectDB, cabalDB, sandboxStackDB, standaloneDB,
	selectModules, selectDeclarations, lookupModule, lookupInspected, lookupFile, refineProject,
	getInspected,

	append, remove,

	Structured(..),
	structured, structurize, merge,

	Map
	) where

import Control.Lens (set, view, preview, _Just, _Right, each, (^..))
import Control.Monad (msum, join)
import Control.DeepSeq (NFData(..))
import Data.Aeson
import Data.Either (rights)
import Data.Foldable (find)
import Data.Function (on)
import Data.Group (Group(..))
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M

import HsDev.Symbols
import HsDev.Symbols.Util
import HsDev.Util ((.::), ordNub, mapBy)

-- | HsDev database
data Database = Database {
	databaseModules :: [InspectedModule],
	databaseProjects :: [Project] }
		deriving (Eq, Ord)

instance NFData Database where
	rnf (Database ms ps) = rnf ms `seq` rnf ps

instance Group Database where
	add old new = Database {
		databaseModules = M.elems $ mapBy (view inspectedId) (databaseModules new) `M.union` mapBy (view inspectedId) (databaseModules old),
		databaseProjects = M.elems $ M.unionWith mergeProject
			(mapBy (view projectCabal) (databaseProjects new))
			(mapBy (view projectCabal) (databaseProjects old)) }
		where
			mergeProject pl pr = set projectDescription (msum [view projectDescription pl, view projectDescription pr]) pl
	sub old new = Database {
		databaseModules = M.elems $ mapBy (view inspectedId) (databaseModules old) `M.difference` mapBy (view inspectedId) (databaseModules new),
		databaseProjects = M.elems $ mapBy (view projectCabal) (databaseProjects old) `M.difference` mapBy (view projectCabal) (databaseProjects new) }
	zero = Database [] []

instance Monoid Database where
	mempty = zero
	mappend = add

instance ToJSON Database where
	toJSON (Database ms ps) = object [
		"modules" .= ms,
		"projects" .= ps]

instance FromJSON Database where
	parseJSON = withObject "database" $ \v -> Database <$>
		(v .:: "modules") <*>
		(v .:: "projects")

-- | Database intersection, prefers first database data
databaseIntersection :: Database -> Database -> Database
databaseIntersection l r = mempty {
	databaseModules = M.elems $ mapBy (view inspectedId) (databaseModules l) `M.intersection` mapBy (view inspectedId) (databaseModules r),
	databaseProjects = M.elems $ mapBy (view projectCabal) (databaseProjects l) `M.intersection` mapBy (view projectCabal) (databaseProjects r) }

-- | Check if database is empty
nullDatabase :: Database -> Bool
nullDatabase db = null (databaseModules db) && null (databaseProjects db)

-- | Bring all locals to scope
databaseLocals :: Database -> Database
databaseLocals db = db {
	databaseModules = fmap (fmap moduleLocals) (databaseModules db) }

-- | All scanned sandboxes
databaseCabals :: Database -> [Cabal]
databaseCabals db = ordNub $ databaseModules db ^.. each . inspectionResult . _Right . moduleLocation . moduleCabal

-- | All modules
allModules :: Database -> [Module]
allModules = rights . fmap (view inspectionResult) . databaseModules

-- | All declarations
allDeclarations :: Database -> [ModuleDeclaration]
allDeclarations db = do
	m <- allModules db
	moduleModuleDeclarations m

-- | All packages
allPackages :: Database -> [ModulePackage]
allPackages = ordNub . mapMaybe (preview (moduleLocation . modulePackage . _Just)) . allModules

-- | Make database from module
fromModule :: InspectedModule -> Database
fromModule m = zero {
	databaseModules = [m] }

-- | Make database from project
fromProject :: Project -> Database
fromProject p = zero {
	databaseProjects = [p] }

-- | Filter database by predicate
filterDB :: (ModuleId -> Bool) -> (Project -> Bool) -> Database -> Database
filterDB m p db = mempty {
	databaseModules = filter (either (const False) (m . view moduleId) . view inspectionResult) (databaseModules db),
	databaseProjects = filter p (databaseProjects db) }

-- | Project database
projectDB :: Project -> Database -> Database
projectDB proj = filterDB (inProject proj) (((==) `on` view projectCabal) proj)

-- | Cabal database
cabalDB :: Cabal -> Database -> Database
cabalDB cabal = filterDB (inCabal cabal) (const False)

sandboxStackDB :: SandboxStack -> Database -> Database
sandboxStackDB sboxes = filterDB (inSandboxStack sboxes) (const False)

-- | Standalone database
standaloneDB :: Database -> Database
standaloneDB = filterDB check' (const False) where
	check' m = standalone m && byFile m

-- | Select module by predicate
selectModules :: (Module -> Bool) -> Database -> [Module]
selectModules p = filter p . allModules

-- | Select declaration by predicate
selectDeclarations :: (ModuleDeclaration -> Bool) -> Database -> [ModuleDeclaration]
selectDeclarations p = filter p . allDeclarations

-- | Lookup module by its location and name
lookupModule :: ModuleLocation -> Database -> Maybe Module
lookupModule mloc db = do
	m <- find ((== mloc) . view inspectedId) $ databaseModules db
	either (const Nothing) Just $ view inspectionResult m

-- | Lookup inspected module
lookupInspected :: ModuleLocation -> Database -> Maybe InspectedModule
lookupInspected mloc db = find ((== mloc) . view inspectedId) $ databaseModules db

-- | Lookup module by its source file
lookupFile :: FilePath -> Database -> Maybe Module
lookupFile f = listToMaybe . selectModules (inFile f . view moduleId)

-- | Refine project
refineProject :: Database -> Project -> Maybe Project
refineProject db proj = find ((== view projectCabal proj) . view projectCabal) $ databaseProjects db

-- | Get inspected module
getInspected :: Database -> Module -> InspectedModule
getInspected db m = fromMaybe err $ find ((== view moduleLocation m) . view inspectedId) $ databaseModules db where
	err = error "Impossible happened: getInspected"

-- | Append database
append :: Database -> Database -> Database
append = add

-- | Remove database
remove :: Database -> Database -> Database
remove = sub

-- | Structured database
data Structured = Structured {
	structuredCabals :: Map Cabal Database,
	structuredProjects :: Map FilePath Database,
	structuredFiles :: Database }
		deriving (Eq, Ord)

instance NFData Structured where
	rnf (Structured cs ps fs) = rnf cs `seq` rnf ps `seq` rnf fs

instance Group Structured where
	add old new = Structured {
		structuredCabals = structuredCabals new `M.union` structuredCabals old,
		structuredProjects = structuredProjects new `M.union` structuredProjects old,
		structuredFiles = structuredFiles old `add` structuredFiles new }
	sub old new = Structured {
		structuredCabals = structuredCabals old `M.difference` structuredCabals new,
		structuredProjects = structuredProjects old `M.difference` structuredProjects new,
		structuredFiles = structuredFiles old `sub` structuredFiles new }
	zero = Structured zero zero zero

instance Monoid Structured where
	mempty = zero
	mappend = add

instance ToJSON Structured where
	toJSON (Structured cs ps fs) = object [
		"cabals" .= M.elems cs,
		"projects" .= M.elems ps,
		"files" .= fs]

instance FromJSON Structured where
	parseJSON = withObject "structured" $ \v -> join $
		either fail return <$> (structured <$>
			(v .:: "cabals") <*>
			(v .:: "projects") <*>
			(v .:: "files"))

structured :: [Database] -> [Database] -> Database -> Either String Structured
structured cs ps fs = Structured <$> mkMap keyCabal cs <*> mkMap keyProj ps <*> pure fs where
	mkMap :: Ord a => (Database -> Either String a) -> [Database] -> Either String (Map a Database)
	mkMap key dbs = do
		keys <- mapM key dbs
		return $ M.fromList $ zip keys dbs
	keyCabal :: Database -> Either String Cabal
	keyCabal db = unique
		"No cabal"
		"Different module cabals"
		(ordNub <$> mapM getCabal (allModules db))
		where
			getCabal m = case view moduleLocation m of
				CabalModule c _ _ -> Right c
				_ -> Left "Module have no cabal"
	keyProj :: Database -> Either String FilePath
	keyProj db = unique
		"No project"
		"Different module projects"
		(return (databaseProjects db ^.. each . projectCabal))
	-- Check that list results in one element
	unique :: (Eq a) => String -> String -> Either String [a] -> Either String a
	unique _ _ (Left e) = Left e
	unique no _ (Right []) = Left no
	unique _ _ (Right [x]) = Right x
	unique _ much (Right _) = Left much

structurize :: Database -> Structured
structurize db = Structured cs ps fs where
	cs = M.fromList [(c, cabalDB c db) | c <- ordNub (mapMaybe modCabal (allModules db))]
	ps = M.fromList [(pname, projectDB (project pname) db) | pname <- databaseProjects db ^.. each . projectCabal]
	fs = standaloneDB db

merge :: Structured -> Database
merge (Structured cs ps fs) = mconcat $ M.elems cs ++ M.elems ps ++ [fs]

modCabal :: Module -> Maybe Cabal
modCabal m = case view moduleLocation m of
	CabalModule c _ _ -> Just c
	_ -> Nothing
