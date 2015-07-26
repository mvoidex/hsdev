{-# LANGUAGE OverloadedStrings #-}

module HsDev.Database (
	Database(..),
	databaseIntersection, nullDatabase, databaseLocals, allModules, allDeclarations,
	fromModule, fromProject,
	filterDB,
	projectDB, cabalDB, standaloneDB,
	selectModules, selectDeclarations, lookupModule, lookupFile, refineProject,
	getInspected,

	append, remove,

	Structured(..),
	structured, structurize, merge,

	Map
	) where

import Control.Lens (set, view)
import Control.Monad (msum, join)
import Control.DeepSeq (NFData(..))
import Data.Aeson
import Data.Either (rights)
import Data.Function (on)
import Data.Group (Group(..))
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M

import HsDev.Symbols
import HsDev.Symbols.Util
import HsDev.Util ((.::), ordNub)

-- | HsDev database
data Database = Database {
	databaseModules :: Map ModuleLocation InspectedModule,
	databaseProjects :: Map FilePath Project }
		deriving (Eq, Ord)

instance NFData Database where
	rnf (Database ms ps) = rnf ms `seq` rnf ps

instance Group Database where
	add old new = Database {
		databaseModules = databaseModules new `M.union` databaseModules old,
		databaseProjects = M.unionWith mergeProject (databaseProjects new) (databaseProjects old) }
		where
			mergeProject pl pr = set projectDescription (msum [view projectDescription pl, view projectDescription pr]) pl
	sub old new = Database {
		databaseModules = databaseModules old `M.difference` databaseModules new,
		databaseProjects = databaseProjects old `M.difference` databaseProjects new }
	zero = Database M.empty M.empty

instance Monoid Database where
	mempty = zero
	mappend = add

instance ToJSON Database where
	toJSON (Database ms ps) = object [
		"modules" .= M.elems ms,
		"projects" .= M.elems ps]

instance FromJSON Database where
	parseJSON = withObject "database" $ \v -> Database <$>
		((M.unions . map mkModule) <$> v .:: "modules") <*>
		((M.unions . map mkProject) <$> v .:: "projects")
		where
			mkModule m = M.singleton (view inspectedId m) m
			mkProject p = M.singleton (view projectCabal p) p

-- | Database intersection, prefers first database data
databaseIntersection :: Database -> Database -> Database
databaseIntersection l r = mempty {
	databaseModules = databaseModules l `M.intersection` databaseModules r,
	databaseProjects = databaseProjects l `M.intersection` databaseProjects r }

-- | Check if database is empty
nullDatabase :: Database -> Bool
nullDatabase db = M.null (databaseModules db) && M.null (databaseProjects db)

-- | Bring all locals to scope
databaseLocals :: Database -> Database
databaseLocals db = db {
	databaseModules = M.map (fmap moduleLocals) (databaseModules db) }

-- | All modules
allModules :: Database -> [Module]
allModules = rights . map (view inspectionResult) . M.elems . databaseModules

-- | All declarations
allDeclarations :: Database -> [ModuleDeclaration]
allDeclarations db = do
	m <- allModules db
	moduleModuleDeclarations m

-- | Make database from module
fromModule :: InspectedModule -> Database
fromModule m = zero {
	databaseModules = M.singleton (view inspectedId m) m }

-- | Make database from project
fromProject :: Project -> Database
fromProject p = zero {
	databaseProjects = M.singleton (view projectCabal p) p }

-- | Filter database by predicate
filterDB :: (ModuleId -> Bool) -> (Project -> Bool) -> Database -> Database
filterDB m p db = mempty {
	databaseModules = M.filter (either (const False) (m . view moduleId) . view inspectionResult) (databaseModules db),
	databaseProjects = M.filter p (databaseProjects db) }

-- | Project database
projectDB :: Project -> Database -> Database
projectDB proj = filterDB (inProject proj) (((==) `on` view projectCabal) proj)

-- | Cabal database
cabalDB :: Cabal -> Database -> Database
cabalDB cabal = filterDB (inCabal cabal) (const False)

-- | Standalone database
standaloneDB :: Database -> Database
standaloneDB db = filterDB check' (const False) db where
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
	m <- M.lookup mloc $ databaseModules db
	either (const Nothing) Just $ view inspectionResult m

-- | Lookup module by its source file
lookupFile :: FilePath -> Database -> Maybe Module
lookupFile f = listToMaybe . selectModules (inFile f . view moduleId)

-- | Refine project
refineProject :: Database -> Project -> Maybe Project
refineProject db proj = M.lookup (view projectCabal proj) $ databaseProjects db

-- | Get inspected module
getInspected :: Database -> Module -> InspectedModule
getInspected db m = fromMaybe err $ M.lookup (view moduleLocation m) $ databaseModules db where
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
		(return (M.keys (databaseProjects db)))
	-- Check that list results in one element
	unique :: (Eq a) => String -> String -> Either String [a] -> Either String a
	unique _ _ (Left e) = Left e
	unique no _ (Right []) = Left no
	unique _ _ (Right [x]) = Right x
	unique _ much (Right _) = Left much

structurize :: Database -> Structured
structurize db = Structured cs ps fs where
	cs = M.fromList [(c, cabalDB c db) | c <- ordNub (mapMaybe modCabal (allModules db))]
	ps = M.fromList [(pname, projectDB (project pname) db) | pname <- M.keys (databaseProjects db)]
	fs = standaloneDB db

merge :: Structured -> Database
merge (Structured cs ps fs) = mconcat $ M.elems cs ++ M.elems ps ++ [fs]

modCabal :: Module -> Maybe Cabal
modCabal m = case view moduleLocation m of
	CabalModule c _ _ -> Just c
	_ -> Nothing
