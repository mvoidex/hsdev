module HsDev.Database (
	Database(..),
	databaseIntersection, nullDatabase, allModules, allDeclarations,
	fromModule, fromProject,
	filterDB,
	projectDB, cabalDB, standaloneDB,
	selectModules, selectDeclarations, lookupModule, lookupFile,
	getInspected,

	append, remove
	) where

import Control.Monad
import Control.DeepSeq
import Data.Either (rights)
import Data.Function (on)
import Data.Group
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import qualified Data.Map as M

import HsDev.Symbols
import HsDev.Symbols.Util
import HsDev.Project

-- | HsDev database
data Database = Database {
	databaseModules :: Map ModuleLocation InspectedModule,
	databaseProjects :: Map String Project }
		deriving (Eq, Ord)

instance NFData Database where
	rnf (Database ms ps) = rnf ms `seq` rnf ps

instance Group Database where
	add old new = Database {
		databaseModules = databaseModules new `M.union` databaseModules old,
		databaseProjects = M.unionWith mergeProject (databaseProjects new) (databaseProjects old) }
		where
			mergeProject pl pr = pl {
				projectDescription = msum [projectDescription pl, projectDescription pr] }
	sub old new = Database {
		databaseModules = databaseModules old `M.difference` databaseModules new,
		databaseProjects = databaseProjects old `M.difference` databaseProjects new }
	zero = Database M.empty M.empty

instance Monoid Database where
	mempty = zero
	mappend = add

-- | Database intersection, prefers first database data
databaseIntersection :: Database -> Database -> Database
databaseIntersection l r = mempty {
	databaseModules = databaseModules l `M.intersection` databaseModules r,
	databaseProjects = databaseProjects l `M.intersection` databaseProjects r }

-- | Check if database is empty
nullDatabase :: Database -> Bool
nullDatabase db = M.null (databaseModules db) && M.null (databaseProjects db)

-- | All modules
allModules :: Database -> [Module]
allModules = rights . map inspectionResult . M.elems . databaseModules

-- | All declarations
allDeclarations :: Database -> [ModuleDeclaration]
allDeclarations db = do
	m <- allModules db
	moduleModuleDeclarations m

-- | Make database from module
fromModule :: InspectedModule -> Database
fromModule m = zero {
	databaseModules = M.singleton (inspectionModule m) m }

-- | Make database from project
fromProject :: Project -> Database
fromProject p = zero {
	databaseProjects = M.singleton (projectCabal p) p }

-- | Filter database by predicate
filterDB :: (Module -> Bool) -> (Project -> Bool) -> Database -> Database
filterDB m p db = mempty {
	databaseModules = M.filter (either (const False) m . inspectionResult) (databaseModules db),
	databaseProjects = M.filter p (databaseProjects db) }

-- | Project database
projectDB :: Project -> Database -> Database
projectDB proj = filterDB (inProject proj) (((==) `on` projectCabal) proj)

-- | Cabal database
cabalDB :: Cabal -> Database -> Database
cabalDB cabal = filterDB (inCabal cabal) (const False)

-- | Standalone database
standaloneDB :: Database -> Database
standaloneDB db = filterDB noProject (const False) db where
	noProject m = all (not . (flip inProject m)) ps
	ps = M.elems $ databaseProjects db

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
	either (const Nothing) Just $ inspectionResult m

-- | Lookup module by its source file
lookupFile :: FilePath -> Database -> Maybe Module
lookupFile f = listToMaybe . selectModules (inFile f)

-- | Get inspected module
getInspected :: Database -> Module -> InspectedModule
getInspected db m = fromMaybe err $ M.lookup (moduleLocation m) $ databaseModules db where
	err = error "Impossible happened: getInspected"

-- | Append database
append :: Database -> Database -> Database
append = add

-- | Remove database
remove :: Database -> Database -> Database
remove = sub
