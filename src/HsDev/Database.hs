module HsDev.Database (
	Database(..),
	databaseIntersection, nullDatabase, allModules, createIndexes,
	fromModule, fromProject,
	filterDB,
	projectDB, cabalDB, standaloneDB,
	selectModule, lookupModule, lookupFile,
	getInspected,

	append, remove
	) where

import Control.Arrow
import Control.Monad
import Control.DeepSeq
import Data.Either (rights)
import Data.Function (on)
import Data.Group
import Data.List
import Data.Map (Map)
import Data.Ord
import Data.Set (Set)
import Data.Maybe
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.HashMap as HM

import HsDev.Symbols
import HsDev.Symbols.Util
import HsDev.Project

-- | HsDev database
data Database = Database {
	databaseModules :: Map ModuleLocation InspectedModule,
	databaseProjects :: Map String Project,
	databaseCabalIndex :: Map Cabal (Set Module),
	databaseModulesIndex :: Map String (Set Module),
	databaseFilesIndex :: Map FilePath Module,
	databaseSymbolsIndex :: HM.Map String (Set ModuleDeclaration) }
		deriving (Eq, Ord)

instance NFData Database where
	rnf (Database ms ps mc mi fi si) = rnf ms `seq` rnf ps `seq` rnf mc `seq` rnf mi `seq` rnf fi `seq` rnf si

instance Group Database where
	add old new = Database {
		databaseModules = databaseModules new `M.union` databaseModules old,
		databaseProjects = M.unionWith mergeProject (databaseProjects new) (databaseProjects old),
		databaseCabalIndex = databaseCabalIndex old `add` databaseCabalIndex new,
		databaseModulesIndex = databaseModulesIndex old `add` databaseModulesIndex new,
		databaseFilesIndex = databaseFilesIndex old `M.union` databaseFilesIndex new,
		databaseSymbolsIndex = databaseSymbolsIndex old `add` databaseSymbolsIndex new }
		where
			mergeProject pl pr = pl {
				projectDescription = msum [projectDescription pl, projectDescription pr] }
	sub old new = Database {
		databaseModules = databaseModules old `M.difference` databaseModules new,
		databaseProjects = databaseProjects old `M.difference` databaseProjects new,
		databaseCabalIndex = databaseCabalIndex old `sub` databaseCabalIndex new,
		databaseModulesIndex = databaseModulesIndex old `sub` databaseModulesIndex new,
		databaseFilesIndex = databaseFilesIndex old `M.difference` databaseFilesIndex new,
		databaseSymbolsIndex = databaseSymbolsIndex old `sub` databaseSymbolsIndex new }
	zero = Database M.empty M.empty M.empty M.empty M.empty HM.empty

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

-- | Create indexes
createIndexes :: Database -> Database
createIndexes db = db {
	databaseCabalIndex = M.fromList $ map ((head *** S.fromList) . unzip) $ groupSort fst $ mapMaybe getCabal ms,
	databaseModulesIndex = M.fromList $ map ((moduleName . head) &&& S.fromList) $ groupSort moduleName ms,
	databaseFilesIndex = M.fromList $ mapMaybe getSrc ms,
	databaseSymbolsIndex = HM.fromList $ map ((declName . head) &&& S.fromList) $ groupSort declName $ concatMap moduleModuleDeclarations ms }
	where
		ms = allModules db
		getCabal m = case moduleLocation m of
			CabalModule c _ _ -> Just (c, m)
			_ -> Nothing
		getSrc m = case moduleLocation m of
			FileModule f _ -> Just (f, m)
			_ -> Nothing
		declName = declarationName . moduleDeclaration
		groupSort f = groupBy ((==) `on` f) . sortBy (comparing f)

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
selectModule :: Database -> (Module -> Bool) -> [Module]
selectModule db p = filter p $ rights $ map inspectionResult $ M.elems $ databaseModules db

-- | Lookup module by its location and name
lookupModule :: ModuleLocation -> Database -> Maybe Module
lookupModule mloc db = do
	m <- M.lookup mloc $ databaseModules db
	either (const Nothing) Just $ inspectionResult m

-- | Lookup module by its source file
lookupFile :: FilePath -> Database -> Maybe Module
lookupFile f = M.lookup f . databaseFilesIndex

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
