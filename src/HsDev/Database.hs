module HsDev.Database (
	Database(..),
	databaseIntersection,
	createIndexes,
	fromModule,
	fromProject,
	projectModules,
	cabalModules,

	lookupModule,
	lookupFile,

	append,
	remove
	) where

import Control.Monad
import Control.DeepSeq
import Data.Group
import Data.Map (Map)
import Data.Set (Set)
import Data.Maybe
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Set as S

import HsDev.Symbols
import HsDev.Project

-- | Info about modules
data Database = Database {
	databaseCabalModules :: Map Cabal (Map String (Symbol Module)),
	databaseFiles :: Map FilePath (Symbol Module),
	databaseProjects :: Map String Project,
	databaseModules :: Map String (Set (Symbol Module)),
	databaseSymbols :: Map String (Set (Symbol Declaration)) }
		deriving (Eq, Ord)

instance NFData Database where
	rnf (Database cm f p ms ss) = rnf cm `seq` rnf f `seq` rnf p `seq` rnf ms `seq` rnf ss

instance Group Database where
	add old new = force Database {
		databaseCabalModules = M.unionWith M.union (databaseCabalModules old') (databaseCabalModules new),
		databaseFiles = databaseFiles old' `M.union` databaseFiles new,
		databaseProjects = databaseProjects old' `M.union` databaseProjects new,
		databaseModules = add (databaseModules old') (databaseModules new),
		databaseSymbols = add (databaseSymbols old') (databaseSymbols new) }
		where
			old' = sub old (databaseIntersection old new)
	sub old new = force Database {
		databaseCabalModules = M.differenceWith diff' (databaseCabalModules old) (databaseCabalModules new),
		databaseFiles = M.difference (databaseFiles old) (databaseFiles new),
		databaseProjects = M.difference (databaseProjects old) (databaseProjects new),
		databaseModules = sub (databaseModules old) (databaseModules new),
		databaseSymbols = sub (databaseSymbols old) (databaseSymbols new) }
		where
			diff' x y = if M.null z then Nothing else Just z where
				z = M.difference x y
	zero = Database M.empty M.empty M.empty M.empty M.empty

instance Monoid Database where
	mempty = zero
	mappend = add

-- | Database intersection, returns data from first database
databaseIntersection :: Database -> Database -> Database
databaseIntersection l r = force $ createIndexes $ mempty {
	databaseCabalModules = M.intersectionWith M.intersection (databaseCabalModules l) (databaseCabalModules r),
	databaseFiles = M.intersection (databaseFiles l) (databaseFiles r),
	databaseProjects = M.intersection (databaseProjects l) (databaseProjects r) }

-- | Create indexes
createIndexes :: Database -> Database
createIndexes db = force $ db {
	databaseModules = groupSum $ map (\m -> M.singleton (symbolName m) (S.singleton m)) ms,
	databaseSymbols = groupSum $ map (M.map S.singleton . moduleDeclarations . symbol) ms }
	where
		ms = concatMap M.elems (M.elems (databaseCabalModules db)) ++ M.elems (databaseFiles db)

-- | Make database from module
fromModule :: Symbol Module -> Database
fromModule m = force $ fromMaybe (error "Module must specify source file or cabal") (inSource `mplus` inCabal) where
	inSource = do
		loc <- symbolLocation m
		return $ createIndexes $ Database mempty (M.singleton (locationFile loc) m) mempty zero zero
	inCabal = do
		cabal <- moduleCabal $ symbol m
		return $ createIndexes $ Database (M.singleton cabal (M.singleton (symbolName m) m)) mempty mempty zero zero

-- | Make database from project
fromProject :: Project -> Database
fromProject p = zero {
	databaseProjects = M.singleton (projectCabal p) p }

-- Modules for project specified
projectModules :: Project -> Database -> Map FilePath (Symbol Module)
projectModules proj = M.filter thisProject . databaseFiles where
	thisProject m = maybe False (== proj) $ do
		loc <- symbolLocation m
		locationProject loc

-- | Modules for cabal specified
cabalModules :: Cabal -> Database -> Map String (Symbol Module)
cabalModules cabal db = fromMaybe M.empty $ M.lookup cabal $ databaseCabalModules db
		
lookupModule :: Cabal -> String -> Database -> Maybe (Symbol Module)
lookupModule cabal name db = do
	c <- M.lookup cabal $ databaseCabalModules db
	M.lookup name c

lookupFile :: FilePath -> Database -> Maybe (Symbol Module)
lookupFile file db = M.lookup file (databaseFiles db)

append :: Database -> Database -> Database
append = add

remove :: Database -> Database -> Database
remove = sub
