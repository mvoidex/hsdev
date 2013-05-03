module HsDev.Database (
	Database(..),
	createIndexes,
	fromModule,
	--projectModules,

	lookupModule,
	lookupFile,

	append,
	remove
	) where

import Control.Arrow
import Control.Monad
import Data.Group
import Data.List
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

instance Group Database where
	add l r = Database {
		databaseCabalModules = M.unionWith M.union (databaseCabalModules l) (databaseCabalModules r),
		databaseFiles = M.union (databaseFiles l) (databaseFiles r),
		databaseProjects = M.union (databaseProjects l) (databaseProjects r),
		databaseModules = add (databaseModules l) (databaseModules r),
		databaseSymbols = add (databaseSymbols l) (databaseSymbols r) }
	sub l r = Database {
		databaseCabalModules = M.differenceWith diff' (databaseCabalModules l) (databaseCabalModules r),
		databaseFiles = M.difference (databaseFiles l) (databaseFiles r),
		databaseProjects = M.difference (databaseProjects l) (databaseProjects r),
		databaseModules = sub (databaseModules l) (databaseModules r),
		databaseSymbols = sub (databaseSymbols l) (databaseSymbols r) }
		where
			diff' x y = if M.null z then Nothing else Just z where
				z = M.difference x y
	zero = Database M.empty M.empty M.empty M.empty M.empty

instance Monoid Database where
	mempty = zero
	mappend = add

-- | Create indexes
createIndexes :: Database -> Database
createIndexes db = db {
	databaseModules = groupSum $ map (\m -> M.singleton (symbolName m) (S.singleton m)) ms,
	databaseSymbols = groupSum $ map (M.map S.singleton . moduleDeclarations . symbol) ms }
	where
		ms = concatMap M.elems (M.elems (databaseCabalModules db)) ++ M.elems (databaseFiles db)

-- | Make database from module
fromModule :: Symbol Module -> Database
fromModule m = fromMaybe (error "Module must specify source file or cabal") $ inSource `mplus` inCabal where
	inSource = do
		loc <- symbolLocation m
		return $ createIndexes $ Database mempty (M.singleton (locationFile loc) m) mempty zero zero
	inCabal = do
		cabal <- moduleCabal $ symbol m
		return $ createIndexes $ Database (M.singleton cabal (M.singleton (symbolName m) m)) mempty mempty zero zero

-- Modules for project specified
--projectModules :: String -> Database -> Map FilePath (Symbol Module)
--projectModules project = M.filter thisProject . databaseFiles where
--	thisProject m = maybe False (== project) $ do
--		loc <- symbolLocation m
--		locationProject loc

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
