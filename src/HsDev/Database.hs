module HsDev.Database (
	Database(..)
	) where

import Data.Map (Map)
import qualified Data.Map as M

import HsDev.Symbols

-- | Info about modules
data Database = Database {
	databaseCabalModules :: Map Cabal (Map String (Symbol Module)),
	databaseFiles :: Map FilePath (Symbol Module),
	databaseModules :: Map String [Symbol Module],
	databaseSymbols :: Map String [Symbol Declaration] }
		deriving (Eq, Ord, Read, Show)

-- Modules for project specified
projectModules :: String -> Database -> Map FilePath (Symbol Module)
projectModules project = M.filter thisProject . databaseFiles where
	thisProject m = maybe False (== project) $ do
		loc <- symbolLocation m
		locationProject loc

-- | Remove indexes for module
removeIndexes :: Symbol Module -> Database -> Database
removeIndexes m db = db {
	databaseModules = M.update (Just . filter (/= m)) (symbolName m) (databaseModules db),
	databaseSymbols = M.map (filter (not . inModule)) (databaseSymbols db) }
	where
		inModule = (`elem` (M.elems (moduleDeclarations $ symbol m)))

setIndexes :: Symbol Module -> Database -> Database
setIndexes m db = db {
	databaseModules = M.unionWith (++) (databaseModules db) module',
	databaseSymbols = M.unionWith (++) (databaseSymbols db) symbols' }
	where
		module' = M.singleton (symbolName m) [m]
		symbols' = M.map return $ moduleDeclarations $ symbol m

-- | Remove module from database
removeModule :: Cabal -> String -> Database -> Database
removeModule cabal moduleName db = maybe db removeModule' module' where
	removeModule' m = removeIndexes m $ db {
		databaseCabalModules = M.update remove' cabal (databaseCabalModules db) }
	remove' = Just . M.delete moduleName
	module' = do
		c <- M.lookup cabal $ databaseCabalModules db
		M.lookup moduleName c

-- | Add module to database
addModule :: Symbol Module -> Database -> Database
addModule m db = maybe noCabal add' (moduleCabal $ symbol m) where
	noCabal = error "HsDev.Database.addModule: cabal not specified in module"
	add' cabal = setIndexes m $ db' {
		databaseCabalModules = M.update (Just . (`M.union` module')) cabal (databaseCabalModules db') }
		where
			db' = removeModule cabal (symbolName m) db
			module' = M.singleton (symbolName m) m

-- | Remove file from database
removeFile :: FilePath -> Database -> Database
removeFile file db = maybe db removeFile' module' where
	removeFile' m = removeIndexes m $ db {
		databaseFiles = M.delete file (databaseFiles db) }
	module' = M.lookup file $ databaseFiles db

-- | Add file to database
addFile :: FilePath -> Symbol Module -> Database -> Database
addFile file m db = setIndexes m $ (removeFile file db) {
	databaseFiles = M.union (databaseFiles db) (M.singleton file m) }
