{-# LANGUAGE OverloadedStrings, TemplateHaskell, RankNTypes #-}

module HsDev.Database (
	Database(..), databaseModules, databaseProjects,
	databaseIntersection, nullDatabase, databasePackageDbs, databaseSandboxes,
	modules, symbols, packages,
	fromModule, fromProject,
	Slice, slice', pslice, slice, unionSlice, slices, inversedSlice,
	targetSlice, projectSlice, packageSlice, newestPackagesSlice, sandboxSlice,
	targetDepsSlice, projectDepsSlice, packageDbSlice, packageDbStackSlice, standaloneSlice, fileDepsSlice,
	refineProject,
	
	append, remove,

	Structured(..),
	structured, structurize, merge,

	Map
	) where

import Control.Lens hiding ((.=), (%=))
import Control.Monad (msum, join)
import Control.DeepSeq (NFData(..))
import Data.Aeson
import Data.Function (on)
import Data.Group (Group(..))
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

import HsDev.Sandbox
import HsDev.Symbols
import HsDev.Symbols.Util
import HsDev.Util ((.::), ordNub)

-- | HsDev database
data Database = Database {
	_databaseModules :: Map ModuleLocation InspectedModule,
	_databaseProjects :: Map FilePath Project }
		deriving (Eq, Ord)

makeLenses ''Database

instance NFData Database where
	rnf (Database ms ps) = rnf ms `seq` rnf ps

instance Group Database where
	add old new = Database {
		_databaseModules = M.union (_databaseModules new) (_databaseModules old),
		_databaseProjects = M.unionWith mergeProject (_databaseProjects new) (_databaseProjects old) }
		where
			mergeProject pl pr = set projectDescription (msum [view projectDescription pl, view projectDescription pr]) pl
	sub old new = Database {
		_databaseModules = M.difference (_databaseModules old) (_databaseModules new),
		_databaseProjects = M.difference (_databaseProjects old) (_databaseProjects new) }
	zero = Database mempty mempty

instance Monoid Database where
	mempty = zero
	mappend = add

instance ToJSON Database where
	toJSON (Database ms ps) = object [
		"modules" .= M.toList ms,
		"projects" .= M.toList ps]

instance FromJSON Database where
	parseJSON = withObject "database" $ \v -> Database <$>
		(M.fromList <$> v .:: "modules") <*>
		(M.fromList <$> v .:: "projects")

-- | Database intersection, prefers first database data
databaseIntersection :: Database -> Database -> Database
databaseIntersection l r = Database {
	_databaseModules = M.intersection (_databaseModules l) (_databaseModules r),
	_databaseProjects = M.intersection (_databaseProjects l) (_databaseProjects r) }

-- | Check if database is empty
nullDatabase :: Database -> Bool
nullDatabase db = M.null (_databaseModules db) && M.null (_databaseProjects db)

-- | All scanned sandboxes
databasePackageDbs :: Database -> [PackageDb]
databasePackageDbs db = ordNub $ M.keys (_databaseModules db) ^.. each . modulePackageDb

-- | Scanned sandboxes
databaseSandboxes :: Database -> [Sandbox]
databaseSandboxes = mapMaybe packageDbSandbox . databasePackageDbs

-- | All modules
modules :: Traversal' Database Module
modules = databaseModules . each . inspected

-- | All symbols
symbols :: Traversal' Database Symbol
symbols = modules . moduleExports . each

-- | All packages
packages :: Traversal' Database ModulePackage
packages = modules . moduleId . moduleLocation . modulePackage . _Just

-- | Make database from module
fromModule :: InspectedModule -> Database
fromModule m = mempty {
	_databaseModules = M.singleton (view inspectedKey m) m }

-- | Make database from project
fromProject :: Project -> Database
fromProject p = mempty {
	_databaseProjects = M.singleton (view projectCabal p) p }

type SliceF f = (Database -> f Database) -> (Database -> f Database)
type SliceC = SliceF (Const Database)
type Slice = forall f . Functor f => SliceF f

slice' :: (Database -> Database) -> Slice
slice' f = lens f s' where
	s' db db' = (db `sub` f db) `add` db'

-- | Slice of database for projects
pslice :: (Project -> Bool) -> Slice
pslice p = slice' g' where
	g' db = mempty { _databaseProjects = M.filterWithKey (const p) (_databaseProjects db) }

-- | Slice of database
slice :: (ModuleId -> Bool) -> Slice
slice m = slice' g' where
	g' db = mempty {
		_databaseModules = M.filter (maybe False m . preview (inspected . moduleId)) (_databaseModules db) }

-- | Union slices
unionSlice :: Slice -> Slice -> Slice
unionSlice l r = slice' g' where
	g' db = mconcat [view l db, view r db]

-- | Union slices
slices :: [SliceC] -> Slice
slices ss = slice' g' where
	g' db = mconcat [getConst (s Const db) | s <- ss]

inversedSlice :: Slice -> Slice
inversedSlice s = slice' g' where
	g' db = db `sub` view s db

targetSlice :: Info -> Slice
targetSlice info = slice (inTarget info)

projectSlice :: Project -> Slice
projectSlice proj = slice (inProject proj) `unionSlice` pslice (((==) `on` view projectCabal) proj)

packageSlice :: ModulePackage -> Slice
packageSlice pkg = slice (inPackage pkg)

newestPackagesSlice :: Slice
newestPackagesSlice = slice' g' where
	g' db = db {
		_databaseModules = M.filter (maybe True (`elem` pkgs) . preview (inspected . moduleId . moduleLocation . modulePackage . _Just)) (_databaseModules db) }
		where
			pkgs = latestPackages (db ^.. modules . moduleId . moduleLocation . modulePackage . _Just)

-- | Restrict only sandbox for path
sandboxSlice :: FilePath -> Slice
sandboxSlice fpath = slice' g' where
	g' db = db {
		_databaseModules = M.filter (maybe True ((== mbox) . packageDbSandbox) . preview (inspected . moduleId . moduleLocation . modulePackageDb)) (_databaseModules db) }
		where
			mbox = find (pathInSandbox fpath) $ databaseSandboxes db

-- | Dependencies of target
targetDepsSlice :: Project -> Info -> Slice
targetDepsSlice proj target = sandboxSlice (view projectPath proj) . slice (inDepsOfTarget proj target)

-- | Dependencies of project
projectDepsSlice :: Project -> Slice
projectDepsSlice proj = sandboxSlice (view projectPath proj) . slice (inDepsOfProject proj)

-- | Package-db database
packageDbSlice :: PackageDb -> Slice
packageDbSlice pdb = slice (inPackageDb pdb)

-- | Package-db stack database
packageDbStackSlice :: PackageDbStack -> Slice
packageDbStackSlice pdbs = slice (inPackageDbStack pdbs)

-- | Standalone database
standaloneSlice :: Slice
standaloneSlice = slice check' where
	check' m = standalone m && byFile m

fileDepsSlice :: FilePath -> Slice
fileDepsSlice fpath = sandboxSlice fpath . slice' g' where
	g' db = case mproj of
		Nothing -> db ^. slices [standaloneSlice, slice installed]
		Just proj -> db ^. slices (projectSlice proj : map (targetDepsSlice proj) targets') where
			targets' = fileTargets proj fpath
		where
			mproj = db ^? modules . filtered (inFile fpath) . moduleId . moduleLocation . moduleProject . _Just

-- | Refine project
refineProject :: Database -> Project -> Maybe Project
refineProject db proj = (_databaseProjects db) ^? ix (view projectCabal proj)

-- | Append database
append :: Database -> Database -> Database
append = add

-- | Remove database
remove :: Database -> Database -> Database
remove = sub

-- | Structured database
data Structured = Structured {
	structuredPackageDbs :: Map PackageDb Database,
	structuredProjects :: Map FilePath Database,
	structuredFiles :: Database }
		deriving (Eq, Ord)

instance NFData Structured where
	rnf (Structured cs ps fs) = rnf cs `seq` rnf ps `seq` rnf fs

instance Group Structured where
	add old new = Structured {
		structuredPackageDbs = structuredPackageDbs new `M.union` structuredPackageDbs old,
		structuredProjects = structuredProjects new `M.union` structuredProjects old,
		structuredFiles = structuredFiles old `add` structuredFiles new }
	sub old new = Structured {
		structuredPackageDbs = structuredPackageDbs old `M.difference` structuredPackageDbs new,
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
	keyCabal :: Database -> Either String PackageDb
	keyCabal db = unique
		"No cabal"
		"Different module cabals"
		(ordNub (M.keys (_databaseModules db) ^.. each . modulePackageDb))
	keyProj :: Database -> Either String FilePath
	keyProj db = unique
		"No project"
		"Different module projects"
		(M.keys $ _databaseProjects db)
	-- Check that list results in one element
	unique :: String -> String -> [a] -> Either String a
	unique _ _ [x] = Right x
	unique no _ [] = Left no
	unique _ much _ = Left much

structurize :: Database -> Structured
structurize db = Structured cs ps fs where
	cs = M.fromList [(c, db ^. packageDbSlice c) | c <- ordNub (M.keys (_databaseModules db) ^.. each . modulePackageDb)]
	ps = M.fromList [(pname, db ^. projectSlice (project pname)) | pname <- M.keys (_databaseProjects db)]
	fs = db ^. standaloneSlice

merge :: Structured -> Database
merge (Structured cs ps fs) = mconcat $ M.elems cs ++ M.elems ps ++ [fs]
