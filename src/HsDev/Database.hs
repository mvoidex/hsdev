{-# LANGUAGE OverloadedStrings, TemplateHaskell, RankNTypes #-}

module HsDev.Database (
	Database(..), databaseModules, databaseProjects,
	databaseIntersection, nullDatabase, databasePackageDbs, databasePackages, databaseSandboxes,
	modules, symbols, packages,
	fromModule, fromProject, fromPackageDb, fromPackage,
	Slice, slice', pslice, slice, unionSlice, slices, inversedSlice,
	targetSlice, projectSlice, packageSlice, newestPackagesSlice, sandboxSlice,
	targetDepsSlice, projectDepsSlice, packageDbSlice, packageDbStackSlice, standaloneSlice, fileDepsSlice,
	refineProject,
	
	append, remove,

	Map
	) where

import Control.Lens hiding ((.=), (%=))
import Control.Monad (msum)
import Control.DeepSeq (NFData(..))
import Data.Aeson
import Data.Function (on)
import Data.Group (Group(..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe (mapMaybe, catMaybes)

import HsDev.Sandbox
import HsDev.Symbols
import HsDev.Symbols.Util
import HsDev.Util ((.::))

-- | HsDev database
data Database = Database {
	_databaseModules :: Map ModuleLocation InspectedModule,
	_databaseProjects :: Map FilePath Project,
	_databasePackageDbs :: Map PackageDb (Set ModulePackage),
	_databasePackages :: Map ModulePackage (Set ModuleLocation) }
		deriving (Eq, Ord)

makeLenses ''Database

instance NFData Database where
	rnf (Database ms ps ds as) = rnf ms `seq` rnf ps `seq` rnf ds `seq` rnf as

instance Group Database where
	add old new = Database {
		_databaseModules = M.union (_databaseModules new) (_databaseModules old),
		_databaseProjects = M.unionWith mergeProject (_databaseProjects new) (_databaseProjects old),
		_databasePackageDbs = M.unionWith S.union (_databasePackageDbs new) (_databasePackageDbs old),
		_databasePackages = M.unionWith S.union (_databasePackages new) (_databasePackages old) }
		where
			mergeProject pl pr = set projectDescription (msum [view projectDescription pl, view projectDescription pr]) pl
	sub old new = Database {
		_databaseModules = M.difference (_databaseModules old) (_databaseModules new),
		_databaseProjects = M.difference (_databaseProjects old) (_databaseProjects new),
		_databasePackageDbs = M.difference (_databasePackageDbs old) (_databasePackageDbs new),
		_databasePackages = M.difference (_databasePackages old) (_databasePackages new) }
	zero = Database mempty mempty mempty mempty

instance Monoid Database where
	mempty = zero
	mappend = add

instance ToJSON Database where
	toJSON (Database ms ps ds as) = object [
		"modules" .= M.toList ms,
		"projects" .= M.toList ps,
		"package-dbs" .= M.toList ds,
		"packages" .= M.toList as]

instance FromJSON Database where
	parseJSON = withObject "database" $ \v -> Database <$>
		(M.fromList <$> v .:: "modules") <*>
		(M.fromList <$> v .:: "projects") <*>
		(M.fromList <$> v .:: "package-dbs") <*>
		(M.fromList <$> v .:: "packages")

-- | Database intersection, prefers first database data
databaseIntersection :: Database -> Database -> Database
databaseIntersection l r = Database {
	_databaseModules = M.intersection (_databaseModules l) (_databaseModules r),
	_databaseProjects = M.intersection (_databaseProjects l) (_databaseProjects r),
	_databasePackageDbs = M.intersection (_databasePackageDbs l) (_databasePackageDbs r),
	_databasePackages = M.intersection (_databasePackages l) (_databasePackages r) }

-- | Check if database is empty
nullDatabase :: Database -> Bool
nullDatabase db = db == mempty

-- | Scanned sandboxes
databaseSandboxes :: Database -> [Sandbox]
databaseSandboxes = mapMaybe packageDbSandbox . M.keys . _databasePackageDbs

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

fromPackageDb :: PackageDb -> [ModulePackage] -> Database
fromPackageDb pdb pkgs = mempty {
	_databasePackageDbs = M.singleton pdb (S.fromList pkgs) }

fromPackage :: ModulePackage -> [ModuleLocation] -> Database
fromPackage pkg mlocs = mempty {
	_databasePackages = M.singleton pkg (S.fromList mlocs) }

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
unionSlice :: SliceC -> SliceC -> Slice
unionSlice l r = slice' g' where
	g' db = mconcat [getConst (l Const db), getConst (r Const db)]

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
sandboxSlice = const id
-- sandboxSlice fpath = slice' g' where
-- 	g' db = db {
-- 		_databaseModules = M.filter (maybe True ((== mbox) . packageDbSandbox) . preview (inspected . moduleId . moduleLocation . modulePackageDb)) (_databaseModules db) }
-- 		where
-- 			mbox = find (pathInSandbox fpath) $ databaseSandboxes db

-- | Dependencies of target
targetDepsSlice :: Project -> Info -> Slice
targetDepsSlice proj target = sandboxSlice (view projectPath proj) . slice (inDepsOfTarget proj target)

-- | Dependencies of project
projectDepsSlice :: Project -> Slice
projectDepsSlice proj = sandboxSlice (view projectPath proj) . slice (inDepsOfProject proj)

-- | Package-db database
packageDbSlice :: PackageDb -> Slice
packageDbSlice pdb = slice' inPackageDb' where
	inPackageDb' db = db {
		_databaseModules = M.filter (maybe False (`S.member` mlocs) . preview (inspected . moduleId . moduleLocation)) (_databaseModules db) }
		where
			pkgs = maybe [] S.toList $ M.lookup pdb $ _databasePackageDbs db
			mlocs = S.unions $ catMaybes [M.lookup pkg (_databasePackages db) | pkg <- pkgs]

-- | Package-db stack database
packageDbStackSlice :: PackageDbStack -> Slice
packageDbStackSlice pdbs = slice' inPackageDb' where
	inPackageDb' db = db {
		_databaseModules = M.filter (maybe False (`S.member` mlocs) . preview (inspected . moduleId . moduleLocation)) (_databaseModules db) }
		where
			pkgs = S.toList $ S.unions $ catMaybes [M.lookup pdb (_databasePackageDbs db) | pdb <- packageDbs pdbs]
			mlocs = S.unions $ catMaybes [M.lookup pkg (_databasePackages db) | pkg <- pkgs]

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
