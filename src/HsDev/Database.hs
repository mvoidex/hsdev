{-# LANGUAGE OverloadedStrings, TemplateHaskell, RankNTypes #-}

module HsDev.Database (
	Database(..), databaseModules, databaseProjects, databaseProjectsInfos,
	databaseIntersection, nullDatabase, databasePackageDbs, databasePackages, databaseSandboxes,
	modules, symbols, packages,
	fromModule, fromProject, fromProjectInfo, fromPackageDb, fromPackage, fromPackageDbState,
	Slice, slice', pslice, slice, unionSlice, slices, inversedSlice,
	packageDbStackSlice, packageDbSlice, projectSlice, projectDepsSlice, targetSlice,
	newestPackagesSlice, standaloneSlice, filesSlice, sourcesSlice, freshSlice,

	refineProject,
	append, remove,

	atFile,

	Map
	) where

import Control.Lens hiding ((.=), (%=))
import Control.Monad (msum, liftM2)
import Control.DeepSeq (NFData(..))
import Data.Aeson
import Data.Group (Group(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe (mapMaybe, catMaybes, fromMaybe)
import System.FilePath

import HsDev.Sandbox
import HsDev.Symbols
import HsDev.Symbols.Util
import HsDev.Util ((.::), ordNub, ordUnion)
import System.Directory.Paths

-- | HsDev database
data Database = Database {
	_databaseModules :: Map ModuleLocation InspectedModule,
	_databaseProjects :: Map Path Project,
	_databaseProjectsInfos :: Map (Maybe Project) (PackageDbStack, [ModuleLocation]),
	_databasePackageDbs :: Map PackageDb [ModulePackage],
	_databasePackages :: Map ModulePackage [ModuleLocation] }
		deriving (Eq, Ord)

makeLenses ''Database

instance NFData Database where
	rnf (Database ms ps pms ds as) = rnf ms `seq` rnf ps `seq` rnf pms `seq` rnf ds `seq` rnf as

instance Group Database where
	add old new = Database {
		_databaseModules = M.union (_databaseModules new) (_databaseModules old),
		_databaseProjects = M.unionWith mergeProject (_databaseProjects new) (_databaseProjects old),
		_databaseProjectsInfos = M.unionWith mergeInfos (_databaseProjectsInfos new) (_databaseProjectsInfos old),
		_databasePackageDbs = M.unionWith ordUnion (_databasePackageDbs new) (_databasePackageDbs old),
		_databasePackages = M.unionWith ordUnion (_databasePackages new) (_databasePackages old) }
		where
			mergeProject pl pr = set projectDescription (msum [view projectDescription pl, view projectDescription pr]) pl
			mergeInfos (lpdb, lms) (_, rms) = (lpdb, ordUnion lms rms)
	sub old new = Database {
		_databaseModules = M.difference (_databaseModules old) (_databaseModules new),
		_databaseProjects = M.difference (_databaseProjects old) (_databaseProjects new),
		_databaseProjectsInfos = M.difference (_databaseProjectsInfos old) (_databaseProjectsInfos new),
		_databasePackageDbs = M.difference (_databasePackageDbs old) (_databasePackageDbs new),
		_databasePackages = M.difference (_databasePackages old) (_databasePackages new) }
	zero = Database mempty mempty mempty mempty mempty

instance Monoid Database where
	mempty = zero
	mappend = add

instance ToJSON Database where
	toJSON (Database ms ps pms ds as) = object [
		"modules" .= M.toList ms,
		"projects" .= M.toList ps,
		"projects-modules" .= M.toList pms,
		"package-dbs" .= M.toList ds,
		"packages" .= M.toList as]

instance FromJSON Database where
	parseJSON = withObject "database" $ \v -> Database <$>
		(M.fromList <$> v .:: "modules") <*>
		(M.fromList <$> v .:: "projects") <*>
		(M.fromList <$> v .:: "projects-modules") <*>
		(M.fromList <$> v .:: "package-dbs") <*>
		(M.fromList <$> v .:: "packages")

-- | Database intersection, prefers first database data
databaseIntersection :: Database -> Database -> Database
databaseIntersection l r = Database {
	_databaseModules = M.intersection (_databaseModules l) (_databaseModules r),
	_databaseProjects = M.intersection (_databaseProjects l) (_databaseProjects r),
	_databaseProjectsInfos = M.intersection (_databaseProjectsInfos l) (_databaseProjectsInfos r),
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
packages = databasePackageDbs . each . each

-- | Make database from module
fromModule :: InspectedModule -> Database
fromModule m = mempty {
	_databaseModules = M.singleton (view inspectedKey m) m,
	_databaseProjects = maybe mempty (\proj -> M.singleton (view projectCabal proj) proj) mproj,
	_databaseProjectsInfos = mempty,
	_databasePackageDbs = mempty,
	_databasePackages = maybe mempty (\pkg -> M.singleton pkg [mloc]) mpkg }
	where
		mloc = view inspectedKey m
		mproj = preview (moduleProject . _Just) mloc
		mpkg = preview (modulePackage . _Just) mloc

-- | Make database from project
fromProject :: Project -> Database
fromProject p = mempty {
	_databaseProjects = M.singleton (view projectCabal p) p }

fromProjectInfo :: Maybe Project -> PackageDbStack -> [ModuleLocation] -> Database
fromProjectInfo mproj pdbs mlocs = maybe mempty fromProject mproj `mappend` mempty {
	_databaseProjectsInfos = M.singleton mproj (pdbs, mlocs) }

fromPackageDb :: PackageDb -> [ModulePackage] -> Database
fromPackageDb pdb pkgs = mempty {
	_databasePackageDbs = M.singleton pdb pkgs }

fromPackage :: ModulePackage -> [ModuleLocation] -> Database
fromPackage pkg mlocs = mempty {
	_databasePackages = M.singleton pkg mlocs }

fromPackageDbState :: PackageDb -> Map ModulePackage [ModuleLocation] -> Database
fromPackageDbState pdb st = mempty {
	_databasePackageDbs = M.singleton pdb (M.keys st),
	_databasePackages = st }

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

packageDbListSlice :: [PackageDb] -> Slice
packageDbListSlice pdbs = slice' fn where
	fn db = db {
		_databaseModules = restrictKeys (_databaseModules db) (S.fromList mods'),
		_databaseProjects = mempty,
		_databaseProjectsInfos = mempty,
		_databasePackageDbs = restrictKeys (_databasePackageDbs db) (S.fromList pdbs),
		_databasePackages = restrictKeys (_databasePackages db) (S.fromList packages') }
		where
			packages' = ordNub $ concat $ catMaybes [M.lookup pdb (_databasePackageDbs db) | pdb <- pdbs]
			mods' = ordNub $ concat $ catMaybes [M.lookup pkg (_databasePackages db) | pkg <- packages']

-- | Leave only modules within specified @PackageDbStack@
packageDbStackSlice :: PackageDbStack -> Slice
packageDbStackSlice = packageDbListSlice . packageDbs

packageDbSlice :: PackageDb -> Slice
packageDbSlice = packageDbListSlice . return

-- | Leave only source modules within project
projectSlice :: Project -> Slice
projectSlice proj = slice' fn where
	fn db = db {
		_databaseModules = restrictKeys (_databaseModules db) (S.fromList [FileModule m' (Just proj) | m' <- modFiles']),
		_databaseProjects = restrictKeys (_databaseProjects db) (S.singleton (proj ^. projectCabal)),
		_databaseProjectsInfos = restrictKeys (_databaseProjectsInfos db) (S.singleton (Just proj)),
		_databasePackageDbs = mempty,
		_databasePackages = mempty }
		where
			modFiles' = concat [
				concatMap targets (proj ^.. projectDescription . _Just . projectLibrary . _Just),
				concatMap targets (proj ^.. projectDescription . _Just . projectExecutables . each),
				concatMap targets (proj ^.. projectDescription . _Just . projectTests . each)]

			targets :: Target t => t -> [Path]
			targets target' = map addRoot $ liftM2 subPath (target' ^. buildInfo . infoSourceDirsDef) (targetFiles target') where
				addRoot f
					| isRelative (view path f) = normPath $ (proj ^. projectPath) `subPath` f
					| otherwise = f

-- | Leave installed module project depends on
projectDepsSlice :: Project -> Slice
projectDepsSlice proj = slice' fn where
	fn db = view (packageDbStackSlice pdbs) db {
		_databaseModules = restrictKeys (_databaseModules db) (S.fromList mlocs),
		_databaseProjects = mempty,
		_databaseProjectsInfos = mempty,
		_databasePackages = restrictKeys (_databasePackages db) (S.fromList pkgs) }
		where
			pdbs = fromMaybe userDb (db ^? databaseProjectsInfos . ix (Just proj) . _1)
			deps = S.fromList $ concat [
				concatMap depends (proj ^.. projectDescription . _Just . projectLibrary . _Just),
				concatMap depends (proj ^.. projectDescription . _Just . projectExecutables . each),
				concatMap depends (proj ^.. projectDescription . _Just . projectTests . each)]
			pkgs = filter ((`S.member` deps) . view packageName) $ concat $ M.elems (_databasePackageDbs db)
			mlocs = ordNub $ concat $ catMaybes [M.lookup pkg (_databasePackages db) | pkg <- pkgs]

			depends :: Target t => t -> [Text]
			depends target' = target' ^. buildInfo . infoDepends

-- | Leave source modules within project's target
targetSlice :: Target a => Project -> a -> Slice
targetSlice proj t = slice' fn where
	fn db = db {
		_databaseModules = restrictKeys (_databaseModules db) (S.fromList [FileModule m' (Just proj) | m' <- modFiles']),
		_databaseProjects = restrictKeys (_databaseProjects db) (S.singleton (proj ^. projectCabal)),
		_databaseProjectsInfos = restrictKeys (_databaseProjectsInfos db) (S.singleton (Just proj)),
		_databasePackageDbs = mempty,
		_databasePackages = mempty }
		where
			modFiles' = map addRoot $ liftM2 subPath (t ^. buildInfo . infoSourceDirsDef) (targetFiles t) where
				addRoot f
					| isRelative (view path f) = normPath $ (proj ^. projectPath) `subPath` f
					| otherwise = f

-- | Remove old packages
newestPackagesSlice :: Slice
newestPackagesSlice = slice' g' where
	g' db = db {
		_databaseModules = restrictKeys (_databaseModules db) (S.fromList mlocs),
		_databaseProjects = mempty,
		_databaseProjectsInfos = mempty,
		_databasePackageDbs = M.filter (not . null) (M.map latestPackages (_databasePackageDbs db)),
		_databasePackages = restrictKeys (_databasePackages db) (S.fromList pkgs) }
		where
			pkgs = latestPackages $ ordNub $ (db ^.. databasePackageDbs . each . each) ++ (M.keys $ db ^. databasePackages)
			mlocs = ordNub $ concat [db ^.. databasePackages . ix pkg . each | pkg <- pkgs]

-- | Standalone database
standaloneSlice :: Slice
standaloneSlice = slice' fn where
	fn db = db {
		_databaseModules = restrictKeys (_databaseModules db) (S.fromList mlocs),
		_databaseProjects = mempty,
		_databaseProjectsInfos = restrictKeys (_databaseProjectsInfos db) (S.singleton Nothing),
		_databasePackageDbs = mempty,
		_databasePackages = mempty }
		where
			mlocs = db ^.. databaseProjectsInfos . ix Nothing . _2 . each

filesSlice :: [Path] -> Slice
filesSlice fs = slice' fn where
	fn db = db {
		_databaseModules = restrictKeys (_databaseModules db) (S.fromList mlocs),
		_databaseProjects = restrictKeys (_databaseProjects db) (S.fromList $ projs ^.. each . _Just . projectName),
		_databaseProjectsInfos = restrictKeys (_databaseProjectsInfos db) (S.fromList projs),
		_databasePackageDbs = mempty,
		_databasePackages = mempty }
		where
			mlocs = catMaybes [db ^? databaseModules . atFile f . inspected . moduleId . moduleLocation | f <- fs]
			projs = mlocs ^.. each . moduleProject

-- | Only source-code modules, no installed
sourcesSlice :: Slice
sourcesSlice = slice' fn where
	fn db = db {
		_databaseModules = restrictKeys (_databaseModules db) (S.fromList mlocs),
		_databasePackageDbs = mempty,
		_databasePackages = mempty }
		where
			projs = db ^.. databaseProjects . each
			mlocs = ordNub $ concat [db ^.. databaseProjectsInfos . ix proj . _2 . each | proj <- Nothing : map Just projs]

-- | Latest installed modules and source modules
freshSlice :: Slice
freshSlice = slices [newestPackagesSlice, sourcesSlice]

-- | Refine project
refineProject :: Database -> Project -> Maybe Project
refineProject db proj = (_databaseProjects db) ^? ix (view projectCabal proj)

-- | Append database
append :: Database -> Database -> Database
append = add

-- | Remove database
remove :: Database -> Database -> Database
remove = sub

-- | Get module at file
atFile :: Path -> Traversal' (Map ModuleLocation InspectedModule) InspectedModule
atFile fpath = ix (FileModule fpath Nothing)

restrictKeys :: Ord k => Map k a -> Set k -> Map k a
restrictKeys m s = M.intersection m (M.fromList [(k, ()) | k <- S.toList s])
