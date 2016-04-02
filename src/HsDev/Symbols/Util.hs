module HsDev.Symbols.Util (
	projectOf, packageDbOf, packageOf,
	inProject, inDepsOfTarget, inDepsOfFile, inDepsOfProject, inPackageDb, inPackageDbStack, inPackage, inVersion, inFile, inModuleSource, inModule, byFile, installed, standalone,
	imports, qualifier, moduleImported, visible, inScope,
	newestPackage,
	sourceModule, visibleModule, preferredModule, uniqueModules,
	allOf, anyOf
	) where

import Control.Arrow ((***), (&&&), first)
import Control.Lens (view)
import Control.Monad (liftM)
import Data.Function (on)
import Data.Maybe
import Data.List (maximumBy, groupBy, sortBy, partition)
import Data.Ord (comparing)
import Data.String (fromString)
import System.FilePath (normalise)

import HsDev.Symbols
import HsDev.Util (ordNub)

-- | Get module project
projectOf :: ModuleId -> Maybe Project
projectOf m = case view moduleIdLocation m of
	FileModule _ proj -> proj
	_ -> Nothing

-- | Get module cabal
packageDbOf :: ModuleId -> Maybe PackageDb
packageDbOf m = case view moduleIdLocation m of
	InstalledModule c _ _ -> Just c
	_ -> Nothing

-- | Get module package
packageOf :: ModuleId -> Maybe ModulePackage
packageOf m = case view moduleIdLocation m of
	InstalledModule _ package' _ -> package'
	_ -> Nothing

-- | Check if module in project
inProject :: Project -> ModuleId -> Bool
inProject p m = projectOf m == Just p

-- | Check if module in deps of project target
inDepsOfTarget :: Info -> ModuleId -> Bool
inDepsOfTarget i m = any (`inPackage` m) $ view infoDepends i

-- | Check if module in deps of source
inDepsOfFile :: Project -> FilePath -> ModuleId -> Bool
inDepsOfFile p f m = any (`inDepsOfTarget` m) (fileTargets p f)

-- | Check if module in deps of project
inDepsOfProject :: Project -> ModuleId -> Bool
inDepsOfProject = maybe (const False) (anyPackage . ordNub . concatMap (view infoDepends) . infos) . view projectDescription where
	anyPackage :: [String] -> ModuleId -> Bool
	anyPackage = liftM or . mapM inPackage

-- | Check if module in package-db
inPackageDb :: PackageDb -> ModuleId -> Bool
inPackageDb c m = case view moduleIdLocation m of
	InstalledModule d _ _ -> d == c
	_ -> False

-- | Check if module in one of sandboxes
inPackageDbStack :: PackageDbStack -> ModuleId -> Bool
inPackageDbStack dbs m = case view moduleIdLocation m of
	InstalledModule d _ _ -> d `elem` packageDbs dbs
	_ -> False

-- | Check if module in package
inPackage :: String -> ModuleId -> Bool
inPackage p m = case view moduleIdLocation m of
	InstalledModule _ package' _ -> Just p == fmap (view packageName) package'
	_ -> False

inVersion :: String -> ModuleId -> Bool
inVersion v m = case view moduleIdLocation m of
	InstalledModule _ package' _ -> Just v == fmap (view packageVersion) package'
	_ -> False

-- | Check if module in file
inFile :: FilePath -> ModuleId -> Bool
inFile fpath m = case view moduleIdLocation m of
	FileModule f _ -> f == normalise fpath
	_ -> False

-- | Check if module in source
inModuleSource :: Maybe String -> ModuleId -> Bool
inModuleSource src m = case view moduleIdLocation m of
	ModuleSource src' -> src' == src
	_ -> False

-- | Check if declaration is in module
inModule :: String -> ModuleId -> Bool
inModule mname m = fromString mname == view moduleIdName m

-- | Check if module defined in file
byFile :: ModuleId -> Bool
byFile m = case view moduleIdLocation m of
	FileModule _ _ -> True
	_ -> False

-- | Check if module got from cabal database
installed :: ModuleId -> Bool
installed m = case view moduleIdLocation m of
	InstalledModule _ _ _ -> True
	_ -> False

-- | Check if module is standalone
standalone :: ModuleId -> Bool
standalone m = case view moduleIdLocation m of
	FileModule _ Nothing -> True
	_ -> False

-- | Get list of imports
imports :: Module -> [Import]
imports = view moduleImports

-- | Get list of imports, which can be accessed with specified qualifier or unqualified
qualifier :: Module -> Maybe String -> [Import]
qualifier m q = filter (importQualifier (fmap fromString q)) $
	import_ (fromString "Prelude") :
	import_ (view moduleName m) :
	imports m

-- | Check if module imported via imports specified
moduleImported :: ModuleId -> [Import] -> Bool
moduleImported m = any (\i -> view moduleIdName m == view importModuleName i)

-- | Check if module visible from this module within this project
visible :: Project -> ModuleId -> ModuleId -> Bool
visible p (ModuleId _ (FileModule src _)) m =
	inProject p m || any (`inPackage` m) deps || maybe False ((`elem` deps) . view projectName) (projectOf m)
	where
		deps = concatMap (view infoDepends) $ fileTargets p src
visible _ _ _ = False

-- | Check if module is in scope with qualifier
inScope :: Module -> Maybe String -> ModuleId -> Bool
inScope this q m = m `moduleImported` qualifier this q

-- | Select symbols with last package version
newestPackage :: Symbol a => [a] -> [a]
newestPackage =
	uncurry (++) .
	((selectNewest . groupPackages) *** map snd) .
	partition (isJust . fst) .
	map ((mpackage . symbolModuleLocation) &&& id)
	where
		mpackage (InstalledModule _ (Just p) _) = Just p
		mpackage _ = Nothing
		pname = fmap (view packageName) . fst
		pver = fmap (view packageVersion) . fst
		groupPackages :: Symbol a => [(Maybe ModulePackage, a)] -> [(Maybe ModulePackage, [a])]
		groupPackages = map (first head . unzip) . groupBy ((==) `on` fst) . sortBy (comparing fst)
		selectNewest :: [(Maybe ModulePackage, [a])] -> [a]
		selectNewest =
			concatMap (snd . maximumBy (comparing pver)) .
			groupBy ((==) `on` pname) .
			sortBy (comparing pname)

-- | Select module, defined by sources
sourceModule :: Maybe Project -> [Module] -> Maybe Module
sourceModule proj ms = listToMaybe $ maybe (const []) (filter . (. view moduleId) . inProject) proj ms ++ filter (byFile . view moduleId) ms

-- | Select module, visible in project or cabal
-- TODO: PackageDbStack?
visibleModule :: PackageDb -> Maybe Project -> [Module] -> Maybe Module
visibleModule d proj ms = listToMaybe $ maybe (const []) (filter . (. view moduleId) . inProject) proj ms ++ filter (inPackageDb d . view moduleId) ms

-- | Select preferred visible module
-- TODO: PackageDbStack?
preferredModule :: PackageDb -> Maybe Project -> [ModuleId] -> Maybe ModuleId
preferredModule d proj ms = listToMaybe $ concatMap (`filter` ms) order where
	order = [
		maybe (const False) inProject proj,
		byFile,
		inPackageDb d,
		const True]

-- | Remove duplicate modules, leave only `preferredModule`
-- TODO: PackageDbStack?
uniqueModules :: PackageDb -> Maybe Project -> [ModuleId] -> [ModuleId]
uniqueModules d proj =
	mapMaybe (preferredModule d proj) .
	groupBy ((==) `on` view moduleIdName) .
	sortBy (comparing (view moduleIdName))

-- | Select value, satisfying to all predicates
allOf :: [a -> Bool] -> a -> Bool
allOf ps x = all ($ x) ps

-- | Select value, satisfying one of predicates
anyOf :: [a -> Bool] -> a -> Bool
anyOf ps x = any ($ x) ps

-- | Is file info actual?
--isActual :: Symbol a -> IO Bool
--isActual = maybe (return False) checkStamp . symbolLocation where
--	checkStamp l = do
--		actualStamp <- getModificationTime (locationFile l)
--		return $ Just actualStamp == locationTimeStamp l
