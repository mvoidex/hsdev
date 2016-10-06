module HsDev.Symbols.Util (
	withName, inProject, inTarget, inDepsOfTarget, inDepsOfFile, inDepsOfProject, inPackageDb, inPackageDbStack,
	inPackage, inFile, inOtherLocation, inModule,
	byFile, installed, standalone,
	latestPackages, newestPackage,
	allOf, anyOf
	) where

import Control.Lens (preview, view, _Just, (^..), (^.), each)
import Data.Function (on)
import Data.Maybe
import Data.List (maximumBy, groupBy, sortBy)
import Data.Ord (comparing)
import Data.String (fromString)
import Data.Text (Text)
import System.FilePath (normalise)

import HsDev.Symbols
import HsDev.Util (ordNub)

-- | Check is sourced has name
withName :: Sourced a => Text -> a -> Bool
withName nm = (== nm) . view sourcedName

-- | Check if module in project
inProject :: Sourced s => Project -> s -> Bool
inProject p m = preview (sourcedModule . moduleLocation . moduleProject . _Just) m == Just p

-- | Check if module in target
inTarget :: Sourced s => Info -> s -> Bool
inTarget i = maybe False (`fileInTarget` i) . preview (sourcedModule . moduleLocation . moduleFile)

-- | Check if module in deps of project target
inDepsOfTarget :: Sourced s => Project -> Info -> s -> Bool
inDepsOfTarget p i = anyPackage $ view infoDepends i where
	anyPackage :: Sourced s => [String] -> s -> Bool
	anyPackage = fmap or . mapM (inPackage . mkPackage) . filter (/= (p ^. projectName))

-- | Check if module in deps of source
inDepsOfFile :: Sourced s => Project -> FilePath -> s -> Bool
inDepsOfFile p f m = any (\i -> inDepsOfTarget p i m) (fileTargets p f)

-- | Check if module in deps of project, ignoring self package
inDepsOfProject :: Sourced s => Project -> s -> Bool
inDepsOfProject p = anyPackage $ ordNub (p ^.. projectDescription . _Just . infos . infoDepends . each) where
	anyPackage :: Sourced s => [String] -> s -> Bool
	anyPackage = fmap or . mapM (inPackage . mkPackage) . filter (/= (p ^. projectName))

-- | Check if module in package-db
inPackageDb :: Sourced s => PackageDb -> s -> Bool
inPackageDb c m = preview (sourcedModule . moduleLocation . modulePackageDb) m == Just c

-- | Check if module in one of sandboxes
inPackageDbStack :: Sourced s => PackageDbStack -> s -> Bool
inPackageDbStack dbs m = maybe False (`elem` packageDbs dbs) $
	preview (sourcedModule . moduleLocation . modulePackageDb) m

-- | Check if module in package
inPackage :: Sourced s => ModulePackage -> s -> Bool
inPackage p m = maybe False (equalTo p) $ preview (sourcedModule . moduleLocation . modulePackage . _Just) m where
	equalTo (ModulePackage n "") r = n == view packageName r
	equalTo mp r = mp == r

-- | Check if module in file
inFile :: Sourced s => FilePath -> s -> Bool
inFile fpath m = preview (sourcedModule . moduleLocation . moduleFile) m == Just (normalise fpath)

-- | Check if module in source
inOtherLocation :: Sourced s => String -> s -> Bool
inOtherLocation src m = preview (sourcedModule . moduleLocation . otherLocationName) m == Just src

-- | Check if declaration is in module
inModule :: Sourced s => String -> s -> Bool
inModule mname m = fromString mname == view sourcedModuleName m

-- | Check if module defined in file
byFile :: Sourced s => s -> Bool
byFile = isJust . preview (sourcedModule . moduleLocation . moduleFile)

-- | Check if module got from cabal database
installed :: Sourced s => s -> Bool
installed = isJust . preview (sourcedModule . moduleLocation . cabalModuleName)

-- | Check if module is standalone
standalone :: Sourced s => s -> Bool
standalone m = preview (sourcedModule . moduleLocation . moduleProject) m == Just Nothing

latestPackages :: [ModulePackage] -> [ModulePackage]
latestPackages =
	map (maximumBy (comparing (view packageVersion))) .
	groupBy ((==) `on` view packageName) .
	sortBy (comparing (view packageName))

-- | Select symbols with last package version
newestPackage :: Sourced s => [s] -> [s]
newestPackage v = filter (maybe True (`elem` pkgs) . preview (sourcedModule . moduleLocation . modulePackage . _Just)) v where
	pkgs = latestPackages (v ^.. each . sourcedModule . moduleLocation . modulePackage . _Just)

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
