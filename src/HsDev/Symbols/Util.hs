module HsDev.Symbols.Util (
	projectOf, inProject, inCabal, inPackage, inFile, inMemory, inModule, byFile, byCabal, standalone,
	imports, qualifier, imported, visible, inScope,
	sourceModule, visibleModule, preferredModule, uniqueModules,
	allOf, anyOf
	) where

import Data.Function (on)
import Data.Maybe
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import System.FilePath (normalise)

import HsDev.Symbols
import HsDev.Project

-- | Get module project
projectOf :: ModuleId -> Maybe Project
projectOf m = case moduleIdLocation m of
	FileModule _ proj -> proj
	_ -> Nothing

-- | Check if module in project
inProject :: Project -> ModuleId -> Bool
inProject p m = projectOf m == Just p

-- | Check if module in cabal
inCabal :: Cabal -> ModuleId -> Bool
inCabal c m = case moduleIdLocation m of
	CabalModule cabal _ _ -> cabal == c
	_ -> False

-- | Check if module in package
inPackage :: String -> ModuleId -> Bool
inPackage p m = case moduleIdLocation m of
	CabalModule _ package _ -> Just p == fmap packageName package
	_ -> False

-- | Check if module in file
inFile :: FilePath -> ModuleId -> Bool
inFile fpath m = case moduleIdLocation m of
	FileModule f _ -> f == normalise fpath
	_ -> False

-- | Check if module in memory
inMemory :: Maybe String -> ModuleId -> Bool
inMemory mem m = case moduleIdLocation m of
	MemoryModule mem' -> mem' == mem
	_ -> False

-- | Check if declaration is in module
inModule :: String -> ModuleId -> Bool
inModule mname m = mname == moduleIdName m

-- | Check if module defined in file
byFile :: ModuleId -> Bool
byFile m = case moduleIdLocation m of
	FileModule _ _ -> True
	_ -> False

-- | Check if module got from cabal database
byCabal :: ModuleId -> Bool
byCabal m = case moduleIdLocation m of
	CabalModule _ _ _ -> True
	_ -> False

-- | Check if module is standalone
standalone :: ModuleId -> Bool
standalone m = case moduleIdLocation m of
	FileModule _ Nothing -> True
	_ -> False

-- | Get list of imports
imports :: Module -> [Import]
imports = moduleImports

-- | Get list of imports, which can be accessed with specified qualifier or unqualified
qualifier :: Module -> Maybe String -> [Import]
qualifier m q = filter (importQualifier q) $ (Import "Prelude" False Nothing Nothing : Import (moduleName m) False Nothing Nothing : imports m)

-- | Check if module imported via imports specified
imported :: ModuleId -> [Import] -> Bool
imported m = any (\i -> moduleIdName m == importModuleName i)

-- | Check if module visible from this module within this project
visible :: Project -> ModuleId -> ModuleId -> Bool
visible p (ModuleId _ (FileModule src _)) m =
	inProject p m || any (`inPackage` m) deps || maybe False ((`elem` deps) . projectName) (projectOf m)
	where
		deps = maybe [] infoDepends $ fileTarget p src
visible _ _ _ = False

-- | Check if module is in scope with qualifier
inScope :: Module -> Maybe String -> ModuleId -> Bool
inScope this q m = m `imported` qualifier this q

-- | Select module, defined by sources
sourceModule :: Maybe Project -> [Module] -> Maybe Module
sourceModule proj ms = listToMaybe $ maybe (const []) (filter . (. moduleId) . inProject) proj ms ++ filter (byFile . moduleId) ms

-- | Select module, visible in project or cabal
visibleModule :: Cabal -> Maybe Project -> [Module] -> Maybe Module
visibleModule cabal proj ms = listToMaybe $ maybe (const []) (filter . (. moduleId) . inProject) proj ms ++ filter (inCabal cabal . moduleId) ms

-- | Select preferred visible module
preferredModule :: Cabal -> Maybe Project -> [ModuleId] -> Maybe ModuleId
preferredModule cabal proj ms = listToMaybe $ concatMap (`filter` ms) order where
	order = [
		maybe (const False) inProject proj,
		byFile,
		inCabal cabal,
		const True]

-- | Remove duplicate modules, leave only `preferredModule`
uniqueModules :: Cabal -> Maybe Project -> [ModuleId] -> [ModuleId]
uniqueModules cabal proj =
	catMaybes .
	map (preferredModule cabal proj) .
	groupBy ((==) `on` moduleIdName) .
	sortBy (comparing moduleIdName)

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
