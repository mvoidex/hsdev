module HsDev.Symbols.Util (
	inProject, inCabal, inPackage, inFile, inMemory, inModule, byFile, standalone,
	qualifier, imported, reachable,
	sourceModule, visibleModule, preferredModule,
	allOf, anyOf
	) where

import Data.Maybe
import qualified Data.Map as M
import System.FilePath

import HsDev.Symbols
import HsDev.Project

-- | Check if module in project
inProject :: Project -> Module -> Bool
inProject p m = case moduleLocation m of
	FileModule _ proj -> Just (projectCabal p) == proj
	_ -> False
	where

-- | Check if module in cabal
inCabal :: Cabal -> Module -> Bool
inCabal c m = case moduleLocation m of
	CabalModule cabal _ _ -> cabal == c
	_ -> False

-- | Check if module in package
inPackage :: String -> Module -> Bool
inPackage p m = case moduleLocation m of
	CabalModule _ package _ -> Just p == package
	_ -> False

-- | Check if module in file
inFile :: FilePath -> Module -> Bool
inFile fpath m = case moduleLocation m of
	FileModule f _ -> f == normalise fpath
	_ -> False

-- | Check if module in memory
inMemory :: Maybe String -> Module -> Bool
inMemory mem m = case moduleLocation m of
	MemoryModule mem' -> mem' == mem
	_ -> False

-- | Check if declaration is in module
inModule :: String -> Module -> Bool
inModule mname m = mname == moduleName m

-- | Check if module defined in file
byFile :: Module -> Bool
byFile m = case moduleLocation m of
	FileModule _ _ -> True
	_ -> False

-- | Check if module is standalone
standalone :: Module -> Bool
standalone m = case moduleLocation m of
	FileModule _ Nothing -> True
	_ -> False

-- | Get list of imports accessible via qualifier
qualifier :: Module -> Maybe String -> [Import]
qualifier m q = filter (importQualifier q) $ (Import "Prelude" False Nothing Nothing : Import (moduleName m) False Nothing Nothing : M.elems (moduleImports m))

-- | Check if module imported via import statement
imported :: Module -> Import -> Bool
imported m i = moduleName m == importModuleName i

-- | Check if module reachable from this module via qualifier
-- >reachable m q this -- module `m` reachable from `this`
reachable :: Maybe String -> Module -> Module -> Bool
reachable q this m = any (imported m) $ qualifier this q

-- | Select module, defined by sources
sourceModule :: Maybe Project -> [Module] -> Maybe Module
sourceModule proj ms = listToMaybe $ maybe (const []) (filter . inProject) proj ms ++ filter byFile ms

-- | Select module, visible in project or cabal
visibleModule :: Cabal -> Maybe Project -> [Module] -> Maybe Module
visibleModule cabal proj ms = listToMaybe $ maybe (const []) (filter . inProject) proj ms ++ filter (inCabal cabal) ms

-- | Select preferred visible module
preferredModule :: Cabal -> Maybe Project -> [Module] -> Maybe Module
preferredModule cabal proj ms = listToMaybe $ concatMap (`filter` ms) order where
	order = [
		maybe (const False) inProject proj,
		inCabal cabal,
		byFile,
		const True]

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
