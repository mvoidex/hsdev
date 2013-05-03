module HsDev.Symbols.Util (
	sameProject,
	withinProject,
	withinCabal,
	bySources,
	sourceModule,
	visibleModule,
	preferredModule,
	isImportedModule
	) where

import Data.Maybe
import qualified Data.Map as M

import HsDev.Symbols
import HsDev.Project

sameProject :: Symbol a -> Symbol b -> Bool
sameProject l r = project' l == project' r where
	project' s = do
		loc <- symbolLocation s
		locationProject loc

withinProject :: Project -> Symbol a -> Bool
withinProject project s = (Just project) == project' where
	project' = do
		loc <- symbolLocation s
		locationProject loc

withinCabal :: Cabal -> Symbol Module -> Bool
withinCabal cabal m = moduleCabal (symbol m) == Just cabal

bySources :: Symbol a -> Bool
bySources = isJust . symbolLocation

sourceModule :: Maybe Project -> [Symbol Module] -> Maybe (Symbol Module)
sourceModule project ms = listToMaybe $ inProject ++ filter bySources ms where
	inProject = maybe [] (\p -> filter (withinProject p) ms) project

visibleModule :: Cabal -> Maybe Project -> [Symbol Module] -> Maybe (Symbol Module)
visibleModule cabal project ms = listToMaybe $ inProject ++ filter (withinCabal cabal) ms where
	inProject = maybe [] (\p -> filter (withinProject p) ms) project

preferredModule :: Cabal -> Maybe Project -> [Symbol Module] -> Maybe (Symbol Module)
preferredModule cabal project ms = listToMaybe $ concat [
	maybe [] (\p -> filter (withinProject p) ms) project,
	filter (withinCabal cabal) ms,
	filter bySources ms,
	ms]

isImportedModule :: Symbol Module -> Symbol Module -> Maybe String -> Bool
isImportedModule inModule m q = maybe unqualified qualified' q where
	unqualified = maybe (symbolName m == "Prelude") (not . importIsQualified) $ M.lookup (symbolName m) (moduleImports (symbol m))
	qualified' qname = maybe False (\i -> qname == importModuleName i || Just qname == importAs i) $ M.lookup (symbolName m) (moduleImports $ symbol m)
