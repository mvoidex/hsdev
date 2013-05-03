module HsDev.Symbols.Util (
	) where

import Data.Maybe
import qualified Data.Map as M

import HsDev.Symbols

withinProject :: String -> Symbol a -> Bool
withinProject project s = (Just project) == project' where
	project' = do
		loc <- symbolLocation s
		undefined
		--locationProject loc

withinCabal :: Cabal -> Symbol Module -> Bool
withinCabal cabal m = moduleCabal (symbol m) == Just cabal

bySources :: Symbol Module -> Bool
bySources = isJust . symbolLocation

sourceModules :: Maybe String -> [Symbol Module] -> Symbol Module
sourceModules project ms = head $ inProject ++ filter bySources ms where
	inProject = maybe [] (\p -> filter (withinProject p) ms) project

visibleModules :: Cabal -> Maybe String -> [Symbol Module] -> Symbol Module
visibleModules cabal project ms = head $ inProject ++ filter (withinCabal cabal) ms where
	inProject = maybe [] (\p -> filter (withinProject p) ms) project

preferredModules :: Cabal -> Maybe String -> [Symbol Module] -> Symbol Module
preferredModules cabal project ms = head $ concat [
	maybe [] (\p -> filter (withinProject p) ms) project,
	filter (withinCabal cabal) ms,
	filter bySources ms,
	ms]

isImportedModule :: Symbol Module -> Symbol Module -> Maybe String -> Bool
isImportedModule inModule m q = maybe unqualified qualified' q where
	unqualified = maybe (symbolName m == "Prelude") (not . importIsQualified) $ M.lookup (symbolName m) (moduleImports (symbol m))
	qualified' qname = maybe False (\i -> qname == importModuleName i || Just qname == importAs i) $ M.lookup (symbolName m) (moduleImports $ symbol m)
