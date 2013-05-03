module HsDev.Commands (
	findSymbol,
	goToDeclaration,
	completions
	) where

import Control.Arrow
import Control.Monad
import Control.Monad.Error
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import System.Directory

import HsDev.Util
import HsDev.Database
import HsDev.Symbols
import HsDev.Symbols.Util

findSymbol :: Database -> FilePath -> String -> ErrorT String IO ([Symbol Declaration], [Symbol Module])
findSymbol db file ident = fmap go $ liftIO (canonicalizePath file) where
	go fileName = (candidates, moduleCandidates) where
		candidates = maybe [] S.toList $ M.lookup identName (databaseSymbols db)
		moduleCandidates = maybe [] S.toList $ M.lookup ident (databaseModules db)
	(_, identName) = splitIdentifier ident

goToDeclaration :: Database -> FilePath -> String -> ErrorT String IO ([Symbol Declaration], [Symbol Module])
goToDeclaration db file ident = do
	fileName <- liftIO $ canonicalizePath file
	(decls, modules) <- findSymbol db fileName ident
	return (filter (maybe False (reachable qualifiedName fileName) . symbolModule) decls, filter (reachable (Just ident) fileName) modules)
	where
		reachable qnm f m = bySources m && maybe True reachableFrom (M.lookup f (databaseFiles db)) where
			reachableFrom cur = isImported cur qnm m || reachableFromSelf where
				reachableFromSelf = cur == m && (qnm == Nothing || qnm == Just (symbolName cur))
		(qualifiedName, identName) = splitIdentifier ident

completions :: Database -> FilePath -> String -> ErrorT String IO [String]
completions db file prefix = fmap nub $ do
	file' <- liftIO $ canonicalizePath file
	project <- liftIO $ locateProject file'
	return (maybe [] moduleCompletions project ++ result file')
	where
		result f = maybe [] completions' $ M.lookup f (databaseFiles db)
		completions' curModule = maybe useAllModules useQualifiedModule qualifiedName where
			useAllModules = concat [
				completionsFor curModule,
				concatMap completionsForName ("Prelude" : (map importModuleName $ filter (not . importIsQualified) $ M.elems $ moduleImports (symbol curModule))),
				moduleCompletionsFor prefix $ map importModuleName $ M.elems $ moduleImports $ symbol curModule]
			useQualifiedModule name = concatMap completionsForName (name : (map importModuleName $ filter ((== Just name) . importAs) $ M.elems $ moduleImports (symbol curModule)))
			completionsFor m = filter (identName `isPrefixOf`) $ M.keys $ moduleDeclarations (symbol m)
			completionsForName moduleName = maybe [] completionsFor $
				visibleModule Cabal project' (maybe [] S.toList $ M.lookup moduleName (databaseModules db))
			project' = symbolLocation curModule >>= locationProject
		moduleCompletionsFor pref ms = mapMaybe getNext ms where
			getNext m
				| pref `isPrefixOf` m = listToMaybe $ map snd $ dropWhile (uncurry (==)) $ zip (splitBy '.' pref) (splitBy '.' m)
				| otherwise = Nothing
		moduleCompletions proj = moduleCompletionsFor prefix visibleModules where
			visibleModules = map symbolName $ M.elems (cabalModules Cabal db) ++ M.elems (projectModules proj db)
		(qualifiedName, identName) = splitIdentifier prefix

splitBy :: Char -> String -> [String]
splitBy ch = takeWhile (not . null) . unfoldr (Just . second (drop 1) . break (== ch))

splitIdentifier :: String -> (Maybe String, String)
splitIdentifier name = (qname, name') where
	prefix = dropWhileEnd (/= '.') name
	prefix' = dropWhileEnd (== '.') prefix
	qname = if null prefix' then Nothing else Just prefix'
	name' = fromMaybe (error "Impossible happened") $ stripPrefix prefix name
