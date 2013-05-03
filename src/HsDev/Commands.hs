module HsDev.Commands (
	findSymbol,
	goToDeclaration,
	symbolInfo,
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

findSymbol :: Database -> String -> ErrorT String IO ([Symbol Declaration], [Symbol Module])
findSymbol db ident = return (candidates, moduleCandidates) where
	candidates = maybe [] S.toList $ M.lookup identName (databaseSymbols db)
	moduleCandidates = maybe [] S.toList $ M.lookup ident (databaseModules db)
	(_, identName) = splitIdentifier ident

goToDeclaration :: Database -> Maybe FilePath -> String -> ErrorT String IO ([Symbol Declaration], [Symbol Module])
goToDeclaration db file ident = do
	fileName <- maybe (return "") (liftIO . canonicalizePath) file
	(decls, modules) <- findSymbol db ident
	return (filter (maybe False (reachable qualifiedName fileName) . symbolModule) decls, filter (reachable (Just ident) fileName) modules)
	where
		reachable qnm f m = bySources m && maybe (maybe True (== symbolName m) qnm) (\cur -> isReachable cur qnm m) thisModule where
			thisModule = M.lookup f (databaseFiles db)
		(qualifiedName, identName) = splitIdentifier ident

symbolInfo :: Database -> Maybe FilePath -> String -> ErrorT String IO String
symbolInfo db file ident = do
	fileName <- maybe (return "") (liftIO . canonicalizePath) file
	project <- maybe (return Nothing) (const $ liftIO $ locateProject fileName) file
	(decls, _) <- findSymbol db ident
	let
		filterFunction qname = maybe (\m -> maybe True (== symbolName m) qname) (\cur -> isReachable cur qname) $ M.lookup fileName (databaseFiles db)
		decls' = groupize [inProject_ project, notPrelude] $ filter (maybe False (filterFunction qualifiedName) . symbolModule) decls
		resultDecls = fromMaybe [] $ listToMaybe $ dropWhile null decls'
		notPrelude m = maybe True ((/= "Prelude") . symbolName) $ symbolModule m
	case length resultDecls of
		0 -> throwError $ "Symbol '" ++ ident ++ "' not found"
		1 -> return $ detailed (head resultDecls)
		_ -> throwError $ "Ambiguous symbols: " ++ intercalate ", " (map put resultDecls)
	where
		(qualifiedName, identName) = splitIdentifier ident
		put s = maybe "" ((++ ".") . symbolName) (symbolModule s) ++ symbolName s

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

groupize :: [a -> Bool] -> [a] -> [[a]]
groupize [] l = [l]
groupize (p:ps) l = p' : groupize ps tl where
	(p', tl) = partition p l
