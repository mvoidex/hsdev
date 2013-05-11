{-# LANGUAGE RankNTypes #-}

module HsDev.Commands (
	findDeclaration, findModule,
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

findDeclaration :: Database -> String -> ErrorT String IO [Symbol Declaration]
findDeclaration db ident = return $ maybe [] S.toList $ M.lookup ident (databaseSymbols db)

findModule :: Database -> String -> ErrorT String IO [Symbol Module]
findModule db mname = return $ maybe [] S.toList $ M.lookup mname (databaseModules db)

goToDeclaration :: Database -> Maybe FilePath -> String -> ErrorT String IO [Symbol Declaration]
goToDeclaration db file ident = do
	fileName <- maybe (return "") (liftIO . canonicalizePath) file
	thisModule <- maybe (throwError $ "Can't find module at file " ++ fileName) return $ M.lookup fileName (databaseFiles db)
	decls <- findDeclaration db identName
	return $ filter (maybe False (isReachable thisModule qualifiedName) . symbolModule) decls
	where
		(qualifiedName, identName) = splitIdentifier ident

symbolInfo :: Database -> Maybe FilePath -> String -> ErrorT String IO String
symbolInfo db file ident = do
	fileName <- maybe (return "") (liftIO . canonicalizePath) file
	project <- maybe (return Nothing) (const $ liftIO $ locateProject fileName) file
	decls <- findDeclaration db identName
	let
		filterFunction qname = maybe (\m -> maybe True (== symbolName m) qname) (\cur -> isReachable cur qname) $ M.lookup fileName (databaseFiles db)
		resultDecls = getGroup [inProject_ project, notPrelude] $ filter (maybe False (filterFunction qualifiedName) . symbolModule) decls
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

groupize :: [a -> Bool] -> [a] -> [Maybe [a]]
groupize ps ls = map (\p -> nonull (filter p ls)) ps where
	nonull [] = Nothing
	nonull x = Just x

getGroup :: [a -> Bool] -> [a] -> [a]
getGroup ps ls = fromMaybe [] $ msum $ groupize ps ls
