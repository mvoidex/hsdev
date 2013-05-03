module HsDev.Commands (
	goToDeclaration,
	completions
	) where

import Control.Arrow
import Control.Monad.Error
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

import HsDev.Util
import HsDev.Database
import HsDev.Symbols
import HsDev.Symbols.Util

goToDeclaration :: Database -> FilePath -> Maybe String -> String -> ErrorT String IO [Symbol Declaration]
goToDeclaration db file qualifier ident = do
	project <- liftIO $ locateProject file
	decls <- maybe (throwError $ "Declaration for " ++ ident ++ " not found") return $ M.lookup ident (databaseSymbols db)
	let
		modules = mapMaybe symbolModule $ S.toList decls
		candidates = maybe (S.toList decls) findCandidates $ M.lookup file (databaseFiles db)
		getCandidates ms = if null r then Nothing else Just r where
			r = mapMaybe (\m -> M.lookup ident (moduleDeclarations (symbol m))) ms
		findCandidates curModule = fromMaybe [] $ msum $ map getCandidates [
			if maybe True (== symbolName curModule) qualifier then [curModule] else [],
			filter (\m -> isImportedModule curModule m qualifier && sameProject curModule m) modules,
			filter (\m -> isImportedModule curModule m qualifier) modules,
			modules]
	return candidates

completions :: Database -> FilePath -> Maybe String -> String -> ErrorT String IO [String]
completions db file qualifier prefix = return result where
	qualifiedPrefix = maybe "" (++ ".") qualifier ++ prefix
	result = maybe [] completions' $ M.lookup file (databaseFiles db)
	completions' curModule = maybe useAllModules useQualifiedModule qualifier where
		useAllModules = concat [
			completionsFor curModule,
			concatMap completionsForName ("Prelude" : (map importModuleName $ filter (not . importIsQualified) $ M.elems $ moduleImports (symbol curModule)))]
		useQualifiedModule name = concatMap completionsForName (name : (map importModuleName $ filter ((== Just name) . importAs) $ M.elems $ moduleImports (symbol curModule)))
		completionsFor m = filter (prefix `isPrefixOf`) $ M.keys $ moduleDeclarations (symbol m)
		completionsForName moduleName = maybe [] completionsFor $
			visibleModule Cabal project' (maybe [] S.toList $ M.lookup moduleName (databaseModules db))
		project' = symbolLocation curModule >>= locationProject
