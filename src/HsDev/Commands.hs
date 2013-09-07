{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module HsDev.Commands (
	-- * Commands
	findDeclaration, findModule,
	fileModule,
	goToDeclaration,
	lookupSymbol,
	symbolInfo,
	completions,
	moduleCompletions,

	-- * Filters
	checkModule, checkDeclaration, visibleFrom, reachableFrom
	) where

import Control.Arrow
import Control.Monad
import Control.Monad.Error
import Data.Either (rights)
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Traversable (traverse)
import System.Directory

import HsDev.Database
import HsDev.Project (project)
import HsDev.Symbols
import HsDev.Symbols.Util

-- | Find declaration by name
findDeclaration :: Database -> String -> ErrorT String IO [ModuleDeclaration]
findDeclaration db ident = return $ selectDeclarations ((== ident) . declarationName . moduleDeclaration) db

-- | Find module by name
findModule :: Database -> String -> ErrorT String IO [Module]
findModule db mname = return $ selectModules ((== mname) . moduleName) db

-- | Find module in file
fileModule :: Database -> FilePath -> ErrorT String IO Module
fileModule db src = do
	src' <- liftIO $ canonicalizePath src
	maybe (throwError $ "File '" ++ src' ++ "' not found") return $ lookupFile src' db

-- | Go to declaration
goToDeclaration :: Database -> Maybe FilePath -> String -> ErrorT String IO [ModuleDeclaration]
goToDeclaration db ctx ident = do
	mthis <- (traverse (liftIO . canonicalizePath) >=> traverse (fileModule db)) ctx
	decls <- findDeclaration db iname
	return $ flip filter decls $ checkModule $ allOf [
		maybe (const True) visibleFrom mthis,
		maybe (const True) (reachableFrom qname) mthis,
		byFile]
	where
		(qname, iname) = splitIdentifier ident

-- | Lookup symbol
lookupSymbol :: Database -> FilePath -> String -> ErrorT String IO (Either [ModuleDeclaration] ModuleDeclaration)
lookupSymbol db file ident = do
	mthis <- ((liftIO . canonicalizePath) >=> fileModule db) file
	decls <- findDeclaration db iname
	let
		visible = flip filter decls $ checkModule (visibleFrom mthis)
		reached = flip filter visible $ checkModule (reachableFrom qname mthis)
	case reached of
		[] -> return $ Left visible
		[r] -> return $ Right r
		rs -> throwError $ "Ambiguous symbol: " ++ intercalate ", " (map (brief . declarationModuleId) rs)
	where
		(qname, iname) = splitIdentifier ident

-- | Find symbol info
symbolInfo :: Database -> Maybe FilePath -> String -> ErrorT String IO ModuleDeclaration
symbolInfo db ctx ident = do
	mthis <- (traverse (liftIO . canonicalizePath) >=> traverse (fileModule db)) ctx
	decls <- findDeclaration db iname
	let
		cands = flip filter decls $ checkModule $ allOf [
			maybe (const True) visibleFrom mthis,
			maybe (const True) (reachableFrom qname) mthis,
			not . inModule "Prelude"]
	case cands of
		[] -> throwError $ "Symbol '" ++ ident ++ "' not found"
		[c] -> return c
		cs -> throwError $ "Ambiguous symbols: " ++ intercalate ", " (map (brief . declarationModuleId) cs)
	where
		(qname, iname) = splitIdentifier ident

-- | Completions
completions :: Database -> Module -> String -> ErrorT String IO [ModuleDeclaration]
completions db m prefix = return prefixed where
	modules = flip filter (rights $ map inspectionResult $ M.elems $ databaseModules db) $ (allOf [visibleFrom m, reachableFrom qname m] . moduleId)
	moduleScope = scope m

	scope :: Module -> Map (Maybe String) [Module]
	scope m' = M.unionWith (++) (decls [Nothing, Just (moduleName m')] m') (imports m')

	decls :: [Maybe String] -> Module -> Map (Maybe String) [Module]
	decls qs m' = M.unionsWith (++) $ map (\q -> M.singleton q [m']) qs

	imports :: Module -> Map (Maybe String) [Module]
	imports m' = M.unionsWith (++) $ map (importScope m') $
		(if byFile (moduleId m') then (Import "Prelude" False Nothing Nothing :) else id) $
			(M.elems (moduleImports m))

	importScope :: Module -> Import -> Map (Maybe String) [Module]
	importScope _ i = fromMaybe M.empty $ do
		ms <- return $ selectModules ((== importModuleName i) . moduleName) db
		imp' <- listToMaybe $ ms `intersect` modules
		return $ decls (catMaybes [
			Just (Just (importModuleName i)),
			if not (importIsQualified i) then Just Nothing else Nothing,
			fmap Just (importAs i)]) imp'

	completes = maybe [] (concatMap moduleModuleDeclarations) $ M.lookup qname moduleScope
	prefixed = filter ((iname `isPrefixOf`) . declarationName . moduleDeclaration) completes

	(qname, iname) = splitIdentifier prefix

-- | Module completions
moduleCompletions :: Database -> [Module] -> String -> ErrorT String IO [String]
moduleCompletions _ ms prefix = return $ nub $ completions' $ map moduleName ms where
	completions' = mapMaybe getNext where
		getNext m
			| prefix `isPrefixOf` m = listToMaybe $ map snd $ dropWhile (uncurry (==)) $ zip (splitBy '.' prefix) (splitBy '.' m)
			| otherwise = Nothing

-- | Check module
checkModule :: (ModuleId -> Bool) -> (ModuleDeclaration -> Bool)
checkModule = (. declarationModuleId)

-- | Check declaration
checkDeclaration :: (Declaration -> Bool) -> (ModuleDeclaration -> Bool)
checkDeclaration = (. moduleDeclaration)

-- | Check whether declaration is visible from source file
visibleFrom :: Module -> (ModuleId -> Bool)
visibleFrom m = case moduleLocation m of
	FileModule src proj -> anyOf [inCabal Cabal, maybe (const True) (inProject . project) proj, inFile src]
	CabalModule cabal package _ -> allOf [inCabal cabal, maybe (const False) inPackage package]
	MemoryModule m' -> inMemory m'

-- | Check whether declaration is reachable from source file
reachableFrom :: Maybe String -> Module -> (ModuleId -> Bool)
reachableFrom q m = case moduleLocation m of
	FileModule _ _ -> reachable q m
	CabalModule cabal _ _ -> inCabal cabal
	MemoryModule mem -> inMemory mem

splitBy :: Char -> String -> [String]
splitBy ch = takeWhile (not . null) . unfoldr (Just . second (drop 1) . break (== ch))

splitIdentifier :: String -> (Maybe String, String)
splitIdentifier name = (qname, name') where
	prefix = dropWhileEnd (/= '.') name
	prefix' = dropWhileEnd (== '.') prefix
	qname = if null prefix' then Nothing else Just prefix'
	name' = fromMaybe (error "Impossible happened") $ stripPrefix prefix name
