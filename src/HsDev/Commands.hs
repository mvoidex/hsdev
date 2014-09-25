{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module HsDev.Commands (
	-- * Commands
	findDeclaration, findModule,
	fileModule,
	lookupSymbol,
	whois,
	scopeModules, scope,
	completions,
	moduleCompletions,

	-- * Filters
	checkModule, checkDeclaration, restrictCabal, visibleFrom,
	splitIdentifier,

	-- * Helpers
	fileCtx, fileCtxMaybe
	) where

import Control.Applicative
import Control.Arrow (Arrow(second))
import Control.Monad.Error
import Data.List
import Data.Maybe
import qualified Data.Map as M (lookup)
import Data.Traversable (traverse)
import System.Directory (canonicalizePath)

import HsDev.Database
import HsDev.Project
import HsDev.Symbols
import HsDev.Symbols.Util

-- | Find declaration by name
findDeclaration :: Database -> String -> ErrorT String IO [ModuleDeclaration]
findDeclaration db ident = return $ selectDeclarations checkName db where
	checkName :: ModuleDeclaration -> Bool
	checkName m =
		(declarationName (moduleDeclaration m) == iname) &&
		(maybe True (moduleIdName (declarationModuleId m) ==) qname)

	(qname, iname) = splitIdentifier ident

-- | Find module by name
findModule :: Database -> String -> ErrorT String IO [Module]
findModule db mname = return $ selectModules ((== mname) . moduleName) db

-- | Find module in file
fileModule :: Database -> FilePath -> ErrorT String IO Module
fileModule db src = do
	src' <- liftIO $ canonicalizePath src
	maybe (throwError $ "File '" ++ src' ++ "' not found") return $ lookupFile src' db

-- | Find project of module
getProject :: Database -> Project -> ErrorT String IO Project
getProject db p = do
	p' <- liftIO $ canonicalizePath $ projectCabal p
	maybe (throwError $ "Project " ++ p' ++ " not found") return $
		M.lookup p' $ databaseProjects db

-- | Lookup visible symbol
lookupSymbol :: Database -> Cabal -> FilePath -> String -> ErrorT String IO [ModuleDeclaration]
lookupSymbol db cabal file ident = do
	(_, mthis, mproj) <- fileCtx db file
	liftM
		(filter $ checkModule $ allOf [
			restrictCabal cabal,
			visibleFrom mproj mthis,
			maybe (const True) inModule qname])
		(newestPackage <$> findDeclaration db iname)
	where
		(qname, iname) = splitIdentifier ident

-- | Whois symbol in scope
whois :: Database -> Cabal -> FilePath -> String -> ErrorT String IO [ModuleDeclaration]
whois db cabal file ident = do
	(_, mthis, _) <- fileCtx db file
	liftM
		(filter $ checkModule $ allOf [
			restrictCabal cabal,
			inScope mthis qname])
		(newestPackage <$> findDeclaration db iname)
	where
		(qname, iname) = splitIdentifier ident

-- | Accessible modules
scopeModules :: Database -> Cabal -> FilePath -> ErrorT String IO [Module]
scopeModules db cabal file = do
	(file', mthis, mproj) <- fileCtxMaybe db file
	newestPackage <$> case mproj of
		Nothing -> return $ maybe id (:) mthis $ selectModules (inCabal cabal . moduleId) db
		Just proj -> let deps' = deps file' proj in
			return $ concatMap (\p -> selectModules (p . moduleId) db) [
				inProject proj,
				\m -> any (`inPackage` m) deps']
	where
		deps f p = maybe [] infoDepends $ fileTarget p f

-- | Symbols in scope
scope :: Database -> Cabal -> FilePath -> Bool -> ErrorT String IO [ModuleDeclaration]
scope db cabal file False = do
	(_, mthis, _) <- fileCtx db file
	depModules <- liftM (filter ((`imported` (moduleImports' mthis)) . moduleId)) $
		scopeModules db cabal file
	return $ concatMap moduleModuleDeclarations $ mthis : depModules
scope db cabal file True = concatMap moduleModuleDeclarations <$> scopeModules db cabal file

-- | Completions
completions :: Database -> Cabal -> FilePath -> String -> ErrorT String IO [ModuleDeclaration]
completions db cabal file prefix = do
	(_, mthis, _) <- fileCtx db file
	decls <- scope db cabal file False
	return [decl |
		decl <- decls,
		imp <- filter ((== moduleIdName (declarationModuleId decl)) . importModuleName) $
			moduleImports' mthis,
		qname `elem` catMaybes [
			if not (importIsQualified imp) then Just Nothing else Nothing,
			Just $ Just $ importModuleName imp,
			fmap Just $ importAs imp],
		iname `isPrefixOf` (declarationName . moduleDeclaration $ decl)]
	where
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

-- | Allow only selected cabal sandbox
restrictCabal :: Cabal -> ModuleId -> Bool
restrictCabal cabal m = inCabal cabal m || not (byCabal m)

-- | Check whether module is visible from source file
visibleFrom :: Maybe Project -> Module -> ModuleId -> Bool
visibleFrom (Just p) this m = visible p (moduleId this) m
visibleFrom Nothing this m = (moduleId this) == m || byCabal m

splitBy :: Char -> String -> [String]
splitBy ch = takeWhile (not . null) . unfoldr (Just . second (drop 1) . break (== ch))

-- | Get module imports with Prelude and self import
moduleImports' :: Module -> [Import]
moduleImports' m = Import "Prelude" False Nothing Nothing : Import (moduleName m) False Nothing Nothing : moduleImports m

-- | Split identifier into module name and identifier itself
splitIdentifier :: String -> (Maybe String, String)
splitIdentifier name = (qname, name') where
	prefix = dropWhileEnd (/= '.') name
	prefix' = dropWhileEnd (== '.') prefix
	qname = if null prefix' then Nothing else Just prefix'
	name' = fromMaybe (error "Impossible happened") $ stripPrefix prefix name

-- | Get context file and project
fileCtx :: Database -> FilePath -> ErrorT String IO (FilePath, Module, Maybe Project)
fileCtx db file = do
	file' <- liftIO $ canonicalizePath file
	mthis <- fileModule db file'
	mproj <- traverse (getProject db) $ projectOf $ moduleId mthis
	return (file', mthis, mproj)

-- | Try get context file
fileCtxMaybe :: Database -> FilePath -> ErrorT String IO (FilePath, Maybe Module, Maybe Project)
fileCtxMaybe db file = ((\(f, m, p) -> (f, Just m, p)) <$> fileCtx db file) <|> onlyProj where
	onlyProj = do
		file' <- liftIO $ canonicalizePath file
		mproj <- liftIO $ locateProject file'
		mproj' <- traverse (getProject db) mproj
		return (file', Nothing, mproj')
