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
import Control.Monad.Error
import Data.List
import Data.Maybe
import qualified Data.Map as M (lookup)
import Data.String (fromString)
import qualified Data.Text as T (isPrefixOf, split, unpack)
import Data.Traversable (traverse)
import System.Directory (canonicalizePath)

import HsDev.Database
import HsDev.Project
import HsDev.Symbols
import HsDev.Symbols.Resolve
import HsDev.Symbols.Util

-- | Find declaration by name
findDeclaration :: Database -> String -> ErrorT String IO [ModuleDeclaration]
findDeclaration db ident = return $ selectDeclarations checkName db where
	checkName :: ModuleDeclaration -> Bool
	checkName m =
		(declarationName (moduleDeclaration m) == fromString iname) &&
		(maybe True ((moduleIdName (declarationModuleId m) ==) . fromString) qname)

	(qname, iname) = splitIdentifier ident

-- | Find module by name
findModule :: Database -> String -> ErrorT String IO [Module]
findModule db mname = return $ selectModules ((== fromString mname) . moduleName) db

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

-- | Lookup visible within project symbol
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
	(_, mthis, mproj) <- fileCtx db file
	return $
		newestPackage $ filter (checkDecl . moduleDeclaration) $
		moduleModuleDeclarations $ scopeModule $
		resolveOne (fileDeps file cabal mproj db) mthis
	where
		(qname, iname) = splitIdentifier ident
		checkDecl d = fmap fromString qname `elem` scopes d && declarationName d == fromString iname

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
	(_, mthis, mproj) <- fileCtx db file
	return $ moduleModuleDeclarations $ scopeModule $ resolveOne (fileDeps file cabal mproj db) mthis
scope db cabal file True = concatMap moduleModuleDeclarations <$> scopeModules db cabal file

-- | Completions
completions :: Database -> Cabal -> FilePath -> String -> ErrorT String IO [ModuleDeclaration]
completions db cabal file prefix = do
	(_, mthis, _) <- fileCtx db file
	decls <- scope db cabal file False
	return [decl' |
		decl' <- decls,
		imp <- filter ((== moduleIdName (declarationModuleId decl')) . importModuleName) $
			moduleImports' mthis,
		fmap fromString qname `elem` catMaybes [
			if not (importIsQualified imp) then Just Nothing else Nothing,
			Just $ Just $ importModuleName imp,
			fmap Just $ importAs imp],
		fromString iname `T.isPrefixOf` (declarationName . moduleDeclaration $ decl')]
	where
		(qname, iname) = splitIdentifier prefix

-- | Module completions
moduleCompletions :: Database -> [Module] -> String -> ErrorT String IO [String]
moduleCompletions _ ms prefix = return $ map T.unpack $ nub $ completions' $ map moduleName ms where
	completions' = mapMaybe getNext where
		getNext m
			| fromString prefix `T.isPrefixOf` m = listToMaybe $ map snd $ dropWhile (uncurry (==)) $ zip (T.split (== '.') $ fromString prefix) (T.split (== '.') m)
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

-- | Get module imports with Prelude and self import
moduleImports' :: Module -> [Import]
moduleImports' m =
	import_ (fromString "Prelude") :
	import_ (moduleName m) :
	moduleImports m

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

-- | Restrict only modules file depends on
fileDeps :: FilePath -> Cabal -> Maybe Project -> Database -> Database
fileDeps file cabal mproj = filterDB fileDeps' (const True) where
	fileDeps' = and . sequence [
		restrictCabal cabal,
		maybe (const True) inProject mproj,
		maybe (const True) inDepsOf (join $ fileTarget <$> mproj <*> pure file)]
