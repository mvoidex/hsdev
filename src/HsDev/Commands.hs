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
	fileCtx, fileCtxMaybe,

	-- * Reexports
	module HsDev.Database,
	module HsDev.Symbols.Types,
	module Control.Monad.Except
	) where

import Control.Applicative
import Control.Lens (view, set, each)
import Control.Monad.Except
import Data.Maybe
import qualified Data.Map as M (lookup)
import Data.String (fromString)
import qualified Data.Text as T (isPrefixOf, split, unpack)
import System.Directory (canonicalizePath)

import HsDev.Database
import HsDev.Project
import HsDev.Symbols
import HsDev.Symbols.Resolve
import HsDev.Symbols.Types
import HsDev.Symbols.Util
import HsDev.Tools.Base (matchRx, at_)
import HsDev.Util (liftE, ordNub)

-- | Find declaration by name
findDeclaration :: Database -> String -> ExceptT String IO [ModuleDeclaration]
findDeclaration db ident = return $ selectDeclarations checkName db where
	checkName :: ModuleDeclaration -> Bool
	checkName m =
		(view (moduleDeclaration . declarationName) m == fromString iname) &&
		(maybe True ((view (declarationModuleId . moduleIdName) m ==) . fromString) qname)

	(qname, iname) = splitIdentifier ident

-- | Find module by name
findModule :: Database -> String -> ExceptT String IO [Module]
findModule db mname = return $ selectModules ((== fromString mname) . view moduleName) db

-- | Find module in file
fileModule :: Database -> FilePath -> ExceptT String IO Module
fileModule db src = do
	src' <- liftE $ canonicalizePath src
	maybe (throwError $ "File '" ++ src' ++ "' not found") return $ lookupFile src' db

-- | Find project of module
getProject :: Database -> Project -> ExceptT String IO Project
getProject db p = do
	p' <- liftE $ canonicalize p
	maybe (throwError $ "Project " ++ view projectCabal p' ++ " not found") return $
		refineProject db p'

-- | Lookup visible within project symbol
lookupSymbol :: Database -> Cabal -> FilePath -> String -> ExceptT String IO [ModuleDeclaration]
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
whois :: Database -> Cabal -> FilePath -> String -> ExceptT String IO [ModuleDeclaration]
whois db cabal file ident = do
	(_, mthis, mproj) <- fileCtx db file
	return $
		newestPackage $ filter (checkDecl . view moduleDeclaration) $
		moduleModuleDeclarations $ scopeModule $
		resolveOne (fileDeps file cabal mproj db) $
		moduleLocals mthis
	where
		(qname, iname) = splitIdentifier ident
		checkDecl d = fmap fromString qname `elem` scopes d && view declarationName d == fromString iname

-- | Accessible modules
scopeModules :: Database -> Cabal -> FilePath -> ExceptT String IO [Module]
scopeModules db cabal file = do
	(file', mthis, mproj) <- fileCtxMaybe db file
	newestPackage <$> case mproj of
		Nothing -> return $ maybe id (:) mthis $ selectModules (inCabal cabal . view moduleId) db
		Just proj -> let deps' = deps file' proj in
			return $ concatMap (\p -> selectModules (p . view moduleId) db) [
				inProject proj,
				\m -> any (`inPackage` m) deps']
	where
		deps f p = concatMap (view infoDepends) $ fileTargets p f

-- | Symbols in scope
scope :: Database -> Cabal -> FilePath -> Bool -> ExceptT String IO [ModuleDeclaration]
scope db cabal file False = do
	(_, mthis, mproj) <- fileCtx db file
	return $ moduleModuleDeclarations $ scopeModule $ resolveOne (fileDeps file cabal mproj db) mthis
scope db cabal file True = concatMap moduleModuleDeclarations <$> scopeModules db cabal file

-- | Completions
completions :: Database -> Cabal -> FilePath -> String -> Bool -> ExceptT String IO [ModuleDeclaration]
completions db cabal file prefix wide = do
	(_, mthis, mproj) <- fileCtx db file
	return $
		newestPackage $ filter (checkDecl . view moduleDeclaration) $
		moduleModuleDeclarations $ scopeModule $
		resolveOne (fileDeps file cabal mproj db) $
		dropImportLists mthis
	where
		(qname, iname) = splitIdentifier prefix
		checkDecl d = fmap fromString qname `elem` scopes d && fromString iname `T.isPrefixOf` view declarationName d
		dropImportLists m
			| wide = set (moduleImports . each . importList) Nothing m
			| otherwise = m

-- | Module completions
moduleCompletions :: Database -> [Module] -> String -> ExceptT String IO [String]
moduleCompletions _ ms prefix = return $ map T.unpack $ ordNub $ completions' $ map (view moduleName) ms where
	completions' = mapMaybe getNext where
		getNext m
			| fromString prefix `T.isPrefixOf` m = listToMaybe $ map snd $ dropWhile (uncurry (==)) $ zip (T.split (== '.') $ fromString prefix) (T.split (== '.') m)
			| otherwise = Nothing

-- | Check module
checkModule :: (ModuleId -> Bool) -> (ModuleDeclaration -> Bool)
checkModule = (. view declarationModuleId)

-- | Check declaration
checkDeclaration :: (Declaration -> Bool) -> (ModuleDeclaration -> Bool)
checkDeclaration = (. view moduleDeclaration)

-- | Allow only selected cabal sandbox
restrictCabal :: Cabal -> ModuleId -> Bool
restrictCabal cabal m = inCabal cabal m || not (byCabal m)

-- | Check whether module is visible from source file
visibleFrom :: Maybe Project -> Module -> ModuleId -> Bool
visibleFrom (Just p) this m = visible p (view moduleId this) m
visibleFrom Nothing this m = view moduleId this == m || byCabal m

-- | Split identifier into module name and identifier itself
splitIdentifier :: String -> (Maybe String, String)
splitIdentifier name = fromMaybe (Nothing, name) $ do
	groups <- matchRx "(([A-Z][\\w']*\\.)*)(.*)" name
	return (fmap dropDot $ groups 1, groups `at_` 3)
	where
		dropDot :: String -> String
		dropDot "" = ""
		dropDot s = init s

-- | Get context file and project
fileCtx :: Database -> FilePath -> ExceptT String IO (FilePath, Module, Maybe Project)
fileCtx db file = do
	file' <- liftE $ canonicalizePath file
	mthis <- fileModule db file'
	mproj <- traverse (getProject db) $ projectOf $ view moduleId mthis
	return (file', mthis, mproj)

-- | Try get context file
fileCtxMaybe :: Database -> FilePath -> ExceptT String IO (FilePath, Maybe Module, Maybe Project)
fileCtxMaybe db file = ((\(f, m, p) -> (f, Just m, p)) <$> fileCtx db file) <|> onlyProj where
	onlyProj = do
		file' <- liftE $ canonicalizePath file
		mproj <- liftE $ locateProject file'
		mproj' <- traverse (getProject db) mproj
		return (file', Nothing, mproj')

-- | Restrict only modules file depends on
fileDeps :: FilePath -> Cabal -> Maybe Project -> Database -> Database
fileDeps file cabal mproj = filterDB fileDeps' (const True) where
	fileDeps' = liftM2 (||)
		(maybe (const True) inProject mproj)
		(liftM2 (&&)
			(restrictCabal cabal)
			(\m -> any (`inDepsOfTarget` m) (fromMaybe [] $ fileTargets <$> mproj <*> pure file)))
