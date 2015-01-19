{-# LANGUAGE OverloadedStrings #-}

module HsDev.Client.Commands (
	commands
	) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Error
import Control.Monad.Catch (try, SomeException(..))
import Control.Monad.Trans.Maybe
import Data.Aeson hiding (Result, Error)
import Data.Aeson.Encode.Pretty
import Data.Either
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Map as M
import Data.String (fromString)
import Data.Text (unpack)
import qualified Data.Text as T (isInfixOf, isPrefixOf)
import Data.Traversable (traverse)
import System.Directory
import System.FilePath
import Text.Read (readMaybe)

import qualified HsDev.Database.Async as DB
import HsDev.Commands
import HsDev.Database
import HsDev.Project
import HsDev.Symbols
import HsDev.Symbols.Resolve (resolveOne, scopeModule, exportsModule)
import HsDev.Symbols.Util
import HsDev.Util
import HsDev.Scan
import HsDev.Server.Message as M
import HsDev.Server.Types
import qualified HsDev.Tools.Cabal as Cabal
import HsDev.Tools.Ghc.Worker
import qualified HsDev.Tools.GhcMod as GhcMod
import qualified HsDev.Tools.Hayoo as Hayoo
import qualified HsDev.Cache.Structured as SC
import HsDev.Cache

import Control.Concurrent.Util
import System.Console.Cmd

import qualified HsDev.Database.Update as Update

-- | Client commands
commands :: [Cmd CommandAction]
commands = [
	-- Ping command
	cmd' "ping" [] [] "ping server" ping',
	cmd' "listen" [] [] "listen server log" listen',
	-- Database commands
	cmd' "add" [] [dataArg] "add info to database" add',
	cmd' "scan" [] (sandboxes ++ [
		manyReq $ projectArg `desc` "project path or .cabal",
		manyReq $ fileArg `desc` "source file",
		manyReq $ pathArg `desc` "directory to scan for files and projects",
		ghcOpts])
		"scan sources"
		scan',
	cmd' "rescan" [] (sandboxes ++ [
		manyReq $ projectArg `desc` "project path or .cabal",
		manyReq $ fileArg `desc` "source file",
		manyReq $ pathArg `desc` "path to rescan",
		ghcOpts])
		"rescan sources"
		rescan',
	cmdList' "remove" [] (sandboxes ++ [
		projectArg `desc` "module project",
		fileArg `desc` "module source file",
		moduleArg,
		packageArg, noLastArg, packageVersionArg,
		allFlag "remove all"])
		"remove modules info"
		remove',
	-- Context free commands
	cmdList' "modules" [] (sandboxes ++ [
		manyReq $ projectArg `desc` "projects to list modules from",
		moduleArg,
		depsArg,
		noLastArg,
		manyReq packageArg,
		sourced, standaloned])
		"list modules"
		listModules',
	cmdList' "packages" [] [] "list packages" listPackages',
	cmdList' "projects" [] [] "list projects" listProjects',
	cmdList' "sandboxes" [] [] "list sandboxes" listSandboxes',
	cmdList' "symbol" ["name"] (matches ++ sandboxes ++ [
		projectArg `desc` "related project",
		fileArg `desc` "source file",
		moduleArg, localsArg,
		packageArg, depsArg, noLastArg, packageVersionArg,
		sourced, standaloned])
		"get symbol info"
		symbol',
	cmd' "module" [] (sandboxes ++ [
		moduleArg, localsArg,
		packageArg, depsArg, noLastArg, packageVersionArg,
		projectArg `desc` "module project",
		fileArg `desc` "module source file",
		sourced])
		"get module info"
		modul',
	cmd' "resolve" [] (sandboxes ++ [
		moduleArg, localsArg,
		projectArg `desc` "module project",
		fileArg `desc` "module source file",
		exportsArg])
		"resolve module scope (or exports)"
		resolve',
	cmd' "project" [] [
		projectArg `desc` "project path or name",
		pathArg `desc` "locate project in parent of this path"]
		"get project info"
		project',
	cmd' "sandbox" [] [
		pathArg `desc` "locate sandbox in parent of this path"]
		"get sandbox info"
		sandbox',
	-- Context commands
	cmdList' "lookup" ["symbol"] ctx "lookup for symbol" lookup',
	cmdList' "whois" ["symbol"] ctx "get info for symbol" whois',
	cmdList' "scope modules" [] ctx "get modules accessible from module or within a project" scopeModules',
	cmdList' "scope" [] (ctx ++ matches ++ [globalArg]) "get declarations accessible from module or within a project" scope',
	cmdList' "complete" ["input"] (ctx ++ [wideArg]) "show completions for input" complete',
	-- Tool commands
	cmdList' "hayoo" ["query"] hayooArgs "find declarations online via Hayoo" hayoo',
	cmdList' "cabal list" ["packages..."] [] "list cabal packages" cabalList',
	cmdList' "ghc-mod lang" [] [] "get LANGUAGE pragmas" ghcmodLang',
	cmdList' "ghc-mod flags" [] [] "get OPTIONS_GHC pragmas" ghcmodFlags',
	cmdList' "ghc-mod type" ["line", "column"] (ctx ++ [ghcOpts]) "infer type with 'ghc-mod type'" ghcmodType',
	cmdList' "ghc-mod check" ["files..."] [sandboxArg, ghcOpts] "check source files" ghcmodCheck',
	cmdList' "ghc-mod lint" ["files..."] [hlintOpts] "lint source file" ghcmodLint',
	cmdList' "ghc-mod check lint" ["files..."] [sandboxArg, ghcOpts, hlintOpts] "check & lint source files" ghcmodCheckLint',
	-- Ghc commands
	cmdList' "ghc eval" ["expr..."] [] "evaluate expression" ghcEval',
	-- Dump/load commands
	cmd' "dump" [] (sandboxes ++ [
		cacheDir, cacheFile,
		manyReq $ projectArg `desc` "project",
		manyReq $ fileArg `desc` "file",
		standaloned,
		allFlag "dump all"])
		"dump database info" dump',
	cmd' "load" [] [cacheDir, cacheFile, dataArg] "load data" load',
	-- Link
	cmd' "link" [] [holdArg] "link to server" link',
	-- Exit
	cmd' "exit" [] [] "exit" exit']
	where
		cmd' :: ToJSON a => String -> [String] -> [Opt] -> String -> ([String] -> Opts String -> CommandActionT a) -> Cmd CommandAction
		cmd' nm pos named descr act = checkPosArgs $ cmd nm pos named descr act' where
			act' (Args args os) copts = do
				r <- runErrorT (act args os copts)
				case r of
					Left (CommandError e ds) -> return $ Error e $ M.fromList $ map (first unpack) ds
					Right r' -> return $ Result $ toJSON r'

		cmdList' :: ToJSON a => String -> [String] -> [Opt] -> String -> ([String] -> Opts String -> CommandActionT [a]) -> Cmd CommandAction
		cmdList' nm pos named descr act = cmd' nm pos (named ++ [splitRes]) descr act' where
			splitRes = flag "split-result" `desc` "split result list and return it with notifications"
			act' args os copts = do
				rs <- act args os' copts
				if flagSet "split-result" isSplit
					then do
						liftIO $ mapM_ (commandNotify copts . resultPart) rs
						return []
					else return rs
				where
					(isSplit, os') = splitOpts [splitRes] os

		-- Command arguments and flags
		allFlag d = flag "all" `short` ['a'] `desc` d
		cacheDir = req "cache-dir" "path" `desc` "cache path"
		cacheFile = req "cache-file" "path" `desc` "cache file"
		ctx = [fileArg `desc` "source file", sandboxArg]
		dataArg = req "data" "contents" `desc` "data to pass to command"
		depsArg = req "deps" "object" `desc` "filter to such that in dependency of specified object (file or project)"
		exportsArg = flag "exports" `short` ['e'] `desc` "resolve module exports"
		fileArg = req "file" "path" `short` ['f']
		findArg = req "find" "query" `desc` "infix match"
		ghcOpts = list "ghc" "option" `short` ['g'] `desc` "options to pass to GHC"
		globalArg = flag "global" `desc` "scope of project"
		hayooArgs = [
			req "page" "n" `short` ['p'] `desc` "page number (0 by default)",
			req "pages" "count" `short` ['n'] `desc` "pages count (1 by default)"]
		hlintOpts = list "hlint" "option" `short` ['h'] `desc` "options to pass to hlint"
		holdArg = flag "hold" `short` ['h'] `desc` "don't return any response"
		localsArg = flag "locals" `short` ['l'] `desc` "look in local declarations"
		noLastArg = flag "no-last" `desc` "select not only last version packages"
		matches = [prefixArg, findArg]
		moduleArg = req "module" "name" `short` ['m'] `desc` "module name"
		packageArg = req "package" "name" `desc` "module package"
		pathArg = req "path" "path" `short` ['p']
		prefixArg = req "prefix" "prefix" `desc` "prefix match"
		projectArg = req "project" "project"
		packageVersionArg = req "version" "id" `short` ['v'] `desc` "package version"
		sandboxArg = req "sandbox" "path" `desc` "path to cabal sandbox"
		sandboxList = manyReq sandboxArg
		sandboxes = [
			flag "cabal" `desc` "cabal",
			sandboxList]
		sourced = flag "src" `desc` "source files"
		standaloned = flag "stand" `desc` "standalone files"
		wideArg = flag "wide" `short` ['f'] `desc` "wide mode - complete as if there were no import lists"

		-- | Ping server
		ping' :: [String] -> Opts String -> CommandActionT Value
		ping' _ _ _ = return $ object ["message" .= ("pong" :: String)]

		-- | Listen server log
		listen' :: [String] -> Opts String -> CommandActionT ()
		listen' _ _ copts = liftIO $ commandListenLog copts $
			mapM_ (\msg -> commandNotify copts (Notification $ object ["message" .= msg]))

		-- | Add data
		add' :: [String] -> Opts String -> CommandActionT ()
		add' _ as copts = do
			dbval <- getDb copts
			jsonData <- maybe (commandError "Specify --data" []) return $ arg "data" as
			decodedData <- either
				(\err -> commandError "Unable to decode data" [
					"why" .= err,
					"data" .= jsonData])
				return $
				eitherDecode $ toUtf8 jsonData

			let
				updateData (ResultDatabase db) = DB.update (dbVar copts) $ return db
				updateData (ResultDeclaration d) = commandError "Can't insert declaration" ["declaration" .= d]
				updateData (ResultModuleDeclaration md) = do
					let
						ModuleId mname mloc = declarationModuleId md
						defMod = Module mname Nothing mloc mempty mempty mempty
						defInspMod = Inspected InspectionNone mloc (Right defMod)
						dbmod = maybe
							defInspMod
							(\i -> i { inspectionResult = inspectionResult i <|> (Right defMod) }) $
							M.lookup mloc (databaseModules dbval)
						updatedMod = dbmod {
							inspectionResult = fmap (addDeclaration $ moduleDeclaration md) (inspectionResult dbmod) }
					DB.update (dbVar copts) $ return $ fromModule updatedMod
				updateData (ResultModuleId (ModuleId mname mloc)) = when (M.notMember mloc $ databaseModules dbval) $
					DB.update (dbVar copts) $ return $ fromModule $ Inspected InspectionNone mloc (Right $ Module mname Nothing mloc mempty mempty mempty)
				updateData (ResultModule m) = DB.update (dbVar copts) $ return $ fromModule $ Inspected InspectionNone (moduleLocation m) (Right m)
				updateData (ResultInspectedModule m) = DB.update (dbVar copts) $ return $ fromModule m
				updateData (ResultProject p) = DB.update (dbVar copts) $ return $ fromProject p
				updateData (ResultList l) = mapM_ updateData l
				updateData (ResultMap m) = mapM_ updateData $ M.elems m
				updateData ResultNone = return ()
				updateData _ = commandError "Invalid data" []
			updateData decodedData

		-- | Scan sources and installed packages
		scan' :: [String] -> Opts String -> CommandActionT ()
		scan' _ as copts = do
			cabals <- getSandboxes copts as
			updateProcess copts as $ concat [
				map (Update.scanCabal (listArg "ghc" as)) cabals,
				concatMap (\(n, f) -> [findPath copts v >>= f (listArg "ghc" as) | v <- listArg n as]) [
					("project", Update.scanProject),
					("file", Update.scanFile),
					("path", Update.scanDirectory)]]

		-- | Rescan data
		rescan' :: [String] -> Opts String -> CommandActionT ()
		rescan' _ as copts = do
			cabals <- getSandboxes copts as
			updateProcess copts as $ map (Update.scanCabal (listArg "ghc" as)) cabals

			dbval <- getDb copts
			let
				fileMap = M.fromList $ mapMaybe toPair $
					selectModules (byFile . moduleId) dbval

			(errors, filteredMods) <- liftM partitionEithers $ mapM runErrorT $ concat [
				[do
					p' <- findProject copts p
					return $ M.fromList $ mapMaybe toPair $
						selectModules (inProject p' . moduleId) dbval |
					p <- listArg "project" as],
				[do
					f' <- findPath copts f
					maybe
						(throwError $ "Unknown file: " ++ f')
						(return . M.singleton f')
						(lookupFile f' dbval) |
					f <- listArg "file" as],
				[do
					d' <- findPath copts d
					return $ M.filterWithKey (\f _ -> isParent d' f) fileMap |
					d <- listArg "path" as]]
			let
				rescanMods = map (getInspected dbval) $
					M.elems $ if null filteredMods then fileMap else M.unions filteredMods

			if not (null errors)
				then commandError (intercalate ", " errors) []
				else updateProcess copts as [Update.runTask "rescanning modules" [] $ do
					needRescan <- Update.liftErrorT $ filterM (changedModule dbval (listArg "ghc" as) . inspectedId) rescanMods
					Update.scanModules (listArg "ghc" as) (map (inspectedId &&& inspectionOpts . inspection) needRescan)]

		-- | Remove data
		remove' :: [String] -> Opts String -> CommandActionT [ModuleId]
		remove' _ as copts = do
			dbval <- getDb copts
			cabal <- getCabal_ copts as
			proj <- traverse (findProject copts) $ arg "project" as
			file <- traverse (findPath copts) $ arg "file" as
			let
				cleanAll = flagSet "all" as
				filters = catMaybes [
					fmap inProject proj,
					fmap inFile file,
					fmap inModule (arg "module" as),
					fmap inPackage (arg "package" as),
					fmap inVersion (arg "version" as),
					fmap inCabal cabal]
				toClean = newest as $ filter (allOf filters . moduleId) (allModules dbval)
				action
					| null filters && cleanAll = liftIO $ do
						DB.modifyAsync (dbVar copts) DB.Clear
						return []
					| null filters && not cleanAll = commandError "Specify filter or explicitely set flag --all" []
					| cleanAll = commandError "--all flag can't be set with filters" []
					| otherwise = liftIO $ do
						DB.modifyAsync (dbVar copts) $ DB.Remove $ mconcat $ map (fromModule . getInspected dbval) toClean
						return $ map moduleId toClean
			action

		-- | List modules
		listModules' :: [String] -> Opts String -> CommandActionT [ModuleId]
		listModules' _ as copts = do
			dbval <- getDb copts
			projs <- traverse (findProject copts) $ listArg "project" as
			deps <- traverse (findDep copts) $ listArg "deps" as
			cabals <- getSandboxes copts as
			let
				packages = listArg "package" as
				hasFilters = not $ null projs && null packages && null cabals && null deps
				filters = allOf $ catMaybes [
					if hasFilters
						then Just $ anyOf $ catMaybes [
							if null projs then Nothing else Just (\m -> any (`inProject` m) projs),
							if null deps then Nothing else Just (\m -> any (`inDeps` m) deps),
							if null packages && null cabals then Nothing
								else Just (\m -> (any (`inPackage` m) packages || null packages) && (any (`inCabal` m) cabals || null cabals))]
						else Nothing,
					fmap (\n m -> fromString n == moduleIdName m) $ arg "module" as,
					if flagSet "src" as then Just byFile else Nothing,
					if flagSet "stand" as then Just standalone else Nothing]
			return $ map moduleId $ newest as $ selectModules (filters . moduleId) dbval

		-- | List packages
		listPackages' :: [String] -> Opts String -> CommandActionT [ModulePackage]
		listPackages' _ _ copts = do
			dbval <- getDb copts
			return $ nub $ sort $
				mapMaybe (moduleCabalPackage . moduleLocation) $
				allModules dbval

		-- | List projects
		listProjects' :: [String] -> Opts String -> CommandActionT [Project]
		listProjects' _ _ copts = do
			dbval <- getDb copts
			return $ M.elems $ databaseProjects dbval

		-- | List sandboxes
		listSandboxes' :: [String] -> Opts String -> CommandActionT [Cabal]
		listSandboxes' _ _ copts = (nub . sort . mapMaybe (cabalOf . moduleId) . allModules) <$> getDb copts

		-- | Get symbol info
		symbol' :: [String] -> Opts String -> CommandActionT [ModuleDeclaration]
		symbol' ns as copts = do
			dbval <- liftM (localsDatabase as) $ getDb copts
			proj <- traverse (findProject copts) $ arg "project" as
			file <- traverse (findPath copts) $ arg "file" as
			deps <- traverse (findDep copts) $ arg "deps" as
			cabal <- getCabal_ copts as
			let
				filters = checkModule $ allOf $ catMaybes [
					fmap inProject proj,
					fmap inFile file,
					fmap inModule (arg "module" as),
					fmap inPackage (arg "package" as),
					fmap inDeps deps,
					fmap inVersion (arg "version" as),
					fmap inCabal cabal,
					if flagSet "src" as then Just byFile else Nothing,
					if flagSet "stand" as then Just standalone else Nothing]
				toResult = newest as . filterMatch as . filter filters
			case ns of
				[] -> return $ toResult $ allDeclarations dbval
				[nm] -> liftM toResult $ mapErrorT
					(liftM $ left (\e -> CommandError ("Can't find symbol: " ++ e) []))
					(findDeclaration dbval nm)
				_ -> commandError "Too much arguments" []

		-- | Get module info
		modul' :: [String] -> Opts String -> CommandActionT Module
		modul' _ as copts = do
			dbval <- liftM (localsDatabase as) $ getDb copts
			proj <- mapErrorT (fmap $ strMsg +++ id) $ traverse (findProject copts) $ arg "project" as
			cabal <- mapErrorT (fmap $ strMsg +++ id) $ getCabal_ copts as
			file' <- mapErrorT (fmap $ strMsg +++ id) $ traverse (findPath copts) $ arg "file" as
			deps <- mapErrorT (fmap $ strMsg +++ id) $ traverse (findDep copts) $ arg "deps" as
			let
				filters = allOf $ catMaybes [
					fmap inProject proj,
					fmap inCabal cabal,
					fmap inFile file',
					fmap inModule (arg "module" as),
					fmap inPackage (arg "package" as),
					fmap inDeps deps,
					fmap inVersion (arg "version" as),
					if flagSet "src" as then Just byFile else Nothing]
			rs <- mapErrorT (fmap $ strMsg +++ id) $
				(newest as . filter (filters . moduleId)) <$> maybe
					(return $ allModules dbval)
					(mapErrorStr . findModule dbval)
					(arg "module" as)
			case rs of
				[] -> commandError "Module not found" []
				[m] -> return m
				ms' -> commandError "Ambiguous modules" ["modules" .= (map moduleId ms')]

		-- | Resolve module scope
		resolve' :: [String] -> Opts String -> CommandActionT Module
		resolve' _ as copts = do
			dbval <- liftM (localsDatabase as) $ getDb copts
			proj <- mapErrorT (fmap $ strMsg +++ id) $ traverse (findProject copts) $ arg "project" as
			cabal <- mapErrorT (fmap $ strMsg +++ id) $ getCabal copts as
			file' <- mapErrorT (fmap $ strMsg +++ id) $ traverse (findPath copts) $ arg "file" as
			let
				filters = allOf $ catMaybes [
					fmap inProject proj,
					fmap inFile file',
					fmap inModule (arg "module" as),
					Just byFile]
			rs <- mapErrorT (fmap $ strMsg +++ id) $
				(newest as . filter (filters . moduleId)) <$> maybe
					(return $ allModules dbval)
					(mapErrorStr . findModule dbval)
					(arg "module" as)
			let
				cabaldb = filterDB (restrictCabal cabal) (const True) dbval
				getScope = if flagSet "exports" as then exportsModule else scopeModule
			case rs of
				[] -> commandError "Module not found" []
				[m] -> return $ getScope $ resolveOne cabaldb m
				ms' -> commandError "Ambiguous modules" ["modules" .= (map moduleId ms')]

		-- | Get project info
		project' :: [String] -> Opts String -> CommandActionT Project
		project' _ as copts = do
			proj <- runMaybeT $ msum $ map MaybeT [
				traverse (mapErrorStr . findProject copts) $ arg "project" as,
				liftM join $ traverse (liftIO . searchProject) $ arg "path" as]
			maybe (commandError "Specify project name, .cabal file or search directory" []) return proj

		-- | Locate sandbox
		sandbox' :: [String] -> Opts String -> CommandActionT Cabal
		sandbox' _ as _ = do
			sbox <- traverse (liftIO . searchSandbox) (arg "path" as)
			maybe (commandError "Specify search directory" []) return sbox

		-- | Lookup info about symbol
		lookup' :: [String] -> Opts String -> CommandActionT [ModuleDeclaration]
		lookup' [nm] as copts = do
			dbval <- getDb copts
			(srcFile, cabal) <- getCtx copts as
			mapErrorStr $ lookupSymbol dbval cabal srcFile nm
		lookup' _ _ _ = commandError "Invalid arguments" []

		-- | Get detailed info about symbol in source file
		whois' :: [String] -> Opts String -> CommandActionT [ModuleDeclaration]
		whois' [nm] as copts = do
			dbval <- getDb copts
			(srcFile, cabal) <- getCtx copts as
			mapErrorStr $ whois dbval cabal srcFile nm
		whois' _ _ _ = commandError "Invalid arguments" []

		-- | Get modules accessible from module or from directory
		scopeModules' :: [String] -> Opts String -> CommandActionT [ModuleId]
		scopeModules' [] as copts = do
			dbval <- getDb copts
			(srcFile, cabal) <- getCtx copts as
			liftM (map moduleId) $ mapErrorStr $ scopeModules dbval cabal srcFile
		scopeModules' _ _ _ = commandError "Invalid arguments" []

		-- | Get declarations accessible from module
		scope' :: [String] -> Opts String -> CommandActionT [ModuleDeclaration]
		scope' [] as copts = do
			dbval <- getDb copts
			(srcFile, cabal) <- getCtx copts as
			liftM (filterMatch as) $ mapErrorStr $ scope dbval cabal srcFile (flagSet "global" as)
		scope' _ _ _ = commandError "Invalid arguments" []

		-- | Completion
		complete' :: [String] -> Opts String -> CommandActionT [ModuleDeclaration]
		complete' [] as copts = complete' [""] as copts
		complete' [input] as copts = do
			dbval <- getDb copts
			(srcFile, cabal) <- getCtx copts as
			mapErrorStr $ completions dbval cabal srcFile input (flagSet "wide" as)
		complete' _ _ _ = commandError "Invalid arguments" []

		-- | Hayoo
		hayoo' :: [String] -> Opts String -> CommandActionT [ModuleDeclaration]
		hayoo' [] _ _ = commandError "Query not specified" []
		hayoo' [query] opts _ = liftM concat $ forM [page .. page + pred pages] $ \i -> liftM
			(mapMaybe Hayoo.hayooAsDeclaration . Hayoo.resultResult) $
			mapErrorStr $ Hayoo.hayoo query (Just i)
			where
				page = fromMaybe 0 $ narg "page" opts
				pages = fromMaybe 1 $ narg "pages" opts
		hayoo' _ _ _ = commandError "Too much arguments" []

		-- | Cabal list
		cabalList' :: [String] -> Opts String -> CommandActionT [Cabal.CabalPackage]
		cabalList' qs _ _ = mapErrorStr $ Cabal.cabalList qs

		-- | Ghc-mod lang
		ghcmodLang' :: [String] -> Opts String -> CommandActionT [String]
		ghcmodLang' _ _ _ = mapErrorStr GhcMod.langs

		-- | Ghc-mod flags
		ghcmodFlags' :: [String] -> Opts String -> CommandActionT [String]
		ghcmodFlags' _ _ _ = mapErrorStr GhcMod.flags

		-- | Ghc-mod type
		ghcmodType' :: [String] -> Opts String -> CommandActionT [GhcMod.TypedRegion]
		ghcmodType' [line] as copts = ghcmodType' [line, "1"] as copts
		ghcmodType' [line, column] as copts = do
			line' <- maybe (commandError "line must be a number" []) return $ readMaybe line
			column' <- maybe (commandError "column must be a number" []) return $ readMaybe column
			dbval <- getDb copts
			(srcFile, cabal) <- getCtx copts as
			(srcFile', _, _) <- mapErrorStr $ fileCtx dbval srcFile
			mapErrorStr $ GhcMod.waitMultiGhcMod (commandGhcMod copts) srcFile' $
				GhcMod.typeOf (listArg "ghc" as) cabal srcFile' line' column'
		ghcmodType' [] _ _ = commandError "Specify line" []
		ghcmodType' _ _ _ = commandError "Too much arguments" []

		-- | Ghc-mod check
		ghcmodCheck' :: [String] -> Opts String -> CommandActionT [GhcMod.OutputMessage]
		ghcmodCheck' [] _ _ = commandError "Specify at least one file" []
		ghcmodCheck' files as copts = do
			files' <- mapM (findPath copts) files
			mproj <- (listToMaybe . catMaybes) <$> liftIO (mapM locateProject files')
			cabal <- getCabal copts as
			mapErrorStr $ liftM concat $ forM files' $ \file' ->
				GhcMod.waitMultiGhcMod (commandGhcMod copts) file' $
					GhcMod.check (listArg "ghc" as) cabal [file'] mproj

		-- | Ghc-mod lint
		ghcmodLint' :: [String] -> Opts String -> CommandActionT [GhcMod.OutputMessage]
		ghcmodLint' [] _ _ = commandError "Specify at least one file to hlint" []
		ghcmodLint' files as copts = do
			files' <- mapM (findPath copts) files
			mapErrorStr $ liftM concat $ forM files' $ \file' ->
				GhcMod.waitMultiGhcMod (commandGhcMod copts) file' $
					GhcMod.lint (listArg "hlint" as) file'

		-- | Ghc-mod check & lint
		ghcmodCheckLint' :: [String] -> Opts String -> CommandActionT [GhcMod.OutputMessage]
		ghcmodCheckLint' [] _ _ = commandError "Specify at least one file" []
		ghcmodCheckLint' files as copts = do
			files' <- mapM (findPath copts) files
			mproj <- (listToMaybe . catMaybes) <$> liftIO (mapM locateProject files')
			cabal <- getCabal copts as
			mapErrorStr $ liftM concat $ forM files' $ \file' ->
				GhcMod.waitMultiGhcMod (commandGhcMod copts) file' $ do
					check' <- GhcMod.check (listArg "ghc" as) cabal [file'] mproj
					lint' <- GhcMod.lint (listArg "hlint" as) file'
					return $ check' ++ lint'

		-- | Evaluate expression
		ghcEval' :: [String] -> Opts String -> CommandActionT [Value]
		ghcEval' exprs _ copts = mapErrorStr $ liftM (map toValue) $ liftTask $
			pushTask (commandGhc copts) $ mapM (try . evaluate) exprs
			where
				toValue :: Either SomeException String -> Value
				toValue (Left (SomeException e)) = object ["fail" .= show e]
				toValue (Right s) = toJSON s

		-- | Dump database info
		dump' :: [String] -> Opts String -> CommandActionT ()
		dump' [] as copts = do
			dbval <- getDb copts

			cabals <- getSandboxes copts as
			ps' <- traverse (findProject copts) $ listArg "project" as
			fs' <- traverse (findPath copts) $ listArg "file" as

			let
				dat = mconcat [
					if flagSet "all" as then dbval else mempty,
					if flagSet "stand" as then standaloneDB dbval else mempty,
					mconcat $ map (`cabalDB` dbval) cabals,
					mconcat $ map (`projectDB` dbval) ps',
					filterDB (\m -> any (`inFile` m) fs') (const False) dbval]

			void $ runMaybeT $ msum [
				do
					p <- MaybeT $ traverse (findPath copts) $ arg "path" as
					fork $ SC.dump p $ structurize dat,
				do
					f <- MaybeT $ traverse (findPath copts) $ arg "file" as
					fork $ dump f dat]
		dump' _ _ _ = commandError "Invalid arguments" []

		-- | Load database
		load' :: [String] -> Opts String -> CommandActionT ()
		load' _ as copts = do
			void $ liftM (maybe (commandError "Specify one of: --path, --file or --data" []) return) $ runMaybeT $ msum [
				do
					p <- MaybeT $ return $ arg "path" as
					lift $ cacheLoad copts (liftA merge <$> SC.load p),
				do
					f <- MaybeT $ return $ arg "file" as
					e <- liftIO $ doesFileExist f
					when e $ lift $ cacheLoad copts (load f),
				do
					dat <- MaybeT $ return $ arg "data" as
					lift $ cacheLoad copts (return $ eitherDecode (toUtf8 dat))]
			waitDb copts as

		-- | Link to server
		link' :: [String] -> Opts String -> CommandActionT ()
		link' _ as copts = liftIO $ do
			commandLink copts
			when (flagSet "hold" as) $ commandHold copts

		-- | Exit
		exit' :: [String] -> Opts String -> CommandActionT ()
		exit' _ _ copts = liftIO $ commandExit copts

-- Helper functions

-- | Check positional args count
checkPosArgs :: Cmd a -> Cmd a
checkPosArgs c = validateArgs pos' c where
	pos' (Args args _) = case cmdArgs c of
		[ellipsis]
			| "..." `isSuffixOf` ellipsis -> return ()
		_ -> mplus
			(guard (length args <= length (cmdArgs c)))
			(failMatch ("unexpected positional arguments: " ++ unwords (drop (length $ cmdArgs c) args)))

-- | Find sandbox by path
findSandbox :: (MonadIO m, Error e) => CommandOptions -> Maybe FilePath -> ErrorT e m Cabal
findSandbox copts = maybe
	(return Cabal)
	(findPath copts >=> mapErrorStr . liftIO . getSandbox)

-- | Canonicalize path
findPath :: (MonadIO m, Error e) => CommandOptions -> FilePath -> ErrorT e m FilePath
findPath copts f = liftIO $ canonicalizePath (normalise f') where
	f'
		| isRelative f = commandRoot copts </> f
		| otherwise = f

-- | Get context: file and sandbox
getCtx :: (MonadIO m, Functor m, Error e) => CommandOptions -> Opts String -> ErrorT e m (FilePath, Cabal)
getCtx copts as = do
	f <- forceJust "No file specified" $ traverse (findPath copts) $ arg "file" as
	c <- getCabal_ copts as >>= maybe (liftIO $ getSandbox f) return
	return (f, c)

-- | Get current sandbox set, user-db by default
getCabal :: (MonadIO m, Error e) => CommandOptions -> Opts String -> ErrorT e m Cabal
getCabal copts as
	| flagSet "cabal" as = findSandbox copts Nothing
	| otherwise  = findSandbox copts $ arg "sandbox" as

-- | Get current sandbox if set
getCabal_ :: (MonadIO m, Functor m, Error e) => CommandOptions -> Opts String -> ErrorT e m (Maybe Cabal)
getCabal_ copts as
	| flagSet "cabal" as = Just <$> findSandbox copts Nothing
	| otherwise = case arg "sandbox" as of
		Just f -> Just <$> findSandbox copts (Just f)
		Nothing -> return Nothing

-- | Get list of enumerated sandboxes
getSandboxes :: (MonadIO m, Functor m, Error e) => CommandOptions -> Opts String -> ErrorT e m [Cabal]
getSandboxes copts as = traverse (findSandbox copts) paths where
	paths
		| flagSet "cabal" as = Nothing : sboxes
		| otherwise = sboxes
	sboxes = map Just $ listArg "sandbox" as

-- | Find project by name of path
findProject :: (MonadIO m, Error e) => CommandOptions -> String -> ErrorT e m Project
findProject copts proj = do
	db' <- getDb copts
	proj' <- liftM addCabal $ findPath copts proj
	let
		resultProj =
			M.lookup proj' (databaseProjects db') <|>
			find ((== proj) . projectName) (M.elems $ databaseProjects db')
	maybe (throwError $ strMsg $ "Projects " ++ proj ++ " not found") return resultProj
	where
		addCabal p
			| takeExtension p == ".cabal" = p
			| otherwise = p </> (takeBaseName p <.> "cabal")

-- | Find dependency: it may be source, project file or project name, also returns sandbox found
findDep :: (MonadIO m, Error e) => CommandOptions -> String -> ErrorT e m (Project, Maybe FilePath, Cabal)
findDep copts depName = do
	proj <- msum [
		mapErrorStr $ do
			p <- liftIO (locateProject depName)
			maybe (throwError $ strMsg $ "Project " ++ depName ++ " not found") (mapErrorT liftIO . loadProject) p,
		findProject copts depName]
	src <- if takeExtension depName == ".hs"
		then liftM Just (findPath copts depName)
		else return Nothing
	sbox <- liftIO $ searchSandbox $ projectPath proj
	return (proj, src, sbox)

-- | Check if project or source depends from this module
inDeps :: (Project, Maybe FilePath, Cabal) -> ModuleId -> Bool
inDeps (proj, src, cabal) = liftM2 (&&) (restrictCabal cabal) deps' where
	deps' = case src of
		Nothing -> inDepsOfProject proj
		Just src' -> inDepsOfFile proj src'

-- | Supply module with its source file path if any
toPair :: Module -> Maybe (FilePath, Module)
toPair m = case moduleLocation m of
	FileModule f _ -> Just (f, m)
	_ -> Nothing

-- | Wait for DB to complete update
waitDb :: CommandOptions -> Opts String -> CommandM ()
waitDb copts as = when (flagSet "wait" as) $ liftIO $ do
	commandLog copts "wait for db"
	DB.wait (dbVar copts)
	commandLog copts "db done"

cacheLoad :: CommandOptions -> IO (Either String Database) -> CommandM ()
cacheLoad copts act = liftIO $ do
	db' <- act
	case db' of
		Left e -> commandLog copts e
		Right database -> DB.update (dbVar copts) (return database)

-- | Bring locals to top scope to search within them if 'locals' flag set
localsDatabase :: Opts String -> Database -> Database
localsDatabase as
	| flagSet "locals" as = databaseLocals
	| otherwise = id

-- | Select newest packages if 'no-last' flag not set
newest :: Symbol a => Opts String -> [a] -> [a]
newest as
	| flagSet "no-last" as = id
	| otherwise = newestPackage

-- | Convert from just of throw
forceJust :: (MonadIO m, Error e) => String -> ErrorT e m (Maybe a) -> ErrorT e m a
forceJust msg act = act >>= maybe (throwError $ strMsg msg) return

-- | Get actual DB state
getDb :: (MonadIO m) => CommandOptions -> m Database
getDb = liftIO . DB.readAsync . commandDatabase

-- | Get DB async var
dbVar :: CommandOptions -> DB.Async Database
dbVar = commandDatabase

mapErrorStr :: (Monad m, Error e) => ErrorT String m a -> ErrorT e m a
mapErrorStr = mapErrorT (liftM $ left strMsg)

-- | Run DB update action
updateProcess :: CommandOptions -> Opts String -> [ErrorT String (Update.UpdateDB IO) ()] -> CommandM ()
updateProcess copts as acts = lift $ Update.updateDB settings $ sequence_ [act `catchError` logErr | act <- acts] where
	settings = Update.Settings
		(commandDatabase copts)
		(commandReadCache copts)
		(commandWriteCache copts)
		(commandNotify copts . Notification . toJSON)
		(listArg "ghc" as)
	logErr :: String -> ErrorT String (Update.UpdateDB IO) ()
	logErr e = liftIO $ commandLog copts e

-- | Filter declarations with prefix and infix
filterMatch :: Opts String -> [ModuleDeclaration] -> [ModuleDeclaration]
filterMatch as = findMatch as . prefMatch as

-- | Filter declarations with infix match
findMatch :: Opts String -> [ModuleDeclaration] -> [ModuleDeclaration]
findMatch as = case arg "find" as of
	Nothing -> id
	Just str -> filter (match' str)
	where
		match' str m = fromString str `T.isInfixOf` declarationName (moduleDeclaration m)

-- | Filter declarations with prefix match
prefMatch :: Opts String -> [ModuleDeclaration] -> [ModuleDeclaration]
prefMatch as = case fmap splitIdentifier (arg "prefix" as) of
	Nothing -> id
	Just (qname, pref) -> filter (match' qname pref)
	where
		match' qname pref m =
			fromString pref `T.isPrefixOf` declarationName (moduleDeclaration m) &&
			maybe True (== moduleIdName (declarationModuleId m)) (fmap fromString qname)
