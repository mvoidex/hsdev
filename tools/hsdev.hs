{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, OverloadedStrings #-}

module Main (
	main
	) where

import Control.Arrow
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Error
import qualified Data.ByteString.Lazy.Char8 as L (unpack)
import Data.Aeson
import Data.Maybe
import Data.Monoid
import Data.List
import Data.Traversable (traverse)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Network.Socket
import System.Command hiding (brief)
import qualified System.Command as C (brief)
import System.Environment
import System.Exit
import System.Console.GetOpt
import System.Directory (canonicalizePath, getDirectoryContents, doesFileExist, createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath
import System.IO
import Text.Read (readMaybe)

import HsDev.Cache
import HsDev.Commands
import HsDev.Database
import HsDev.Database.Async
import HsDev.Project
import HsDev.Scan
import HsDev.Symbols
import HsDev.Symbols.Util
import HsDev.Symbols.JSON
import HsDev.Tools.GhcMod
import HsDev.Util

mainCommands :: [Command (IO ())]
mainCommands = [
	cmd_ ["help"] ["command"] "help" $ \as -> case as of
		[] -> do
			printMainUsage
			putStrLn ""
			putStrLn "Interactive commands:"
			putStrLn ""
			printUsage
		[cmdname] -> do
			let
				addHeader [] = []
				addHeader (h:hs) = map ('\t':) $ ("hsdev " ++ h) : map ('\t':) hs
			mapM_ (putStrLn . unlines . addHeader . help) $ filter ((== cmdname) . head . commandName) commands
		_ -> do
			printMainUsage
			putStrLn "Invalid arguments",
	cmd_ ["run"] [] "run interactive" $ \_ -> do
			db <- newAsync
			forever $ do
				s <- getLine
				r <- processCmd db s
				putStrLn $ either id id r
				either (const exitSuccess) (const $ return ()) r,
	cmd ["server"] [] "start server" [
		Option ['p'] ["port"] (ReqArg (First . readMaybe) "n") "port number"] $ \p _ -> do
			s <- socket AF_INET Stream defaultProtocol
			bind s (SockAddrInet (maybe 4567 fromInteger (getFirst p)) iNADDR_ANY)
			listen s maxListenQueue
			db <- newAsync
			forever $ handle ignoreIO $ do
				putStrLn "listening for connection"
				s' <- fmap fst $ accept s
				h <- socketToHandle s' ReadWriteMode
				str <- hGetLine h
				putStrLn $ "received: " ++ str
				r <- processCmd db str
				hPutStrLn h $ either id id r
				either (const exitSuccess) (const $ return ()) r
				putStrLn "response sent"
				hClose h]
	where
		ignoreIO :: IOException -> IO ()
		ignoreIO _ = return ()

data ClientOpts = ClientOpts {
	clientPort :: First Int,
	clientJson :: Any }

instance Monoid ClientOpts where
	mempty = ClientOpts mempty mempty
	l `mappend` r = ClientOpts (clientPort l `mappend` clientPort r) (clientJson l `mappend` clientJson r)

main :: IO ()
main = withSocketsDo $ do
	hSetBuffering stdout LineBuffering
	hSetEncoding stdout utf8
	as <- getArgs
	when (null as) $ do
		printMainUsage
		exitSuccess
	run mainCommands (onDef as) onError as
	where
		onError :: [String] -> IO ()
		onError = mapM_ putStrLn

		onDef :: [String] -> IO ()
		onDef as = do
			s <- socket AF_INET Stream defaultProtocol
			addr' <- inet_addr "127.0.0.1"
			connect s (SockAddrInet (maybe 4567 fromIntegral (getFirst $ clientPort p)) addr')
			h <- socketToHandle s ReadWriteMode
			hPutStrLn h $ unsplit $ (if getAny (clientJson p) then ("--json":) else id) as'
			hGetContents h >>= putStrLn
			where
				(ps, as', _) = getOpt RequireOrder [
					Option [] ["port"] (ReqArg (\p -> mempty { clientPort = First (readMaybe p) }) "number") "connection port",
					Option [] ["json"] (NoArg (mempty { clientJson = Any True })) "json output"] as
				p = mconcat ps

--main :: IO ()
--main = withSocketsDo $ do
--	hSetBuffering stdout LineBuffering
--	hSetEncoding stdout utf8
--	(args'@(~(cmdName:cmdArgs))) <- getArgs
--	when (null args') $ do
--		printMainUsage
--		exitSuccess
--	case parseArgs mainCommands args' of
--		Right (act, as) -> runAction act as onError onOk
--		Left _ -> do
--			as <- either (\e -> putStrLn e >> return M.empty) return $ args_ cmdArgs
--			let
--				cmdSpec = maybe [] commandArgs $ find ((== cmdName) . commandName) commands
--				p = fromMaybe (4567 :: Integer) $ val "port" as
--				as' = unargsJson $ M.insert "cmd" [cmdName] $ implArgs cmdSpec $ M.delete "port" as
--			s <- socket AF_INET Stream defaultProtocol
--			addr' <- inet_addr "127.0.0.1"
--			connect s (SockAddrInet (fromIntegral p) addr')
--			h <- socketToHandle s ReadWriteMode
--			hPutStrLn h $ L.unpack $ encode as'
--			hGetContents h >>= putStrLn
--	where
--		onError :: CommandError -> IO ()
--		onError err = putStrLn $ L.unpack $ encode err
--		onOk :: () -> IO ()
--		onOk _ = return ()

data CommandResult =
	ResultDeclarations [Symbol Declaration] |
	ResultModules [Symbol Module] |
	ResultOk (Map String String) |
	ResultError String (Map String String) |
	ResultExit

instance ToJSON CommandResult where
	toJSON (ResultDeclarations decls) = object [
		"result" .= ("ok" :: String),
		"declarations" .= toJSON (map encodeDeclaration decls)]
	toJSON (ResultModules ms) = object [
		"result" .= ("ok" :: String),
		"modules" .= toJSON (map encodeModule ms)]
	toJSON (ResultOk ps) = object [
		"result" .= ("ok" :: String),
		"params" .= toJSON ps]
	toJSON (ResultError e as) = object [
		"result" .= ("error" :: String),
		"error" .= e,
		"details" .= as]
	toJSON ResultExit = object [
		"result" .= ("exit" :: String)]

ok :: CommandResult
ok = ResultOk M.empty

err :: String -> CommandResult
err s = ResultError s M.empty

errArgs :: String -> [(String, String)] -> CommandResult
errArgs s as = ResultError s (M.fromList as)

commands :: [Command (Async Database -> IO CommandResult)]
commands = [
	cmd ["scan", "cabal"] [] "scan modules installed in cabal" [
		Option ['c'] ["cabal"] (ReqArg (First . Just) "path") "path to cabal sandbox"] $ \p _ db -> do
			ms <- runErrorT list
			case ms of
				Left e -> return $ err $ "Failed to invoke ghc-mod 'list': " ++ e
				Right r -> do
					forkIO $ void $ runErrorT $ mapM_ (update db . scanModule (maybe Cabal CabalDev $ getFirst p)) r
					return ok,
	cmd ["scan", "project"] [] "scan project" [
		Option ['p'] ["project"] (ReqArg (First . Just) "path") "project's .cabal file"] $ \p _ db -> do
			proj <- locateProject $ fromMaybe "." $ getFirst p
			case proj of
				Nothing -> return $ err $ "Project " ++ maybe "in current directory" (\p' -> "'" ++ p' ++ "'") (getFirst p) ++ " not found"
				Just proj' -> do
					forkIO $ void $ runErrorT $ update db $ scanProject proj'
					return ok,
	cmd_ ["scan", "file"] ["source file"] "scan file" $ \fs db -> case fs of
		[file] -> do
			forkIO $ void $ runErrorT $ update db $ scanFile file
			return ok
		_ -> return $ err "Invalid arguments",
	cmd ["scan", "module"] ["module name"] "scan module in cabal" [
		Option ['c'] ["cabal"] (ReqArg (First . Just) "path") "path to cabal sandbox"] $ \p ms db -> case ms of
			[mname] -> do
				forkIO $ void $ runErrorT $ update db $ scanModule (maybe Cabal CabalDev $ getFirst p) mname
				return ok
			_ -> return $ err "Invalid arguments",
	cmd ["find"] ["symbol"] "find symbol" [
		Option ['p'] ["project"] (ReqArg (opt "project") "path") "project to find in",
		Option ['f'] ["file"] (ReqArg (opt "file") "path") "file to find in",
		Option ['m'] ["module"] (ReqArg (opt "module") "name") "module to find in",
		Option ['c'] ["cabal"] (ReqArg (opt "cabal") "path") "path to cabal sandbox"] $ \as ns db -> case ns of
			[nm] -> do
				dbval <- getDb db
				proj <- maybe (return Nothing) (fmap Just . getProject db) $ askOpt "project" as
				file <- traverse canonicalizePath (askOpt "file" as)
				cabal <- traverse asCabal $ askOpt "cabal" as
				let
					filters = satisfy $ catMaybes [
						fmap inProject proj,
						fmap inFile file,
						if isJust file then Just bySources else Nothing,
						fmap inModule (askOpt "module" as),
						fmap inCabal cabal]
				rs <- runErrorT $ findDeclaration dbval nm
				return $ either (err . ("Unable to find declaration: " ++)) (ResultDeclarations . filter filters) rs
			_ -> return $ err "Invalid arguments",
	cmd ["list"] [] "list modules" [
		Option ['p'] ["project"] (ReqArg (opt "project") "path") "project to list modules for",
		Option ['c'] ["cabal"] (ReqArg (opt "cabal") "path") "path to cabal sandbox"] $ \as _ db -> do
			dbval <- getDb db
			proj <- maybe (return Nothing) (fmap Just . getProject db) $ askOpt "project" as
			cabal <- traverse asCabal $ askOpt "cabal" as
			let
				filters = satisfy $ catMaybes [
					fmap inProject proj,
					fmap inCabal cabal]
			return $ ResultOk $ M.singleton "modules" $ unlines $ map symbolName $ filter filters $ concatMap S.toList $ M.elems $ databaseModules dbval,
	cmd ["browse"] ["module name"] "browse module" [
		Option ['p'] ["project"] (ReqArg (opt "project") "path") "project to look module in",
		Option ['c'] ["cabal"] (ReqArg (opt "cabal") "path") "path to cabal sandbox"] $ \as ms db -> case ms of
			[mname] -> do
				dbval <- getDb db
				proj <- maybe (return Nothing) (fmap Just . getProject db) $ askOpt "project" as
				cabal <- traverse asCabal $ askOpt "cabal" as
				let
					filters = satisfy $ catMaybes [
						fmap inProject proj,
						fmap inCabal cabal]
				rs <- runErrorT $ findModule dbval mname
				case rs of
					Left e -> return $ err e
					Right rs' -> case filter filters rs' of
						[] -> return $ errArgs "Module not found" [("module", mname)]
						[m] -> return $ ResultModules [m]
						ms' -> return $ errArgs "Ambiguous modules" [("modules", unlines (map showModule ms'))],
	cmd ["goto"] ["symbol"] "find symbol declaration" [
		Option ['f'] ["file"] (ReqArg (opt "file") "path") "context source file"] $ \as ns db -> case ns of
			[nm] -> do
				dbval <- getDb db
				liftM (either err ResultDeclarations) $ runErrorT $ goToDeclaration dbval (askOpt "file" as) nm
			_ -> return $ err "Invalid arguments",
	cmd ["info"] ["symbol"] "get info for symbol" [
		Option ['f'] ["file"] (ReqArg (opt "file") "path") "context source file"] $ \as ns db -> case ns of
			[nm] -> do
				dbval <- getDb db
				liftM (either err (ResultDeclarations . return)) $ runErrorT $ symbolInfo dbval (askOpt "file" as) nm
			_ -> return $ err "Invalid arguments",
	cmd_ ["lookup"] ["source file", "symbol"] "find symbol" $ \ns db -> case ns of
		[file, nm] -> do
			dbval <- getDb db
			r <- runErrorT $ liftM (either ResultDeclarations (ResultDeclarations . return)) (lookupSymbol dbval file nm)
			return $ either err id r
		_ -> return $ err "Invalid arguments",
	cmd_ ["import"] ["source file", "symbol"] "import module to bring symbols in scope" $ \ns db -> case ns of
		[file, nm] -> do
			dbval <- getDb db
			liftM (either err (ResultOk . M.singleton "imports" . unlines)) $ runErrorT (importSymbol dbval file nm)
		_ -> return $ err "Invalid arguments",
	cmd_ ["complete", "file"] ["source file", "input"] "autocompletion" $ \as db -> case as of
		[file, input] -> do
			dbval <- getDb db
			maybe
				(return $ errArgs "File is not scanned" [("file", file)])
				(\m -> liftM (either err ResultDeclarations) (runErrorT (completions dbval m input)))
				(lookupFile file dbval)
		_ -> return $ err "Invalid arguments",
	cmd ["complete", "module"] ["module name", "input"] "autocompletion" [
		Option ['c'] ["cabal"] (ReqArg (First . Just) "path") "path to cabal sandbox"] $ \p as db -> case as of
			[mname, input] -> do
				dbval <- getDb db
				cabal <- getCabal $ getFirst p
				maybe
					(return $ errArgs "Unknown module" [("module", mname)])
					(\m -> liftM (either err ResultDeclarations) (runErrorT (completions dbval m input)))
					(lookupModule cabal mname dbval)
			_ -> return $ err "Invalid arguments",
	cmd_ ["dump", "files"] [] "dump file names loaded in database" $ \_ db -> do
		dbval <- getDb db
		return $ ResultOk $ M.fromList $ map (snd &&& fst) $ M.assocs $ M.map symbolName $ databaseFiles dbval,
	cmd_ ["dump", "contents"] ["file name"] "dump file contents" $ \as db -> case as of
		[file] -> do
			dbval <- getDb db
			return $ maybe (errArgs "File not found" [("file", file)]) (ResultModules . return) $ M.lookup file (databaseFiles dbval)
		_ -> return $ err "Invalid arguments",
	cmd ["cache", "dump", "cabal"] [] "dump cache of cabal packages" [
		Option ['c'] ["cabal"] (ReqArg (First . Just) "path") "path to cabal sandbox"] $ \p _ db -> do
			dbval <- getDb db
			cabal <- getCabal $ getFirst p
			dump (cabalCache cabal) (cabalModules cabal dbval)
			return ok,
	cmd_ ["cache", "dump", "project"] ["project name"] "dump cache of project" $ \as db -> case as of
		[proj] -> do
			dbval <- getDb db
			proj' <- getProject db proj
			dump (projectCache proj') (projectModules proj' dbval)
			return ok,
	cmd_ ["cache", "dump", "all"] ["path"] "dump all" $ \as db -> do
		if length as > 1
			then return (err "Invalid arguments")
			else do
				let
					path = fromMaybe "." $ listToMaybe as
				dbval <- getDb db
				createDirectoryIfMissing True (path </> "cabal")
				createDirectoryIfMissing True (path </> "projects")
				forM_ (M.keys $ databaseCabalModules dbval) $ \c -> dump (path </> "cabal" </> cabalCache c) (cabalModules c dbval)
				forM_ (M.keys $ databaseProjects dbval) $ \p -> dump (path </> "projects" </> projectCache (project p)) (projectModules (project p) dbval)
				return ok,
	cmd ["cache", "load", "cabal"] [] "load cache for cabal packages" [
		Option ['c'] ["cabal"] (ReqArg (First . Just) "path") "path to cabal sandbox"] $ \p _ db -> do
			cabal <- getCabal $ getFirst p
			cacheLoad db $ load (cabalCache cabal)
			return ok,
	cmd_ ["cache", "load", "project"] ["project name"] "load cache for project" $ \as db -> case as of
		[proj] -> do
			proj' <- getProj proj
			cacheLoad db . load . projectCache $ proj'
			return ok
		_ -> return $ err "Invalid arguments",
	cmd_ ["cache", "load", "all"] ["paths..."] "load cache from directory" $ \as db -> do
		cts <- liftM concat $ mapM (liftM (filter ((== ".json") . takeExtension)) . getDirectoryContents') $ concatMap (\p -> [p, p </> "cabal", p </> "projects"]) (if null as then ["."] else as)
		forM_ cts $ \c -> do
			e <- doesFileExist c
			when e $ cacheLoad db (load c)
		return ok,
	cmd_ ["help"] ["command"] "this command" $ \as db -> case as of
		[] -> printUsage >> return ok
		[cmdname] -> do
			let
				addHeader [] = []
				addHeader (h:hs) = map ('\t':) $ ("hsdev " ++ h) : map ('\t':) hs
			mapM_ (putStrLn . unlines . addHeader . help) $ filter ((== cmdname) . head . commandName) commands
			return ok
		_ -> return $ err "Invalid arguments",
	cmd_ ["exit"] [] "exit" $ \_ _ -> return ResultExit]
	where
		getDirectoryContents' :: FilePath -> IO [FilePath]
		getDirectoryContents' p = do
			b <- doesDirectoryExist p
			if b then liftM (map (p </>) . filter (`notElem` [".", ".."])) (getDirectoryContents p) else return []

		cacheLoad db act = do
			db' <- act
			case db' of
				Left e -> putStrLn e
				Right database -> modifyAsync db (Append database)

		showModule = symbolName

		getCabal :: Maybe String -> IO Cabal
		getCabal = liftM (maybe Cabal CabalDev) . traverse canonicalizePath

		getProj :: String -> IO Project
		getProj = fmap project . canonicalizePath

		asCabal "" = return Cabal
		asCabal p = fmap CabalDev $ canonicalizePath p

		getProject :: Async Database -> String -> IO Project
		getProject db projName = do
			db' <- getDb db
			let
				foundProject = find ((== projName) . projectName) $ M.elems $ databaseProjects db'
			projCabal <- canonicalizePath projName
			return $ fromMaybe (error "Impossible happened") $ msum [foundProject, Just $ project projCabal]

		getDb :: (MonadIO m) => Async Database -> m Database
		getDb = liftIO . readAsync

--commands :: [Command (Async Database -> CommandAction (Args, CommandResult))]
--commands = map addArgs [
--	cmd "scan" "scan modules installed in cabal" [cabal] $ \db -> do
--		forkAction $ do
--			cabal' <- force cabalArg
--			ms <- liftIO (runErrorT list) >>= either (\e -> throwError (strMsg ("Failed to invoke ghc-mod list: " ++ e))) return
--			mapM_ (liftCmd . update db . scanModule cabal') ms
--		return ok,
--	cmd "scan" "scan project" [arg "project" "p" |> desc "path to .cabal file"] $ \db -> do
--		forkAction $ do
--			projName <- asks (get_ "project")
--			proj <- liftIO $ locateProject projName
--			maybe (liftIO $ putStrLn $ "Project " ++ projName ++ " not found") (liftCmd . update db . scanProject) proj
--		return ok,
--	cmd "scan" "scan file" [arg "file" "f"] $ \db -> do
--		forkAction $ do
--			file <- force fileArg
--			liftCmd $ update db $ scanFile file
--		return ok,
--	cmd "scan" "scan module in cabal" [arg "module" "m", cabal] $ \db -> do
--		forkAction $ do
--			cabal' <- force cabalArg
--			mname <- asks (get_ "module")
--			liftCmd $ update db $ scanModule cabal' mname
--		return ok,
--	cmd "find" "find symbol" [
--		name,
--		arg "project" "p" |> opt,
--		arg "file" "f" |> opt,
--		arg "module" "m" |> opt,
--		cabal |> opt,
--		fmt |> def "detailed", nodocs, onlyname] $ \db -> do
--			dbval <- getDb db
--			nm <- asks (get_ "name")
--			rs <- liftCmd $ findDeclaration dbval nm
--			proj <- getProject db
--			file <- fileArg
--			filters <- liftM (satisfy . catMaybes) $ sequence [
--				return $ fmap inProject proj,
--				return $ fmap inFile file,
--				fmap (\h -> if h then Just bySources else Nothing) $ asks (has "file"),
--				fmap (fmap inModule) $ asks (get "module"),
--				fmap (fmap inCabal) cabalArg]
--			return $ ResultDeclarations $ filter filters rs,
--	cmd "list" "list modules" [arg "project" "p" |> opt, cabal |> opt] $ \db -> do
--		dbval <- getDb db
--		proj <- getProject db
--		filters <- liftM (satisfy . catMaybes) $ sequence [
--			return $ fmap inProject proj,
--			fmap (fmap inCabal) cabalArg]
--		return $ ResultOk $ M.singleton "modules" $ List $ map symbolName $ filter filters $ concatMap S.toList $ M.elems $ databaseModules dbval,
--	cmd "browse" "browse module" [
--		arg "module" "m",
--		arg "project" "p" |> opt,
--		cabal |> opt,
--		fmt |> def "detailed", nodocs, onlyname] $ \db -> do
--			dbval <- getDb db
--			mname <- asks (get_ "module")
--			rs <- liftCmd $ findModule dbval mname
--			proj <- getProject db
--			filters <- liftM (satisfy . catMaybes) $ sequence [
--				return $ fmap inProject proj,
--				fmap (fmap inCabal) cabalArg]
--			case filter filters rs of
--				[] -> throwError $ strMsg "Module not found" &&= [("module", Param mname)]
--				[m] -> return $ ResultModules [m]
--				ms' -> throwError $ strMsg "Ambiguous modules" &&= [("modules", List $ map showModule ms')],
--	cmd "goto" "find symbol declaration" [name, ctx |> opt, fmt |> def "detailed"] $ \db ->
--		liftM ResultDeclarations (withDbFileName db goToDeclaration),
--	cmd "info" "get info for symbol" [name, ctx |> opt] $ \db ->
--		liftM (ResultDeclarations . return) (withDbFileName db symbolInfo),
--	cmd "lookup" "find symbol" [name, ctx, fmt |> def "brief"] $ \db ->
--		liftM (either ResultDeclarations (ResultDeclarations . return)) (withDbFileName db lookupSymbol'),
--	cmd "import" "import module to bring symbol in scope" [name, ctx] $ \db -> do
--		dbval <- getDb db
--		file <- force fileArg
--		name <- asks (get_ "name")
--		ss <- liftCmd $ importSymbol dbval file name
--		return $ ResultOk $ M.singleton "imports" $ List ss,
--	cmd "complete" "autocompletion" [arg "input" "str" |> desc "string to complete", ctx, fmt |> def "name"] $ \db -> do
--		file <- force fileArg
--		dbval <- getDb db
--		maybe (throwError $ strMsg "Can't locate file in database" &&= [("file", Param file)]) (complete db) $ lookupFile file dbval,
--	cmd "complete" "autocompletion" [
--		arg "input" "str" |> desc "string to complete",
--		arg "module" "m",
--		cabal |> opt,
--		fmt |> def "name", nodocs, onlyname] $ \db -> do
--			mname <- asks (get_ "module")
--			dbval <- getDb db
--			cabal' <- fmap (fromMaybe Cabal) cabalArg
--			maybe (throwError $ strMsg "Can't find module specified" &&= [("module", Param mname)]) (complete db) $ lookupModule cabal' mname dbval,
--	cmd "dump" "dump file names loaded in database" [flag "files" |> expl] $ \db -> do
--		dbval <- getDb db
--		return $ ResultOk $ M.fromList $ map (snd &&& (Param . fst)) $ M.assocs $ M.map symbolName $ databaseFiles dbval,
--	cmd "dump" "dump module contents" [arg "file" "f" |> desc "file to dump"] $ \db -> do
--		dbval <- getDb db
--		file <- force fileArg
--		maybe (throwError $ strMsg "File not found" &&= [("file", Param file)]) (return . ResultModules . return) $ M.lookup file (databaseFiles dbval),
--	cmd "cache" "dump cache of cabal packages" [flag "dump" |> expl, cabal] $ \db -> do
--		cabal' <- force cabalArg
--		dbval <- getDb db
--		liftIO $ dump (cabalCache cabal') (cabalModules cabal' dbval)
--		return ok,
--	cmd "cache" "dump cache of project" [flag "dump" |> expl, arg "project" "p"] $ \db -> do
--		proj <- force projArg
--		dbval <- getDb db
--		liftIO $ dump (projectCache proj) (projectModules proj dbval)
--		return ok,
--	cmd "cache" "dump all" [flag "dump" |> expl, cachepath] $ \db -> do
--		dbval <- getDb db
--		path' <- force pathArg
--		liftIO $ do
--			createDirectoryIfMissing True (path' </> "cabal")
--			createDirectoryIfMissing True (path' </> "projects")
--			forM_ (M.keys $ databaseCabalModules dbval) $ \c -> dump (path' </> "cabal" </> cabalCache c) (cabalModules c dbval)
--			forM_ (M.keys $ databaseProjects dbval) $ \p -> dump (path' </> "projects" </> projectCache (project p)) (projectModules (project p) dbval)
--		return ok,
--	cmd "cache" "load cache for cabal packages" [flag "load" |> expl, cabal] $ \db -> cacheLoad db $ do
--		cabal' <- force cabalArg
--		liftIO $ load (cabalCache cabal'),
--	cmd "cache" "load cache for project" [flag "load" |> expl, arg "project" "p"] $ \db -> cacheLoad db $ do
--		proj <- force projArg
--		liftIO $ load (projectCache proj),
--	cmd "cache" "load cache for directory" [flag "load" |> expl, cachepath] $ \db -> cacheLoad db $ do
--		path' <- force pathArg
--		cts <- liftM concat $ mapM (liftM (filter ((== ".json") . takeExtension)) . liftIO . getDirectoryContents') [path', path' </> "cabal", path' </> "projects"]
--		liftM mconcat $ forM cts $ \c -> liftIO $ do
--			e <- doesFileExist c
--			if e then load c else return mempty,
--	cmd "help" "this command" [arg "command" "name" |> opt] $ \_ -> do
--		asks (get "command") >>= maybe (liftIO printUsage) (\cmdName ->
--			liftIO (mapM_ (putStrLn . unlines . help) $ filter ((== cmdName) . commandName) commands))
--		return ok,
--	cmd "exit" "exit" [] $ \_ -> return ResultExit]
--	where
--		getDirectoryContents' :: FilePath -> IO [FilePath]
--		getDirectoryContents' p = do
--			b <- doesDirectoryExist p
--			if b then liftM (map (p </>) . filter (`notElem` [".", ".."])) (getDirectoryContents p) else return []

--		lookupSymbol' d (Just f) s = lookupSymbol d f s
--		lookupSymbol' _ Nothing _ = throwError "No file"

--		withDbFileName :: Async Database -> (Database -> Maybe FilePath -> String -> ErrorT String IO a) -> CommandAction a
--		withDbFileName db f = join $ liftM3 (\x y z -> liftCmd (f x y z)) (getDb db) fileArg (asks $ get_ "name")

--		addArgs :: Command (Async Database -> CommandAction CommandResult) -> Command (Async Database -> CommandAction (Args, CommandResult))
--		addArgs c = c {
--			commandId = liftM2 (,) ask . commandId c }

--		cabal = arg "cabal" "sandbox" |> impl "" |> desc "path to sandbox"
--		name = arg "name" "symbol"
--		ctx = arg "file" "f" |> desc "context source file"
--		cachepath = arg "path" "p" |> def "." |> desc "cache path"
--		fmt = arg "format" "fmt" |> opt |> desc "output format, can be 'raw' (using print), 'name' for just name of symbol, 'brief' for short info, 'detailed' for detailed info and 'json' for json output"
--		nodocs = flag "nodocs"
--		onlyname = flag "onlyname"

--		complete :: Async Database -> Symbol Module -> CommandAction CommandResult
--		complete db m = do
--			dbval <- getDb db
--			input <- asks (get_ "input")
--			liftM ResultDeclarations (liftCmd $ completions dbval m input)

--		getDb :: (MonadIO m) => Async Database -> m Database
--		getDb = liftIO . readAsync

--		cabalArg :: (MonadReader Args m, MonadIO m, Applicative m) => m (Maybe Cabal)
--		cabalArg = asks (get "cabal") >>= traverse asCabal
--		projArg :: (MonadReader Args m, MonadIO m, Applicative m) => m (Maybe Project)
--		projArg = asks (get "project") >>= traverse (fmap project . liftIO . canonicalizePath)
--		fileArg :: (MonadReader Args m, MonadIO m, Applicative m) => m (Maybe FilePath)
--		fileArg = asks (get "file") >>= traverse (liftIO . canonicalizePath)
--		pathArg :: (MonadReader Args m, MonadIO m, Applicative m) => m (Maybe FilePath)
--		pathArg = asks (get "path") >>= traverse (liftIO . canonicalizePath)
--		force :: Monad m => m (Maybe a) -> m a
--		force = liftM fromJust

--		cacheLoad db act = do
--			db' <- act
--			liftIO $ modifyAsync db (Append db')
--			return ok

--		showModule = symbolName

--		asCabal "" = return Cabal
--		asCabal p = fmap CabalDev $ liftIO $ canonicalizePath p

--		getProject db = do
--			db' <- getDb db
--			projName <- asks (get "project")
--			let
--				foundProject = find ((== projName) . Just . projectName) $ M.elems $ databaseProjects db'
--			projCabal <- traverse (liftIO . canonicalizePath) projName
--			return $ msum [foundProject, fmap project projCabal]

--		liftCmd = CommandAction . lift . mapErrorT (fmap $ left strMsg)

--forkAction :: CommandAction a -> CommandAction ()
--forkAction act = do
--	r <- ask
--	liftIO $ void $ forkIO $ void $ runErrorT (runReaderT (runCommand act) r)

printMainUsage :: IO ()
printMainUsage = do
	mapM_ (putStrLn . ('\t':) . ("hsdev " ++) . C.brief) mainCommands
	putStrLn "\thsdev [--port=number] [--json] interactive command... -- send command to server"

printUsage :: IO ()
printUsage = mapM_ (putStrLn . ('\t':) . ("hsdev " ++) . C.brief) commands

processCmd :: Async Database -> String -> IO (Either String String)
processCmd db cmdArgs = fmap (toResult (getAny $ mconcat isJson)) $ run commands (asCmd unknownCommand) (asCmd . commandError) cmdArgs' db where
	(isJson, cmdArgs', _) = getOpt RequireOrder [Option [] ["json"] (NoArg (Any True)) "json output"] (split cmdArgs)

	asCmd :: CommandResult -> (Async Database -> IO CommandResult)
	asCmd r _ = return r

	unknownCommand :: CommandResult
	unknownCommand = err "Unknown command"
	commandError :: [String] -> CommandResult
	commandError errs = errArgs "Command syntax error" [("what", unlines errs)]

	toResult True ResultExit = Left $ L.unpack $ encode ResultExit
	toResult True v = Right $ L.unpack $ encode v
	toResult False ResultExit = Left "bye"
	toResult False v = Right $ case v of
		ResultDeclarations decls -> unlines $ map (formatResult fmt) decls
		ResultModules ms -> unlines $ map (formatModules fmt) ms
		ResultOk ps -> unlines $ "ok" : map (('\t':) . uncurry showP) (M.toList ps)
		ResultError e ds -> unlines $ ("error: " ++ e) : map (('\t':) . uncurry showP) (M.toList ds)
		where
			fmt :: String
			fmt = "detailed" -- fromMaybe "raw" $ get "format" as
			formatResult fmt' = case fmt' of
				"raw" -> show
				"name" -> symbolName
				"brief" -> brief
				"detailed" -> detailed
				_ -> show
			formatModules fmt' = case fmt' of
				"raw" -> show
				"name" -> symbolName
				"brief" -> moduleBrief
				"detailed" -> moduleDetailed
				_ -> show
			moduleBrief m
				| bySources m = "module " ++ symbolName m ++ maybe "" (\l -> " (" ++ locationFile l ++ ")") (symbolLocation m)
				| otherwise = "module " ++ symbolName m ++ maybe "" (\c -> " (" ++ show c ++ ")") (moduleCabal (symbol m))
			moduleDetailed m = unlines $ moduleBrief m : map (formatResult ("brief" :: String)) (M.elems $ moduleDeclarations $ symbol m)
	showP :: String -> String -> String
	showP n v = n ++ ": " ++ v

--processCmd :: Bool -> Async Database -> String -> IO (Either String String)
--processCmd isJson db cmd' = run (fmap (first ($ db)) $ (if isJson then parseJson else parseCmd) commands cmd') (return . onError isJson) (return . onOk isJson) where
--	onError True err = Right $ L.unpack $ encode err
--	onError False err = Right $ unlines $ ("error: " ++ errorMsg err) : map ('\t':) (concatMap (uncurry showP) (M.toList (errorParams err)))
--	onOk True (_, ResultExit) = Left $ L.unpack $ encode ResultExit
--	onOk True (_, v) = Right $ L.unpack $ encode v
--	onOk False (_, ResultExit) = Left "bye"
--	onOk False (as, v) = Right $ case v of
--		ResultDeclarations decls -> unlines $ map (formatResult fmt) decls
--		ResultModules ms -> unlines $ map (formatModules fmt) ms
--		ResultOk ps -> unlines $ "ok" : map ('\t':) (concatMap (uncurry showP) (M.toList ps))
--		where
--			fmt = fromMaybe "raw" $ get "format" as
--			formatResult fmt' = case fmt' of
--				"raw" -> show
--				"name" -> symbolName
--				"brief" -> brief
--				"detailed" -> detailed
--				_ -> show
--			formatModules fmt' = case fmt' of
--				"raw" -> show
--				"name" -> symbolName
--				"brief" -> moduleBrief
--				"detailed" -> moduleDetailed
--				_ -> show
--			moduleBrief m
--				| bySources m = "module " ++ symbolName m ++ maybe "" (\l -> " (" ++ locationFile l ++ ")") (symbolLocation m)
--				| otherwise = "module " ++ symbolName m ++ maybe "" (\c -> " (" ++ show c ++ ")") (moduleCabal (symbol m))
--			moduleDetailed m = unlines $ moduleBrief m : map (formatResult ("brief" :: String)) (M.elems $ moduleDeclarations $ symbol m)
--	showP :: String -> Param -> [String]
--	showP n (Param s) = [n ++ ": " ++ s]
--	showP n (List ls) = [n ++ ": [" ++ intercalate ", " ls ++ "]"]
--	showP n (Dictionary d) = (n ++ ":") : map ('\t':) (concatMap (uncurry showP) (M.toList d))
