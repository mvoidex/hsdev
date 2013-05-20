{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, OverloadedStrings #-}

module Main (
	main
	) where

import Control.Applicative
import Control.Arrow
import Control.Concurrent
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
import System.Args hiding (list, detailed)
import System.Command hiding (list, brief, detailed)
import qualified System.Command as C (brief)
import System.Environment
import System.Exit
import System.Directory (canonicalizePath, getDirectoryContents, doesFileExist, createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath
import System.IO

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

mainCommands :: [Command (CommandAction ())]
mainCommands = [
	cmd "help" "help" [] $ liftIO printMainUsage,
	cmd "run" "run interactive" [flag "json" |> desc "json input and output"] $ do
		db <- liftIO newAsync
		isJson <- asks (has "json")
		forever $ liftIO $ do
			s <- getLine
			r <- processCmd isJson db s
			putStrLn r,
	cmd "server" "start server" [arg "port" "p" |> def "4567"] $ do
		p <- asks (val_ "port")
		s <- liftIO $ socket AF_INET Stream defaultProtocol
		liftIO $ bind s (SockAddrInet (fromInteger p) iNADDR_ANY)
		liftIO $ listen s maxListenQueue
		db <- liftIO newAsync
		forever $ liftIO $ do
			s' <- fmap fst $ accept s
			h <- socketToHandle s' ReadWriteMode
			str <- hGetLine h
			r <- processCmd True db str
			hPutStrLn h r
			hClose h]

main :: IO ()
main = withSocketsDo $ do
	hSetBuffering stdout LineBuffering
	hSetEncoding stdout utf8
	(args'@(~(cmdName:cmdArgs))) <- getArgs
	when (null args') $ do
		printMainUsage
		exitSuccess
	case parseArgs mainCommands args' of
		Right (act, as) -> runAction act as onError onOk
		Left _ -> do
			as <- either (\e -> putStrLn e >> return M.empty) return $ args_ cmdArgs
			let
				cmdSpec = maybe [] commandArgs $ find ((== cmdName) . commandName) commands
				p = fromMaybe (4567 :: Integer) $ val "port" as
				as' = unargsJson $ M.insert "cmd" [cmdName] $ implArgs cmdSpec $ M.delete "port" as
			s <- socket AF_INET Stream defaultProtocol
			addr' <- inet_addr "127.0.0.1"
			connect s (SockAddrInet (fromIntegral p) addr')
			h <- socketToHandle s ReadWriteMode
			hPutStrLn h $ L.unpack $ encode as'
			hGetContents h >>= putStrLn
	where
		onError :: CommandError -> IO ()
		onError err = putStrLn $ L.unpack $ encode err
		onOk :: () -> IO ()
		onOk _ = return ()

data CommandResult =
	ResultDeclarations [Symbol Declaration] |
	ResultModules [Symbol Module] |
	ResultOk (Map String Param)

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

ok :: CommandResult
ok = ResultOk M.empty

commands :: [Command (Async Database -> CommandAction (Args, CommandResult))]
commands = map addArgs [
	cmd "scan" "scan modules installed in cabal" [cabal] $ \db -> do
		forkAction $ do
			cabal' <- force cabalArg
			ms <- liftIO (runErrorT list) >>= either (\e -> throwError (strMsg ("Failed to invoke ghc-mod list: " ++ e))) return
			mapM_ (liftCmd . update db . scanModule cabal') ms
		return ok,
	cmd "scan" "scan project" [arg "project" "p" |> desc "path to .cabal file"] $ \db -> do
		forkAction $ do
			projName <- asks (get_ "project")
			proj <- liftIO $ locateProject projName
			maybe (liftIO $ putStrLn $ "Project " ++ projName ++ " not found") (liftCmd . update db . scanProject) proj
		return ok,
	cmd "scan" "scan file" [arg "file" "f"] $ \db -> do
		forkAction $ do
			file <- force fileArg
			liftCmd $ update db $ scanFile file
		return ok,
	cmd "scan" "scan module in cabal" [arg "module" "m", cabal] $ \db -> do
		forkAction $ do
			cabal' <- force cabalArg
			mname <- asks (get_ "module")
			liftCmd $ update db $ scanModule cabal' mname
		return ok,
	cmd "find" "find symbol" [
		name,
		arg "project" "p" |> opt,
		arg "file" "f" |> opt,
		arg "module" "m" |> opt,
		cabal |> opt,
		fmt |> def "detailed", nodocs, onlyname] $ \db -> do
			dbval <- getDb db
			nm <- asks (get_ "name")
			rs <- liftCmd $ findDeclaration dbval nm
			proj <- getProject db
			file <- fileArg
			filters <- liftM (satisfy . catMaybes) $ sequence [
				return $ fmap inProject proj,
				return $ fmap inFile file,
				fmap (\h -> if h then Just bySources else Nothing) $ asks (has "file"),
				fmap (fmap inModule) $ asks (get "module"),
				fmap (fmap inCabal) cabalArg]
			return $ ResultDeclarations $ filter filters rs,
	cmd "list" "list modules" [arg "project" "p" |> opt, cabal |> opt] $ \db -> do
		dbval <- getDb db
		proj <- getProject db
		filters <- liftM (satisfy . catMaybes) $ sequence [
			return $ fmap inProject proj,
			fmap (fmap inCabal) cabalArg]
		return $ ResultOk $ M.singleton "modules" $ List $ map symbolName $ filter filters $ concatMap S.toList $ M.elems $ databaseModules dbval,
	cmd "browse" "browse module" [
		arg "module" "m",
		arg "project" "p" |> opt,
		cabal |> opt,
		fmt |> def "detailed", nodocs, onlyname] $ \db -> do
			dbval <- getDb db
			mname <- asks (get_ "module")
			rs <- liftCmd $ findModule dbval mname
			proj <- getProject db
			filters <- liftM (satisfy . catMaybes) $ sequence [
				return $ fmap inProject proj,
				fmap (fmap inCabal) cabalArg]
			case filter filters rs of
				[] -> throwError $ strMsg "Module not found" &&= [("module", Param mname)]
				[m] -> return $ ResultModules [m]
				ms' -> throwError $ strMsg "Ambiguous modules" &&= [("modules", List $ map showModule ms')],
	cmd "goto" "find symbol declaration" [name, ctx |> opt, fmt |> def "detailed"] $ \db ->
		liftM ResultDeclarations (withDbFileName db goToDeclaration),
	cmd "info" "get info for symbol" [name, ctx |> opt] $ \db ->
		liftM (ResultDeclarations . return) (withDbFileName db symbolInfo),
	cmd "lookup" "find symbol" [name, ctx, fmt |> def "brief"] $ \db ->
		liftM (either ResultDeclarations (ResultDeclarations . return)) (withDbFileName db lookupSymbol'),
	cmd "import" "import module to bring symbol in scope" [name, ctx] $ \db -> do
		dbval <- getDb db
		file <- force fileArg
		name <- asks (get_ "name")
		ss <- liftCmd $ importSymbol dbval file name
		return $ ResultOk $ M.singleton "imports" $ List ss,
	cmd "complete" "autocompletion" [arg "input" "str" |> desc "string to complete", ctx, fmt |> def "name"] $ \db -> do
		file <- force fileArg
		dbval <- getDb db
		maybe (throwError $ strMsg "Can't locate file in database" &&= [("file", Param file)]) (complete db) $ lookupFile file dbval,
	cmd "complete" "autocompletion" [
		arg "input" "str" |> desc "string to complete",
		arg "module" "m",
		cabal |> opt,
		fmt |> def "name", nodocs, onlyname] $ \db -> do
			mname <- asks (get_ "module")
			dbval <- getDb db
			cabal' <- fmap (fromMaybe Cabal) cabalArg
			maybe (throwError $ strMsg "Can't find module specified" &&= [("module", Param mname)]) (complete db) $ lookupModule cabal' mname dbval,
	cmd "dump" "dump file names loaded in database" [flag "files" |> expl] $ \db -> do
		dbval <- getDb db
		return $ ResultOk $ M.fromList $ map (snd &&& (Param . fst)) $ M.assocs $ M.map symbolName $ databaseFiles dbval,
	cmd "dump" "dump module contents" [arg "file" "f" |> desc "file to dump"] $ \db -> do
		dbval <- getDb db
		file <- force fileArg
		maybe (throwError $ strMsg "File not found" &&= [("file", Param file)]) (return . ResultModules . return) $ M.lookup file (databaseFiles dbval),
	cmd "cache" "dump cache of cabal packages" [flag "dump" |> expl, cabal] $ \db -> do
		cabal' <- force cabalArg
		dbval <- getDb db
		liftIO $ dump (cabalCache cabal') (cabalModules cabal' dbval)
		return ok,
	cmd "cache" "dump cache of project" [flag "dump" |> expl, arg "project" "p"] $ \db -> do
		proj <- force projArg
		dbval <- getDb db
		liftIO $ dump (projectCache proj) (projectModules proj dbval)
		return ok,
	cmd "cache" "dump all" [flag "dump" |> expl, cachepath] $ \db -> do
		dbval <- getDb db
		path' <- force pathArg
		liftIO $ do
			createDirectoryIfMissing True (path' </> "cabal")
			createDirectoryIfMissing True (path' </> "projects")
			forM_ (M.keys $ databaseCabalModules dbval) $ \c -> dump (path' </> "cabal" </> cabalCache c) (cabalModules c dbval)
			forM_ (M.keys $ databaseProjects dbval) $ \p -> dump (path' </> "projects" </> projectCache (project p)) (projectModules (project p) dbval)
		return ok,
	cmd "cache" "load cache for cabal packages" [flag "load" |> expl, cabal] $ \db -> cacheLoad db $ do
		cabal' <- force cabalArg
		liftIO $ load (cabalCache cabal'),
	cmd "cache" "load cache for project" [flag "load" |> expl, arg "project" "p"] $ \db -> cacheLoad db $ do
		proj <- force projArg
		liftIO $ load (projectCache proj),
	cmd "cache" "load cache for directory" [flag "load" |> expl, cachepath] $ \db -> cacheLoad db $ do
		path' <- force pathArg
		cts <- liftM concat $ mapM (liftM (filter ((== ".json") . takeExtension)) . liftIO . getDirectoryContents') [path', path' </> "cabal", path' </> "projects"]
		liftM mconcat $ forM cts $ \c -> liftIO $ do
			e <- doesFileExist c
			if e then load c else return mempty,
	cmd "help" "this command" [arg "command" "name" |> opt] $ \_ -> do
		asks (get "command") >>= maybe (liftIO printUsage) (\cmdName ->
			liftIO (mapM_ (putStrLn . unlines . help) $ filter ((== cmdName) . commandName) commands))
		return ok,
	cmd "exit" "exit" [] $ \_ -> liftIO exitSuccess >> return ok]
	where
		getDirectoryContents' :: FilePath -> IO [FilePath]
		getDirectoryContents' p = do
			b <- doesDirectoryExist p
			if b then liftM (map (p </>) . filter (`notElem` [".", ".."])) (getDirectoryContents p) else return []

		lookupSymbol' d (Just f) s = lookupSymbol d f s
		lookupSymbol' _ Nothing _ = throwError "No file"

		withDbFileName :: Async Database -> (Database -> Maybe FilePath -> String -> ErrorT String IO a) -> CommandAction a
		withDbFileName db f = join $ liftM3 (\x y z -> liftCmd (f x y z)) (getDb db) fileArg (asks $ get_ "name")

		addArgs :: Command (Async Database -> CommandAction CommandResult) -> Command (Async Database -> CommandAction (Args, CommandResult))
		addArgs c = c {
			commandId = liftM2 (,) ask . commandId c }

		cabal = arg "cabal" "sandbox" |> impl "" |> desc "path to sandbox"
		name = arg "name" "symbol"
		ctx = arg "file" "f" |> desc "context source file"
		cachepath = arg "path" "p" |> def "." |> desc "cache path"
		fmt = arg "format" "fmt" |> opt |> desc "output format, can be 'raw' (using print), 'name' for just name of symbol, 'brief' for short info, 'detailed' for detailed info and 'json' for json output"
		nodocs = flag "nodocs"
		onlyname = flag "onlyname"

		complete :: Async Database -> Symbol Module -> CommandAction CommandResult
		complete db m = do
			dbval <- getDb db
			input <- asks (get_ "input")
			liftM ResultDeclarations (liftCmd $ completions dbval m input)

		getDb :: (MonadIO m) => Async Database -> m Database
		getDb = liftIO . readAsync

		cabalArg :: (MonadReader Args m, MonadIO m, Applicative m) => m (Maybe Cabal)
		cabalArg = asks (get "cabal") >>= traverse asCabal
		projArg :: (MonadReader Args m, MonadIO m, Applicative m) => m (Maybe Project)
		projArg = asks (get "project") >>= traverse (fmap project . liftIO . canonicalizePath)
		fileArg :: (MonadReader Args m, MonadIO m, Applicative m) => m (Maybe FilePath)
		fileArg = asks (get "file") >>= traverse (liftIO . canonicalizePath)
		pathArg :: (MonadReader Args m, MonadIO m, Applicative m) => m (Maybe FilePath)
		pathArg = asks (get "path") >>= traverse (liftIO . canonicalizePath)
		force :: Monad m => m (Maybe a) -> m a
		force = liftM fromJust

		cacheLoad db act = do
			db' <- act
			liftIO $ modifyAsync db (Append db')
			return ok

		showModule = symbolName

		asCabal "" = return Cabal
		asCabal p = fmap CabalDev $ liftIO $ canonicalizePath p

		getProject db = do
			db' <- getDb db
			projName <- asks (get "project")
			let
				foundProject = find ((== projName) . Just . projectName) $ M.elems $ databaseProjects db'
			projCabal <- traverse (liftIO . canonicalizePath) projName
			return $ msum [foundProject, fmap project projCabal]

		liftCmd = CommandAction . lift . mapErrorT (fmap $ left strMsg)

forkAction :: CommandAction a -> CommandAction ()
forkAction act = do
	r <- ask
	liftIO $ void $ forkIO $ void $ runErrorT (runReaderT (runCommand act) r)

printMainUsage :: IO ()
printMainUsage = mapM_ (putStrLn . C.brief) mainCommands

printUsage :: IO ()
printUsage = mapM_ (putStrLn . C.brief) commands

processCmd :: Bool -> Async Database -> String -> IO String
processCmd isJson db cmd' = run (fmap (first ($ db)) $ (if isJson then parseJson else parseCmd) commands cmd') (return . onError isJson) (return . onOk isJson) where
	onError True err = L.unpack $ encode err
	onError False err = unlines $ ("error: " ++ errorMsg err) : map ('\t':) (concatMap (uncurry showP) (M.toList (errorParams err)))
	onOk True (_, v) = L.unpack $ encode v
	onOk False (as, v) = case v of
		ResultDeclarations decls -> unlines $ map (formatResult fmt) decls
		ResultModules ms -> unlines $ map (formatModules fmt) ms
		ResultOk ps -> unlines $ "ok" : map ('\t':) (concatMap (uncurry showP) (M.toList ps))
		where
			fmt = fromMaybe "raw" $ get "format" as
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
	showP :: String -> Param -> [String]
	showP n (Param s) = [n ++ ": " ++ s]
	showP n (List ls) = [n ++ ": [" ++ intercalate ", " ls ++ "]"]
	showP n (Dictionary d) = (n ++ ":") : map ('\t':) (concatMap (uncurry showP) (M.toList d))
