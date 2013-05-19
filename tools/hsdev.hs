{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, OverloadedStrings #-}

module Main (
	main
	) where

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Exception (catch, SomeException)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as L (unpack)
import Data.Aeson
import Data.Either
import Data.Maybe
import Data.Monoid
import Data.List
import Data.Traversable (traverse)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import System.Args hiding (list, detailed)
import System.Command hiding (list, brief, detailed)
import qualified System.Command as C (brief)
import System.Environment
import System.Exit
import System.Directory (canonicalizePath, getDirectoryContents, doesFileExist)
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

main :: IO ()
main = do
	hSetBuffering stdout LineBuffering
	hSetEncoding stdout utf8
	args' <- getArgs
	case args' of
		["help"] -> printMainUsage >> printUsage
		[] -> do
			db <- newAsync
			runMain False db
		["-json"] -> do
			db <- newAsync
			runMain True db
		_ -> putStrLn "Unknown command" >> printUsage

data CommandResult =
	ResultDeclarations [Symbol Declaration] |
	ResultModules [Symbol Module] |
	ResultOk (Map String Param)

instance ToJSON CommandResult where
	toJSON (ResultDeclarations decls) = object [
		"result" .= ("ok" :: String),
		"declarations" .= toJSON (M.fromList $ map (symbolName &&& encodeDeclaration) decls)]
	toJSON (ResultModules ms) = object [
		"result" .= ("ok" :: String),
		"modules" .= toJSON (M.fromList $ map (symbolName &&& encodeModule) ms)]
	toJSON (ResultOk ps) = object [
		"result" .= ("ok" :: String),
		"params" .= toJSON ps]

ok :: CommandResult
ok = ResultOk M.empty

commands :: Async Database -> [Command (CommandAction (Args, CommandResult))]
commands db = map addArgs [
	cmd "scan" "scan modules installed in cabal" [cabal] $ do
		forkAction $ do
			cabal' <- force cabalArg
			ms <- liftCmd $ withConfig (config { configCabal = cabal' }) list
			mapM_ (liftCmd . update db . scanModule cabal') ms
		return ok,
	cmd "scan" "scan project" [arg "project" "p" |> desc "path to .cabal file"] $ do
		forkAction $ do
			projName <- asks (get_ "project")
			proj <- liftIO $ locateProject projName
			maybe (liftIO $ putStrLn $ "Project " ++ projName ++ " not found") (liftCmd . update db . scanProject) proj
		return ok,
	cmd "scan" "scan file" [arg "file" "f"] $ do
		forkAction $ do
			file <- force fileArg
			liftCmd $ update db $ scanFile file
		return ok,
	cmd "scan" "scan module in cabal" [arg "module" "m", cabal] $ do
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
		fmt |> def "detailed"] $ do
			dbval <- getDb
			nm <- asks (get_ "name")
			rs <- liftCmd $ findDeclaration dbval nm
			proj <- getProject
			file <- fileArg
			filters <- liftM (satisfy . catMaybes) $ sequence [
				return $ fmap inProject proj,
				return $ fmap inFile file,
				fmap (\h -> if h then Just bySources else Nothing) $ asks (has "file"),
				fmap (fmap inModule) $ asks (get "module"),
				fmap (fmap inCabal) cabalArg]
			return $ ResultDeclarations $ filter filters rs,
	cmd "list" "list modules" [arg "project" "p" |> opt, cabal |> opt] $ do
		dbval <- getDb
		proj <- getProject
		filters <- liftM (satisfy . catMaybes) $ sequence [
			return $ fmap inProject proj,
			fmap (fmap inCabal) cabalArg]
		return $ ResultModules $ filter filters $ concatMap S.toList $ M.elems $ databaseModules dbval,
	cmd "browse" "browse module" [
		arg "module" "m",
		arg "project" "p" |> opt,
		cabal |> opt,
		fmt |> def "brief"] $ do
			dbval <- getDb
			mname <- asks (get_ "module")
			rs <- liftCmd $ findModule dbval mname
			proj <- getProject
			filters <- liftM (satisfy . catMaybes) $ sequence [
				return $ fmap inProject proj,
				fmap (fmap inCabal) cabalArg]
			case filter filters rs of
				[] -> throwError $ strMsg "Module not found" &&= [("module", Param mname)]
				[m] -> return $ ResultDeclarations $ M.elems $ moduleDeclarations $ symbol m
				ms' -> throwError $ strMsg "Ambiguous modules" &&= [("modules", List $ map showModule ms')],
	cmd "goto" "find symbol declaration" [name, ctx |> opt, fmt |> def "detailed"] $ do
		dbval <- getDb
		file <- fileArg
		nm <- asks (get_ "name")
		rs <- liftCmd $ goToDeclaration dbval file nm
		return $ ResultDeclarations rs,
	cmd "info" "get info for symbol" [name, ctx |> opt] $ do
		dbval <- getDb
		file <- fileArg
		nm <- asks (get_ "name")
		decl <- liftCmd $ symbolInfo dbval file nm
		return $ ResultDeclarations [decl],
	cmd "complete" "autocompletion" [arg "input" "str" |> desc "string to complete", ctx, fmt |> def "name"] $ do
		file <- force fileArg
		dbval <- getDb
		maybe (throwError $ strMsg "Can't locate file in database" &&= [("file", Param file)]) complete $ lookupFile file dbval,
	cmd "complete" "autocompletion" [
		arg "input" "str" |> desc "string to complete",
		arg "module" "m",
		cabal |> opt,
		fmt |> def "name"] $ do
			mname <- asks (get_ "module")
			dbval <- getDb
			cabal' <- fmap (fromMaybe Cabal) cabalArg
			maybe (throwError $ strMsg "Can't find module specified" &&= [("module", Param mname)]) complete $ lookupModule cabal' mname dbval,
	cmd "dump" "dump file names loaded in database" [flag "files" |> expl] $ do
		dbval <- getDb
		return $ ResultOk $ M.fromList $ map (snd &&& (Param . fst)) $ M.assocs $ M.map symbolName $ databaseFiles dbval,
	cmd "dump" "dump module contents" [arg "file" "f" |> desc "file to dump"] $ do
		dbval <- getDb
		file <- force fileArg
		maybe (throwError $ strMsg "File not found" &&= [("file", Param file)]) (return . ResultModules . return) $ M.lookup file (databaseFiles dbval),
	cmd "cache" "dump cache of cabal packages" [flag "dump" |> expl, cabal] $ do
		cabal' <- force cabalArg
		dbval <- getDb
		liftIO $ dump (cabalCache cabal') (cabalModules cabal' dbval)
		return ok,
	cmd "cache" "dump cache of project" [flag "dump" |> expl, arg "project" "p"] $ do
		proj <- force projArg
		dbval <- getDb
		liftIO $ dump (projectCache proj) (projectModules proj dbval)
		return ok,
	cmd "cache" "dump all" [flag "dump" |> expl, cachepath] $ do
		dbval <- getDb
		path' <- force pathArg
		liftIO $ forM_ (M.keys $ databaseCabalModules dbval) $ \c -> dump (path' </> cabalCache c) (cabalModules c dbval)
		liftIO $ forM_ (M.keys $ databaseProjects dbval) $ \p -> dump (path' </> projectCache (project p)) (projectModules (project p) dbval)
		return ok,
	cmd "cache" "load cache for cabal packages" [flag "load" |> expl, cabal] $ cacheLoad $ do
		cabal' <- force cabalArg
		liftIO $ load (cabalCache cabal'),
	cmd "cache" "load cache for project" [flag "load" |> expl, arg "project" "p"] $ cacheLoad $ do
		proj <- force projArg
		liftIO $ load (projectCache proj),
	cmd "cache" "load cache for directory" [flag "load" |> expl, cachepath] $ cacheLoad $ do
		path' <- force pathArg
		cts <- liftM (filter ((== ".json") . takeExtension)) $ liftIO $ getDirectoryContents path'
		liftM mconcat $ forM cts $ \c -> liftIO $ do
			e <- doesFileExist (path' </> c)
			if e then load (path' </> c) else return mempty,
	cmd "help" "this command" [arg "command" "name" |> opt] $ do
		asks (get "command") >>= maybe (liftIO printUsage) (\cmdName ->
			liftIO (mapM_ (putStrLn . unlines . help) $ filter ((== cmdName) . commandName) (commands db)))
		return ok,
	cmd "exit" "exit" [] $ liftIO exitSuccess >> return ok]
	where
		addArgs :: Command (CommandAction CommandResult) -> Command (CommandAction (Args, CommandResult))
		addArgs c = c {
			commandId = liftM2 (,) ask (commandId c) }

		cabal = arg "cabal" "sandbox" |> impl "" |> desc "path to sandbox"
		name = arg "name" "symbol"
		ctx = arg "file" "f" |> desc "context source file"
		cachepath = arg "path" "p" |> def "." |> desc "cache path"
		fmt = arg "format" "fmt" |> opt |> desc "output format, can be 'raw' (using print), 'name' for just name of symbol, 'brief' for short info, 'detailed' for detailed info and 'json' for json output"

		complete :: Symbol Module -> CommandAction CommandResult
		complete m = do
			dbval <- getDb
			input <- asks (get_ "input")
			(liftCmd $ completions dbval m input) >>= return . ResultDeclarations

		getDb :: (MonadIO m) => m Database
		getDb = liftIO $ readAsync db

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

		cacheLoad act = do
			db' <- act
			liftIO $ modifyAsync db (Append db')
			return ok

		formatResult rs = do
			fmt <- asks (get_ "format")
			let
				f = case fmt of
					"raw" -> show
					"name" -> symbolName
					"brief" -> brief
					"detailed" -> detailed
					"json" -> L.unpack . encode . encodeDeclaration
					_ -> show
			liftIO $ mapM_ (putStrLn . f) rs

		showModule m = Dictionary $ M.fromList $ catMaybes [
			Just ("module", Param $ symbolName m),
			fmap (\loc -> ("file", Param (locationFile loc))) $ symbolLocation m,
			fmap (\c -> ("cabal", Param $ show c)) $ moduleCabal (symbol m)]

		asCabal "" = return Cabal
		asCabal p = fmap CabalDev $ liftIO $ canonicalizePath p

		getProject = do
			db <- getDb
			projName <- asks (get "project")
			let
				foundProject = find ((== projName) . Just . projectName) $ M.elems $ databaseProjects db
			projCabal <- traverse (liftIO . canonicalizePath) projName
			return $ msum [foundProject, fmap project projCabal]

		liftCmd = CommandAction . lift . mapErrorT (fmap $ left strMsg)

forkAction :: CommandAction a -> CommandAction ()
forkAction act = do
	r <- ask
	liftIO $ void $ forkIO $ void $ runErrorT (runReaderT (runCommand act) r)

printMainUsage :: IO ()
printMainUsage = mapM_ putStrLn [
	"hsdev",
	"",
	"\thsdev -- run hsdev prompt",
	"\thsdev -json -- run hsdev prompt with json input-output",
	"\thsdev help -- this help",
	""]

printUsage :: IO ()
printUsage = mapM_ (putStrLn . C.brief) $ commands undefined

runMain :: Bool -> Async Database -> IO ()
runMain isJson db = forever runMain' where
	runMain' =  do
		cmd <- getLine
		run ((if isJson then parseJson else parseCmd) (commands db) cmd) (onError isJson) (onOk isJson)
	onError True err = putStrLn $ L.unpack $ encode err
	onError False err = putStrLn $ "error: " ++ errorMsg err
	onOk True (_, v) = putStrLn $ L.unpack $ encode v
	onOk False (as, v) = case v of
		ResultDeclarations decls -> mapM_ (putStrLn . formatResult fmt) decls
		ResultModules ms -> mapM_ (putStrLn . formatModules fmt) ms
		ResultOk _ -> putStrLn "ok"
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
			moduleDetailed m = unlines $ moduleBrief m : map (formatResult "brief") (M.elems $ moduleDeclarations $ symbol m)
