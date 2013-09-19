{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, OverloadedStrings #-}

module Main (
	main
	) where

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Exception
import Control.DeepSeq (force, NFData)
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8 as L (unpack, pack)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.Types (parseMaybe)
import Data.Either (partitionEithers)
import Data.Maybe
import Data.Monoid
import Data.List
import Data.Traversable (traverse)
import qualified Data.Traversable as T (forM)
import Data.Map (Map)
import Data.String (fromString)
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import Network.Socket
import System.Command hiding (brief)
import qualified System.Command as C (brief)
import System.Environment
import System.Exit
import System.Console.GetOpt
import System.Directory (canonicalizePath, getDirectoryContents, doesFileExist, createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath
import System.IO
import System.Process
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
import HsDev.Inspect

import qualified Commands as C

mainCommands :: [Command (IO ())]
mainCommands = [
	cmd_ ["help"] ["command"] "help" $ \as -> case as of
		[] -> do
			printMainUsage
			putStrLn ""
			putStrLn "Interactive commands:"
			putStrLn ""
			printUsage
		cmdname -> do
			let
				addHeader [] = []
				addHeader (h:hs) = map ('\t':) $ ("hsdev " ++ h) : map ('\t':) hs
			case filter ((cmdname `isPrefixOf`) . commandName) commands of
				[] -> putStrLn $ "Unknown command: " ++ unwords cmdname
				helpCmds -> mapM_ (putStrLn . unlines . addHeader . help) helpCmds,
	cmd_ ["run"] [] "run interactive" $ \_ -> do
			db <- newAsync
			forever $ do
				s <- getLine
				r <- processCmd db s putStrLn
				unless r exitSuccess,
	cmd ["server", "start"] [] "start remote server" clientOpts $ \copts _ -> do
		let
			args = ["server", "run"] ++ maybe [] (\p' -> ["--port", show p']) (getFirst $ clientPort copts)
		void $ runInteractiveProcess "powershell" ["-Command", "& {start-process hsdev " ++ intercalate ", " args ++ " -WindowStyle Hidden}"] Nothing Nothing
		printOk copts $ "Server started at port " ++ show (fromMaybe 4567 (getFirst $ clientPort copts)),
	cmd ["server", "run"] [] "start server" clientOpts $ \copts _ -> do
		msgs <- newChan
		forkIO $ getChanContents msgs >>= mapM_ (printOk copts)
		let
			outputStr = writeChan msgs

		db <- newAsync

		waitListen <- newEmptyMVar
		clientChan <- newChan

		forkIO $ do
			accepter <- myThreadId
			s <- socket AF_INET Stream defaultProtocol
			bind s (SockAddrInet (maybe 4567 fromIntegral (getFirst $ clientPort copts)) iNADDR_ANY)
			listen s maxListenQueue
			forever $ handle (ignoreIO outputStr) $ do
				s' <- fmap fst $ accept s
				outputStr $ show s' ++ " connected"
				void $ forkIO $ bracket (socketToHandle s' ReadWriteMode) hClose $ \h -> do
					me <- myThreadId
					done <- newEmptyMVar
					let
						timeoutWait = do
							notDone <- isEmptyMVar done
							when notDone $ do
								void $ forkIO $ do
									threadDelay 1000000
									tryPutMVar done ()
									killThread me
								void $ takeMVar done
					writeChan clientChan (Just timeoutWait)
					req <- hGetLine h
					outputStr $ show s' ++ ": " ++ req
					r <- case eitherDecode (L.pack req) of
						Left reqErr -> do
							hPutStrLn h $ L.unpack $ encode $ errArgs "Invalid request" [("request", ResultString req), ("error", ResultString reqErr)]
							return False
						Right reqArgs -> processCmdArgs db reqArgs (hPutStrLn h)
					unless r $ do
						void $ tryPutMVar waitListen ()
						killThread accepter
					putMVar done ()


		takeMVar waitListen
		outputStr "waiting for clients"
		writeChan clientChan Nothing
		getChanContents clientChan >>= sequence_ . catMaybes . takeWhile isJust
		outputStr "server shutdown",
	cmd ["server", "stop"] [] "stop remote server" clientOpts $ \copts _ -> do
		pname <- getExecutablePath
		r <- readProcess pname (["--json"] ++ maybe [] (\p' -> ["--port", show p']) (getFirst $ clientPort copts) ++ ["exit"]) ""
		let
			stopped = case decode (L.pack r) of
				Just (Object obj) -> fromMaybe False $ flip parseMaybe obj $ \_ -> do
					res <- obj .: "result"
					return (res == ("exit" :: String))
				_ -> False
		printOk copts $ if stopped then "Server stopped" else "Server returned: " ++ r]
	where
		ignoreIO :: (String -> IO ()) -> IOException -> IO ()
		ignoreIO out = out . show

		printOk :: ClientOpts -> String -> IO ()
		printOk copts str
			| getAny (clientJson copts) = putStrLn $ L.unpack $ encode $ object [
				"result" .= ("ok" :: String),
				"message" .= str]
			| otherwise = putStrLn str

data ClientOpts = ClientOpts {
	clientPort :: First Int,
	clientJson :: Any,
	clientData :: Any }

clientOpts :: [OptDescr ClientOpts]
clientOpts = [
	Option [] ["port"] (ReqArg (\p -> mempty { clientPort = First (readMaybe p) }) "number") "connection port",
	Option [] ["json"] (NoArg (mempty { clientJson = Any True })) "json output",
	Option [] ["stdin"] (NoArg (mempty { clientData = Any True })) "pass data to stdin"]

instance Monoid ClientOpts where
	mempty = ClientOpts mempty mempty mempty
	l `mappend` r = ClientOpts (clientPort l `mappend` clientPort r) (clientJson l `mappend` clientJson r) (clientData l `mappend` clientData r)

main :: IO ()
main = withSocketsDo $ do
	hSetBuffering stdout LineBuffering
	hSetEncoding stdout utf8
	as <- getArgs
	when (null as) $ do
		printMainUsage
		exitSuccess
	let
		asr = if last as == "-?" then "help" : init as else as 
	run mainCommands (onDef asr) onError asr
	where
		onError :: [String] -> IO ()
		onError errs = do
			mapM_ putStrLn errs
			exitFailure

		onDef :: [String] -> IO ()
		onDef as = do
			stdinData <- if getAny (clientData p)
				then do
					cdata <- liftM (eitherDecode . L.pack) getContents
					case cdata of
						Left cdataErr -> do
							putStrLn $ "Invalid data: " ++ cdataErr
							exitFailure
						Right dataValue -> return $ Just dataValue
				else return Nothing

			s <- socket AF_INET Stream defaultProtocol
			addr' <- inet_addr "127.0.0.1"
			connect s (SockAddrInet (maybe 4567 fromIntegral (getFirst $ clientPort p)) addr')
			h <- socketToHandle s ReadWriteMode
			hPutStrLn h $ L.unpack $ encode (setJson $ setData stdinData as')
			hGetContents h >>= putStrLn
			where
				(ps, as', _) = getOpt RequireOrder clientOpts as
				p = mconcat ps

				setJson
					| getAny (clientJson p) = ("--json" :)
					| otherwise = id
				setData :: Maybe ResultValue -> [String] -> [String]
				setData Nothing = id
				setData (Just d) = (++ ["--data", L.unpack $ encode d])

data ResultValue =
	ResultDatabase Database |
	ResultDeclaration Declaration |
	ResultModuleDeclaration ModuleDeclaration |
	ResultModuleId ModuleId |
	ResultModule Module |
	ResultInspectedModule InspectedModule |
	ResultProject Project |
	ResultList [ResultValue] |
	ResultMap (Map String ResultValue) |
	ResultString String |
	ResultNone

instance ToJSON ResultValue where
	toJSON (ResultDatabase db) = toJSON db
	toJSON (ResultDeclaration d) = toJSON d
	toJSON (ResultModuleDeclaration md) = toJSON md
	toJSON (ResultModuleId mid) = toJSON mid
	toJSON (ResultModule m) = toJSON m
	toJSON (ResultInspectedModule m) = toJSON m
	toJSON (ResultProject p) = toJSON p
	toJSON (ResultList l) = toJSON l
	toJSON (ResultMap m) = toJSON m
	toJSON (ResultString s) = toJSON s
	toJSON ResultNone = toJSON $ object []

instance FromJSON ResultValue where
	parseJSON v = foldr1 (<|>) [
		do
			(Object m) <- parseJSON v
			if HM.null m then return ResultNone else mzero,
		ResultDatabase <$> parseJSON v,
		ResultDeclaration <$> parseJSON v,
		ResultModuleDeclaration <$> parseJSON v,
		ResultModuleId <$> parseJSON v,
		ResultModule <$> parseJSON v,
		ResultInspectedModule <$> parseJSON v,
		ResultProject <$> parseJSON v,
		ResultList <$> parseJSON v,
		ResultMap <$> parseJSON v,
		ResultString <$> parseJSON v]

data CommandResult =
	ResultOk ResultValue |
	ResultError String (Map String ResultValue) |
	ResultProcess ((String -> IO ()) -> IO ()) |
	ResultExit

instance ToJSON CommandResult where
	toJSON (ResultOk v) = toJSON v
	toJSON (ResultError e as) = object [
		"error" .= e,
		"details" .= as]
	toJSON (ResultProcess _) = object [
		"status" .= ("process" :: String)]
	toJSON ResultExit = object [
		"status" .= ("exit" :: String)]

instance Error CommandResult where
	noMsg = ResultError noMsg mempty
	strMsg s = ResultError s mempty

ok :: CommandResult
ok = ResultOk ResultNone

err :: String -> CommandResult
err s = ResultError s M.empty

errArgs :: String -> [(String, ResultValue)] -> CommandResult
errArgs s as = ResultError s (M.fromList as)

commands :: [Command (Async Database -> IO CommandResult)]
commands = addHelp "hsdev" liftHelp cmds where
	cmds = [
		-- Database commands
		cmd ["add"] [] "add info to database" [dataArg] add',
		cmd ["scan"] [] "scan sources" [
			projectArg "project path or .cabal",
			fileArg "source file",
			pathArg "directory to scan for files and projects",
			ghcOpts, wait, status] scan',
		cmd ["scan", "cabal"] [] "scan modules installed in cabal" [
			sandbox, ghcOpts, wait, status] scanCabal',
		cmd ["scan", "module"] ["module name"] "scan module in cabal" [sandbox, ghcOpts] scanModule',
		cmd ["rescan"] [] "rescan sources" [
			projectArg "project path, .cabal or name to rescan",
			fileArg "source file",
			pathArg "path to rescan",
			ghcOpts, wait, status] rescan',
		cmd ["remove"] [] "remove modules info" [
			sandbox,
			projectArg "module project",
			fileArg "module source file",
			moduleArg "module name",
			allFlag] remove',
		-- | Context free commands
		cmd ["list", "modules"] [] "list modules" [
			projectArg "project to list modules from",
			sandbox, sourced, standaloned] listModules',
		cmd_ ["list", "projects"] [] "list projects" listProjects',
		cmd ["symbol"] ["name"] "get symbol info" [
			projectArg "related project",
			fileArg "source file",
			moduleArg "module name",
			sandbox, sourced, standaloned] symbol',
		cmd ["module"] [] "get module info" [
			moduleArg "module name",
			projectArg "module project",
			fileArg "module source file",
			sandbox] modul',
		cmd_ ["project"] ["project name"] "show project info" project',
		-- Context commands
		cmd ["whois"] ["symbol"] "get info for symbol" ctx whois',
		cmd ["complete"] ["input"] "show completions for input" ctx complete',
		-- Dump/load commands
		cmd ["dump", "cabal"] [] "dump cabal modules" [sandbox, cacheDir, cacheFile] dumpCabal',
		cmd ["dump", "project"] ["projects..."] "dump projects" [cacheDir, cacheFile] dumpProjects',
		cmd ["dump", "standalone"] ["files..."] "dump standalone file module" [cacheDir, cacheFile] dumpFiles',
		cmd ["dump"] [] "dump whole database" [cacheDir, cacheFile] dump',
		cmd ["load"] [] "load data" [cacheDir, cacheFile, dataArg] load',
		-- Exit
		cmd_ ["exit"] [] "exit" $ \_ _ -> return ResultExit]

	-- Command arguments and flags
	allFlag = Option ['a'] ["all"] (NoArg $ opt "all" "") "remove all"
	cacheDir = pathArg "cache path"
	cacheFile = fileArg "cache file"
	ctx = [
		fileArg "source file",
		sandbox]
	dataArg = Option [] ["data"] (ReqArg (opt "data") "contents") "data to pass to command"
	fileArg = Option ['f'] ["file"] (ReqArg (opt "file") "file")
	ghcOpts = Option ['g'] ["ghcopt"] (ReqArg (opt "ghc") "ghc options") "options to pass to GHC"
	moduleArg = Option ['m'] ["module"] (ReqArg (opt "module") "module name")
	pathArg = Option ['p'] ["path"] (ReqArg (opt "path") "path")
	projectArg = Option [] ["project", "proj"] (ReqArg (opt "project") "project")
	sandbox = Option [] ["sandbox"] (OptArg (opt "sandbox" . fromMaybe "") "path") "path to cabal sandbox"
	sourced = Option [] ["src"] (NoArg $ opt "source" "") "source files"
	standaloned = Option [] ["stand"] (NoArg $ opt "standalone" "") "standalone files"
	status = Option ['s'] ["status"] (NoArg $ opt "status" "") "show status of operation, works only with --wait"
	wait = Option ['w'] ["wait"] (NoArg $ opt "wait" "") "wait for operation to complete"

	-- add data
	add' as _ db = do
		dbval <- getDb db
		res <- runErrorT $ do
			jsonData <- maybe (throwError $ err "Specify --data") return $ askOpt "data" as
			decodedData <- either
				(\err -> throwError (errArgs "Unable to decode data" [
					("why", ResultString err),
					("data", ResultString jsonData)]))
				return $
				eitherDecode $ L.pack jsonData
			let
				updateData (ResultDeclaration d) = throwError $ errArgs "Can't insert declaration" [("declaration", ResultDeclaration d)]
				updateData (ResultModuleDeclaration md) = do
					let
						ModuleId mname mloc = declarationModuleId md
						defMod = Module mname Nothing mloc [] mempty mempty
						defInspMod = InspectedModule InspectionNone mloc (Right defMod)
						dbmod = maybe
							defInspMod
							(\i -> i { inspectionResult = inspectionResult i <|> (Right defMod) }) $
							M.lookup mloc (databaseModules dbval)
						updatedMod = dbmod {
							inspectionResult = fmap (addDeclaration $ moduleDeclaration md) (inspectionResult dbmod) }
					update db $ return $ fromModule updatedMod
				updateData (ResultModuleId (ModuleId mname mloc)) = when (M.notMember mloc $ databaseModules dbval) $
					update db $ return $ fromModule $ InspectedModule InspectionNone mloc (Right $ Module mname Nothing mloc [] mempty mempty)
				updateData (ResultModule m) = update db $ return $ fromModule $ InspectedModule InspectionNone (moduleLocation m) (Right m)
				updateData (ResultInspectedModule m) = update db $ return $ fromModule m
				updateData (ResultProject p) = update db $ return $ fromProject p
				updateData (ResultList l) = mapM_ updateData l
				updateData (ResultMap m) = mapM_ updateData $ M.elems m
				updateData (ResultString s) = throwError $ err "Can't insert string"
				updateData ResultNone = return ()
			updateData decodedData
		return $ either id (const (ResultOk ResultNone)) res
	-- scan
	scan' as _ db = updateProcess db as $
		mapM_ (\(n, f) -> forM_ (askOpts n as) f) [
			("project", C.scanProject),
			("file", C.scanFile),
			("path", C.scanPath)]
	-- scan cabal
	scanCabal' as _ db = do
		cabal <- askCabal as
		updateProcess db as $ C.scanCabal cabal
	-- scan cabal module
	scanModule' as [mname] db = do
		cabal <- askCabal as
		updateProcess db as $
			forM_ (askOpts "module" as) (C.scanModule cabal)
	scanModule' as _ db = return $ err "Invalid arguments"
	-- rescan
	rescan' as _ db = do
		dbval <- getDb db
		let
			fileMap = M.fromList $ mapMaybe toPair $
				selectModules (byFile . moduleId) dbval
		projectMods <- T.forM (askOpts "project" as) $ \p -> do
			p' <- getProject db p
			let
				res = M.fromList $ mapMaybe toPair $
					selectModules (byFile . moduleId) dbval
			return $ if M.null res then Left ("Unknown project: " ++ p) else Right res
		fileMods <- T.forM (askOpts "file" as) $ \f ->
			return $ maybe
				(Left $ "Unknown file: " ++ f)
				(Right . M.singleton f) $
				lookupFile f dbval
		dirMods <- T.forM (askOpts "path" as) $ \d -> do
			return $ Right $ M.filterWithKey (\f _ -> isParent d f) fileMap

		let
			(errors, filteredMods) = partitionEithers $
				concat [projectMods, fileMods, dirMods]
			rescanMods = if null filteredMods then fileMap else M.unions filteredMods

		if not (null errors)
			then return $ err $ intercalate ", " errors
			else startProcess as $ \onStatus -> forM_ (M.elems rescanMods) $
				\m -> C.updateDB (C.Settings db onStatus (getGhcOpts as)) $ C.runScan "module" (moduleName m) mempty $
					C.liftErrors $ rescanModule (getGhcOpts as) (getInspected dbval m)
	-- remove
	remove' as _ db = do
		dbval <- getDb db
		cabal <- traverse asCabal $ askOpt "cabal" as
		proj <- traverse (getProject db) $ askOpt "project" as
		file <- traverse canonicalizePath $ askOpt "file" as
		let
			cleanAll = hasOpt "all" as
			filters = catMaybes [
				fmap inProject proj,
				fmap inFile file,
				fmap inModule (askOpt "module" as),
				fmap inCabal cabal]
			toClean = filter (allOf filters . moduleId) (allModules dbval)
			action
				| null filters && cleanAll = do
					modifyAsync db Clear
					return ok
				| null filters && not cleanAll = return $ err "Specify filter or explicitely set flag --all"
				| cleanAll = return $ err "--all flag can't be set with filters"
				| otherwise = do
					mapM_ (modifyAsync db . Remove . fromModule) $ map (getInspected dbval) $ toClean
					return $ ResultOk $ ResultList $ map (ResultModuleId . moduleId) toClean
		action
	-- list modules
	listModules' as _ db = do
		dbval <- getDb db
		proj <- traverse (getProject db) $ askOpt "project" as
		cabal <- traverse asCabal $ askOpt "cabal" as
		let
			filters = allOf $ catMaybes [
				fmap inProject proj,
				fmap inCabal cabal,
				if hasOpt "source" as then Just byFile else Nothing,
				if hasOpt "standalone" as then Just standalone else Nothing]
		return $ ResultOk $ ResultList $ map (ResultModuleId . moduleId) $ selectModules (filters . moduleId) dbval
	-- list projects
	listProjects' _ db = do
		dbval <- getDb db
		return $ ResultOk $ ResultList $ map (ResultString . projectName) $ M.elems $ databaseProjects dbval
	-- get symbol info
	symbol' as ns db = do
		dbval <- getDb db
		proj <- traverse (getProject db) $ askOpt "project" as
		file <- traverse canonicalizePath $ askOpt "file" as
		cabal <- traverse asCabal $ askOpt "cabal" as
		let
			filters = checkModule $ allOf $ catMaybes [
				fmap inProject proj,
				fmap inFile file,
				fmap inModule (askOpt "module" as),
				if hasOpt "source" as then Just byFile else Nothing,
				if hasOpt "standalone" as then Just standalone else Nothing]
			result = return . either
				(\e -> err ("Can't find symbol: " ++ e))
				(ResultOk . ResultList . map ResultModuleDeclaration . filter filters)
		case ns of
			[] -> result $ Right $ allDeclarations dbval
			[nm] -> runErrorT (findDeclaration dbval nm) >>= result
			_ -> return $ err "Too much arguments"
	-- get module info
	modul' as _ db = do
		dbval <- getDb db
		proj <- traverse (getProject db) $ askOpt "project" as
		cabal <- traverse asCabal $ askOpt "cabal" as
		file' <- traverse canonicalizePath $ askOpt "file" as
		let
			filters = allOf $ catMaybes [
				fmap inProject proj,
				fmap inCabal cabal,
				fmap inFile file',
				fmap inModule (askOpt "module" as)]
		rs <- maybe
			(return $ Right $ filter (filters . moduleId) $ allModules dbval)
			(runErrorT . findModule dbval) $
			askOpt "module" as
		case rs of
			Left e -> return $ err e
			Right rs' -> case rs' of
				[] -> return $ err "Module not found"
				[m] -> return $ ResultOk $ ResultModule m
				ms' -> return $ errArgs "Ambiguous modules" [("modules", ResultList $ map (ResultModuleId . moduleId) ms')]
	-- get project info
	project' [] db = return $ err "Project name not specified"
	project' pnames db = do
		dbval <- getDb db
		case filter ((`elem` pnames) . projectName) $ M.elems $ databaseProjects dbval of
			[] -> return $ errArgs "Projects not found" [
				("projects", ResultList $ map ResultString pnames)]
			ps -> return $ ResultOk $ ResultList $ map ResultProject ps
	-- get detailed info about symbol in source file
	whois' as [nm] db = do
		dbval <- getDb db
		liftM (either err (ResultOk . ResultModuleDeclaration)) $
			runErrorT $ symbolInfo dbval (askOpt "file" as) nm
	whois' as _ db = return $ err "Invalid arguments"
	-- completion
	complete' as [] db = complete' as [""] db
	complete' as [input] db = fmap (either id id) $ runErrorT $ do
		srcFile <- maybe (throwError $ err "Must specify source file") (liftIO . canonicalizePath) $ askOpt "file" as
		cabal <- liftIO $ getCabal (askOpt "cabal" as)
		dbval <- liftIO $ getDb db
		liftIO $ maybe
			(return $ errArgs "File not found in database" [
				("file", ResultString srcFile)])
			(\m -> liftM
				(either err (ResultOk . ResultList . map ResultModuleDeclaration))
				(runErrorT (completions dbval m input)))
			(lookupFile srcFile dbval)
	complete' as _ db = return $ err "Invalid arguments"
	-- dump cabal modules
	dumpCabal' as _ db = do
		dbval <- getDb db
		cabal <- getCabal $ askOpt "cabal" as
		let
			dat = cabalDB cabal dbval
		liftM (fromMaybe (ResultOk $ ResultDatabase dat)) $ runMaybeT $ msum [
			do
				p <- MaybeT $ return $ askOpt "path" as
				liftIO $ dump (p </> cabalCache cabal) dat
				return ok,
			do
				f <- MaybeT $ return $ askOpt "file" as
				liftIO $ dump f dat
				return ok]
	-- dump projects
	dumpProjects' as ns db = do
		dbval <- getDb db
		ps <- if null ns
			then return (M.elems $ databaseProjects dbval)
			else mapM (getProject db) ns
		let
			dats = map (id &&& flip projectDB dbval) ps
		liftM (fromMaybe (ResultOk $ ResultList $ map (ResultDatabase . snd) dats)) $
			runMaybeT $ msum [
				do
					p <- MaybeT $ return $ askOpt "path" as
					forM_ dats $ \(proj, dat) -> liftIO (dump (p </> projectCache proj) dat)
					return ok,
				do
					f <- MaybeT $ return $ askOpt "file" as
					liftIO $ dump f (mconcat $ map snd dats)
					return ok]
	-- dump files
	dumpFiles' as ns db = do
		dbval <- getDb db
		fs <- mapM canonicalizePath ns
		let
			dat = filterDB
				(\m -> any (\f -> inFile f (moduleId m)) fs)
				(const False) $
				standaloneDB dbval
		liftM (fromMaybe (ResultOk $ ResultDatabase dat)) $ runMaybeT $ msum [
			do
				p <- MaybeT $ return $ askOpt "path" as
				liftIO $ dump (p </> standaloneCache) dat
				return ok,
			do
				f <- MaybeT $ return $ askOpt "file" as
				liftIO $ dump f dat
				return ok]
	dump' as _ db = do
		dbval <- getDb db
		liftM (fromMaybe (ResultOk $ ResultDatabase dbval)) $ runMaybeT $ msum [
			do
				p <- MaybeT $ return $ askOpt "path" as
				liftIO $ do
					createDirectoryIfMissing True (p </> "cabal")
					createDirectoryIfMissing True (p </> "projects")
					forM_ (nub $ mapMaybe modCabal $ allModules dbval) $ \c ->
						dump
							(p </> "cabal" </> cabalCache c)
							(cabalDB c dbval)
					forM_ (M.keys $ databaseProjects dbval) $ \pr ->
						dump
							(p </> "projects" </> projectCache (project pr))
							(projectDB (project pr) dbval)
					dump (p </> standaloneCache) (standaloneDB dbval)
				return ok,
			do
				f <- MaybeT $ return $ askOpt "file" as
				liftIO $ dump f dbval
				return ok]
	load' as _ db = do
		res <- liftM (fromMaybe (err "Specify one of: --path, --file or --data")) $ runMaybeT $ msum [
			do
				p <- MaybeT $ return $ askOpt "path" as
				cts <- liftIO $ liftM concat $ mapM
					(liftM
						(filter ((== ".json") . takeExtension)) .
						getDirectoryContents')
					[p, p </> "cabal", p </> "projects"]
				liftIO $ forM_ cts $ \c -> do
					e <- doesFileExist c
					when e $ cacheLoad db (load c)
				return ok,
			do
				f <- MaybeT $ return $ askOpt "file" as
				e <- liftIO $ doesFileExist f
				liftIO $ when e $ cacheLoad db (load f)
				return ok]
		waitDb as db
		return res

	-- Helper functions
	getGhcOpts = askOpts "ghc"

	isParent :: FilePath -> FilePath -> Bool
	isParent dir file = norm dir `isPrefixOf` norm file where
		norm = splitDirectories . normalise

	toPair :: Module -> Maybe (FilePath, Module)
	toPair m = case moduleLocation m of
		FileModule f _ -> Just (f, m)
		_ -> Nothing

	modCabal :: Module -> Maybe Cabal
	modCabal m = case moduleLocation m of
		CabalModule c _ _ -> Just c
		_ -> Nothing

	liftHelp :: IO () -> Async Database -> IO CommandResult
	liftHelp f _ = f >> return ok

	getDirectoryContents' :: FilePath -> IO [FilePath]
	getDirectoryContents' p = do
		b <- doesDirectoryExist p
		if b then liftM (map (p </>) . filter (`notElem` [".", ".."])) (getDirectoryContents p) else return []

	waitDb as db = when (hasOpt "wait" as) $ do
		putStrLn "wait for db"
		waitDbVar <- newEmptyMVar
		modifyAsync db (Action $ \d -> putStrLn "db done!" >> putMVar waitDbVar () >> return d)
		takeMVar waitDbVar

	cacheLoad db act = do
		db' <- act
		case db' of
			Left e -> putStrLn e
			Right database -> update db (return database)

	getCabal :: Maybe String -> IO Cabal
	getCabal = liftM (maybe Cabal Sandbox) . traverse canonicalizePath

	asCabal "" = return Cabal
	asCabal p = fmap Sandbox $ canonicalizePath p

	askCabal :: Opts -> IO Cabal
	askCabal = maybe (return Cabal) asCabal . askOpt "cabal"

	getProject :: Async Database -> String -> IO Project
	getProject db projName = do
		db' <- getDb db
		let
			foundProject = find ((== projName) . projectName) $ M.elems $ databaseProjects db'
		projCabal <- canonicalizePath projName
		return $ fromMaybe (error "Impossible happened") $ msum [foundProject, Just $ project projCabal]

	getDb :: (MonadIO m) => Async Database -> m Database
	getDb = liftIO . readAsync

	startProcess :: Opts -> ((String -> IO ()) -> IO ()) -> IO CommandResult
	startProcess as f
		| hasOpt "wait" as = return $ ResultProcess (f . onMsg)
		| otherwise = forkIO (f $ const $ return ()) >> return ok
		where
			onMsg showMsg = if hasOpt "status" as then showMsg else (const $ return ())

	updateProcess :: Async Database -> Opts -> C.UpdateDB IO () -> IO CommandResult
	updateProcess db as act = startProcess as $ \onStatus -> C.updateDB (C.Settings db onStatus (getGhcOpts as)) act

printMainUsage :: IO ()
printMainUsage = do
	mapM_ (putStrLn . ('\t':) . ("hsdev " ++) . C.brief) mainCommands
	putStrLn "\thsdev [--port=number] [--json] [--stdin] interactive command... -- send command to server, use flag stdin to pass data argument through stdin"

printUsage :: IO ()
printUsage = mapM_ (putStrLn . ('\t':) . ("hsdev " ++) . C.brief) commands

processCmd :: Async Database -> String -> (String -> IO ()) -> IO Bool
processCmd db cmdLine printResult = processCmdArgs db (split cmdLine) printResult

processCmdArgs :: Async Database -> [String] -> (String -> IO ()) -> IO Bool
processCmdArgs db cmdArgs printResult = run commands (asCmd unknownCommand) (asCmd . commandError) cmdArgs' db >>= processResult where
	(isJsonArgs, cmdArgs', _) = getOpt RequireOrder [Option [] ["json"] (NoArg (Any True)) "json output"] cmdArgs
	isJson = getAny $ mconcat isJsonArgs
	asCmd :: CommandResult -> (Async Database -> IO CommandResult)
	asCmd r _ = return r

	unknownCommand :: CommandResult
	unknownCommand = err "Unknown command"
	commandError :: [String] -> CommandResult
	commandError errs = errArgs "Command syntax error" [("what", ResultList $ map ResultString errs)]

	processResult :: CommandResult -> IO Bool
	processResult (ResultProcess act) = do
		act (printResult . showStatus)
		processResult ok
		`catch`
		processFailed
		where
			processFailed :: SomeException -> IO Bool
			processFailed e = do
				processResult $ errArgs "process throws exception" [("exception", ResultString $ show e)]
				return True

			showStatus s
				| isJson = L.unpack $ encode $ object ["status" .= s]
				| otherwise = "status: " ++ s

	processResult ResultExit = printResult (if isJson then L.unpack (encode ResultExit) else "bye") >> return False
	processResult v = do
		printResult $ L.unpack $ (if isJson then encode else encodePretty) v
		return True
