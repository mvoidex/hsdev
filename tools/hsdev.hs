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
import Control.Monad.Reader
import Control.Monad.Error
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

processPacks :: Monad m => Int -> [a] -> ([a] -> m ()) -> m ()
processPacks n vs f = mapM_ f ps where
	ps = takeWhile (not . null) $ unfoldr (Just . (take n &&& drop n )) vs

collectM :: (Monad m, NFData b, Monoid b) => [a] -> (a -> m b) -> m b
collectM vs f = foldM f' mempty vs where
	f' acc v = force acc `seq` do
		res <- f v
		return $ acc `mappend` res

processM :: (Monad m, NFData b, Monoid b) => Int -> [a] -> (b -> m ()) -> (a -> m b) -> m ()
processM n vs onChunk convert = processPacks n vs $ (flip collectM convert >=> onChunk)

commands :: [Command (Async Database -> IO CommandResult)]
commands = addHelp "hsdev" liftHelp [
	cmd ["add"] [] "add info to database" [dataArg] $
		\as _ db -> do
			dbval <- getDb db
			res <- runErrorT $ do
				jsonData <- maybe (throwError $ err "Specify --data") return $ askOpt "data" as
				decodedData <- either (\err -> throwError (errArgs "Unable to decode data" [("why", ResultString err), ("data", ResultString jsonData)])) return $ eitherDecode $ L.pack jsonData
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
			return $ either id (const (ResultOk ResultNone)) res,
	cmd ["scan", "cabal"] [] "scan modules installed in cabal" (sandbox : ghcOpts : [wait, status, packSz]) $
		\as _ db -> do
			dbval <- getDb db
			ms <- runErrorT $ list (getGhcOpts as)
			cabal <- maybe (return Cabal) asCabal $ askOpt "cabal" as
			case ms of
				Left e -> return $ err $ "Failed to invoke ghc-mod 'list': " ++ e
				Right r -> startProcess as $ \onStatus -> void $ runErrorT $ do
					processM (packArg as) r (update db . return) $ \mname ->
						if (isNothing $ lookupModule (CabalModule cabal Nothing mname) dbval)
							then do
								r' <- scanModule (getGhcOpts as) cabal mname
								liftIO $ onStatus $ "module " ++ mname ++ " scanned"
								return r'
								`catchError`
								\e -> do
									liftIO $ onStatus $ "error scanning module " ++ mname ++ ": " ++ e
									return mempty
							else return mempty,
	cmd ["scan", "project"] [] "scan project" (projectArg "project's .cabal file" : ghcOpts : [wait, status]) $
		\as _ db -> do
			dbval <- getDb db
			proj <- locateProject $ fromMaybe "." $ askOpt "project" as
			case proj of
				Nothing -> return $ err $ "Project " ++ maybe "in current directory" (\p' -> "'" ++ p' ++ "'") (askOpt "project" as) ++ " not found"
				Just proj' -> startProcess as $ \onStatus -> void $ runErrorT $ flip catchError (liftIO . onStatus . (("scanning project " ++ projectName proj' ++ " fails with: ") ++)) $ do
					proj'' <- loadProject proj'
					update db $ return $ fromProject proj''
					srcs <- projectSources proj''
					update db $ collectM srcs $ \src -> scanFile_ src (liftIO . onStatus) $ do
						maybe (fmap fromModule $ inspectFile (getGhcOpts as) src) (rescanModule (getGhcOpts as)) $ fmap (getInspected dbval) $ lookupFile src dbval,
	cmd ["scan", "path"] ["path to scan"] "scan directory" (
		Option ['p'] ["projects"] (NoArg (opt "projects" "")) "scan only for projects" : ghcOpts : [wait, status]) $ \as ps db -> case ps of
		[dir] -> do
			dbval <- getDb db
			dir' <- canonicalizePath dir
			exists <- doesDirectoryExist dir'
			case exists of
				False -> return $ err $ "Invalid directory: " ++ dir
				True -> case hasOpt "projects" as of
					False -> startProcess as $ \onStatus -> void $ runErrorT $ do
						liftIO $ onStatus $ "scanning " ++ dir ++ " for haskell sources"
						srcs <- liftM (filter haskellSource) $ liftIO $ traverseDirectory dir'
						processM (packArg as) srcs (update db . return) $ \src -> scanFile_ src (liftIO . onStatus) $ do
							maybe (fmap fromModule $ inspectFile (getGhcOpts as) src) (rescanModule (getGhcOpts as)) $ fmap (getInspected dbval) $ lookupFile src dbval
					True -> startProcess as $ \onStatus -> do
						onStatus $ "scanning " ++ dir ++ " for cabal projects"
						cabals <- liftM (filter cabalFile) $ traverseDirectory dir'
						forM_ cabals $ \cabal -> void $ runErrorT $ flip catchError (liftIO . onStatus . (("scanning project " ++ cabal ++ " fails with: ") ++)) $ do
							proj <- loadProject $ project cabal
							update db $ return $ fromProject proj
							srcs <- projectSources proj
							processM (packArg as) srcs (update db . return) $ \src -> scanFile_ src (liftIO . onStatus) $ do
								maybe (fmap fromModule $ inspectFile (getGhcOpts as) src) (rescanModule (getGhcOpts as)) $ fmap (getInspected dbval) $ lookupFile src dbval
							liftIO (onStatus $ "project " ++ projectName proj ++ " scanned")
		_ -> return $ err "Invalid arguments",
	cmd ["scan", "file"] ["source file"] "scan file" [ghcOpts] $ \as fs db -> case fs of
		[file] -> do
			dbval <- getDb db
			file' <- canonicalizePath file
			res <- runErrorT $ maybe (update db $ fmap fromModule $ inspectFile (getGhcOpts as) file') (update db . rescanModule (getGhcOpts as)) $ fmap (getInspected dbval) $ lookupFile file' dbval
			return $ either err (const ok) res
		_ -> return $ err "Invalid arguments",
	cmd ["scan", "module"] ["module name"] "scan module in cabal" [sandbox, ghcOpts] $ \as ms db -> case ms of
		[mname] -> do
			dbval <- getDb db
			if (isNothing $ lookupModule (CabalModule (maybe Cabal Sandbox $ askOpt "cabal" as) Nothing mname) dbval)
				then do
					res <- runErrorT $ update db $ scanModule (getGhcOpts as) (maybe Cabal Sandbox $ askOpt "cabal" as) mname
					return $ either err (const ok) res
				else return ok
		_ -> return $ err "Invalid arguments",
	cmd ["rescan"] [] "rescan sources" ([
		projectArg "project to rescan",
		fileArg "file to rescan",
		Option ['d'] ["dir"] (ReqArg (opt "dir") "directory") "directory to rescan"] ++ [ghcOpts, wait, status]) $
			\as _ db -> do
				dbval <- getDb db
				let
					fileMap = M.fromList $ mapMaybe toPair $ selectModules (byFile . moduleId) dbval
				projectMods <- T.forM (askOpt "project" as) $ \p -> do
					p' <- getProject db p
					let
						res = M.fromList $ mapMaybe toPair $ selectModules (byFile . moduleId) (projectDB p' dbval)
					return $ if M.null res then Left ("Unknown project: " ++ p) else Right res
				fileMods <- T.forM (askOpt "file" as) $ \f ->
					return $ maybe (Left $ "Unknown file: " ++ f) (Right . M.singleton f) $ lookupFile f dbval
				dirMods <- T.forM (askOpt "dir" as) $ \d -> do
					return $ Right $ M.filterWithKey (\f _ -> isParent d f) fileMap

				let
					(errors, filteredMods) = partitionEithers $ catMaybes [projectMods, fileMods, dirMods]
					rescanMods = if null filteredMods then fileMap else M.unions filteredMods

				if not $ null errors
					then return $ err $ intercalate ", " errors
					else startProcess as $ \onStatus -> void $ runErrorT $ do
						processM (packArg as) (M.elems rescanMods) (update db . return) $ \m -> do
							r <- rescanModule (getGhcOpts as) (getInspected dbval m)
							liftIO $ onStatus $ "module " ++ brief m ++ " rescanned"
							return r
							`catchError`
							\e -> do
								liftIO $ onStatus $ "error rescanning module " ++ brief m ++ ": " ++ e
								return mempty,
	cmd ["clean"] [] "clean info about modules" [
		sandbox,
		projectArg "module project",
		fileArg "module source file",
		moduleArg "module name",
		Option ['a'] ["all"] (NoArg (opt "all" "")) "clear all"] $ \as _ db -> do
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
			action,
	cmd ["find"] ["symbol"] "find symbol" [
		projectArg "project to find in",
		fileArg "file to find in",
		moduleArg "module to find in",
		sandbox,
		sourced,
		standaloned] $ \as ns db -> do
			dbval <- getDb db
			proj <- traverse (getProject db) $ askOpt "project" as
			file <- traverse canonicalizePath (askOpt "file" as)
			cabal <- traverse asCabal $ askOpt "cabal" as
			let
				filters = checkModule $ allOf $ catMaybes [
					fmap inProject proj,
					fmap inFile file,
					fmap inModule (askOpt "module" as),
					fmap inCabal cabal,
					if hasOpt "source" as then Just byFile else Nothing,
					if hasOpt "standalone" as then Just standalone else Nothing]
				result = return . either (err . ("Unable to find declaration: " ++)) (ResultOk . ResultList . map ResultModuleDeclaration . filter filters)
			case ns of
				[] -> result $ Right $ allDeclarations dbval
				[nm] -> runErrorT (findDeclaration dbval nm) >>= result
				_ -> return $ err "Invalid arguments",
	cmd ["list"] [] "list modules" [projectArg "project to list modules for", sandbox, sourced, standaloned] $ \as _ db -> do
		dbval <- getDb db
		proj <- traverse (getProject db) $ askOpt "project" as
		cabal <- traverse asCabal $ askOpt "cabal" as
		let
			filters = allOf $ catMaybes [
				fmap inProject proj,
				fmap inCabal cabal,
				if hasOpt "source" as then Just byFile else Nothing,
				if hasOpt "standalone" as then Just standalone else Nothing]
		return $ ResultOk $ ResultList $ map (ResultModuleId . moduleId) $ selectModules (filters . moduleId) dbval,
	cmd ["browse"] [] "browse module" [
		moduleArg "module to browse",
		projectArg "project to look module in",
		fileArg "source file to look in",
		sandbox] $ \as _ db -> do
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
			rs <- maybe (return $ Right $ filter (filters . moduleId) $ allModules dbval) (runErrorT . findModule dbval) $ askOpt "module" as
			case rs of
				Left e -> return $ err e
				Right rs' -> case rs' of
					[] -> return $ err "Module not found"
					[m] -> return $ ResultOk $ ResultModule m
					ms' -> return $ errArgs "Ambiguous modules" [("modules", ResultList $ map (ResultModuleId . moduleId) ms')],
	cmd_ ["project"] ["project name"] "show project list or detailed project info" $ \ns db -> case ns of
		[] -> do
			dbval <- getDb db
			return $ ResultOk $ ResultList $ map (ResultString . projectName) $ M.elems $ databaseProjects dbval
		[pname] -> do
			dbval <- getDb db
			case filter ((== pname) . projectName) $ M.elems $ databaseProjects dbval of
				[] -> return $ errArgs "Project not found" [("project", ResultString pname)]
				ps -> return $ ResultOk $ ResultList $ map ResultProject ps
		_ -> return $ err "Invalid arguments",
	cmd ["goto"] ["symbol"] "find symbol declaration" [contextFile] $ \as ns db -> case ns of
		[nm] -> do
			dbval <- getDb db
			liftM (either err (ResultOk . ResultList . map ResultModuleDeclaration)) $ runErrorT $ goToDeclaration dbval (askOpt "file" as) nm
		_ -> return $ err "Invalid arguments",
	cmd ["info"] ["symbol"] "get info for symbol" [contextFile] $ \as ns db -> case ns of
		[nm] -> do
			dbval <- getDb db
			liftM (either err (ResultOk . ResultModuleDeclaration)) $ runErrorT $ symbolInfo dbval (askOpt "file" as) nm
		_ -> return $ err "Invalid arguments",
	cmd_ ["lookup"] ["source file", "symbol"] "find symbol" $ \ns db -> case ns of
		[file, nm] -> do
			dbval <- getDb db
			r <- runErrorT $ liftM (ResultOk . either (ResultList . map ResultModuleDeclaration) (ResultModuleDeclaration)) (lookupSymbol dbval file nm)
			return $ either err id r
		_ -> return $ err "Invalid arguments",
	cmd_ ["complete", "file"] ["source file", "input"] "autocompletion" $ \as db -> do
		dbval <- getDb db
		let
			complete' f i = do
				file' <- canonicalizePath f
				maybe
					(return $ errArgs "File is not scanned" [("file", ResultString f)])
					(\m -> liftM (either err (ResultOk . ResultList . map ResultModuleDeclaration)) (runErrorT (completions dbval m i)))
					(lookupFile file' dbval)
		case as of
			[file] -> complete' file ""
			[file, input] -> complete' file input
			_ -> return $ err "Invalid arguments",
	cmd ["complete", "module"] ["module name", "input"] "autocompletion" [sandbox] $ \p as db -> case as of
		[mname, input] -> do
			dbval <- getDb db
			cabal <- getCabal $ askOpt "cabal" p
			maybe
				(return $ errArgs "Unknown module" [("module", ResultString mname)])
				(\m -> liftM (either err (ResultOk . ResultList . map ResultModuleDeclaration)) (runErrorT (completions dbval m input)))
				(lookupModule (CabalModule cabal Nothing mname) dbval)
		_ -> return $ err "Invalid arguments",
	cmd_ ["dump", "files"] [] "dump file names loaded in database" $ \_ db -> do
		dbval <- getDb db
		return $ ResultOk $ ResultList $ map (ResultModuleId . moduleId) $ selectModules (byFile . moduleId) dbval,
	cmd_ ["dump", "contents"] ["file name"] "dump file contents" $ \as db -> case as of
		[file] -> do
			dbval <- getDb db
			return $ maybe (errArgs "File not found" [("file", ResultString file)]) (ResultOk . ResultModule) $ listToMaybe $ selectModules (inFile file . moduleId) dbval
		_ -> return $ err "Invalid arguments",
	cmd ["cache", "dump", "cabal"] [] "dump cache of cabal modules" [
		sandbox, cacheDir] $ \as _ db -> do
			dbval <- getDb db
			cabal <- getCabal $ askOpt "cabal" as
			dump (pathArg as </> cabalCache cabal) (cabalDB cabal dbval)
			return ok,
	cmd ["cache", "dump", "project"] ["projects..."] "dump cache of project" [cacheDir] $ \as ns db -> do
		dbval <- getDb db
		ps <- if null ns
			then return (M.elems $ databaseProjects dbval)
			else mapM (getProject db) ns
		forM_ ps $ \p -> dump (pathArg as </> projectCache p) (projectDB p dbval)
		return ok,
	cmd ["cache", "dump", "standalone"] [] "dump cache of standalone files" [cacheDir] $ \as _ db -> do
		dbval <- getDb db
		dump (pathArg as </> standaloneCache) (standaloneDB dbval)
		return ok,
	cmd ["cache", "dump"] [] "dump all" [cacheDir] $ \as _ db -> do
		dbval <- getDb db
		createDirectoryIfMissing True (pathArg as </> "cabal")
		createDirectoryIfMissing True (pathArg as </> "projects")
		forM_ (nub $ mapMaybe modCabal $ allModules dbval) $ \c -> dump (pathArg as </> "cabal" </> cabalCache c) (cabalDB c dbval)
		forM_ (M.keys $ databaseProjects dbval) $ \p -> dump (pathArg as </> "projects" </> projectCache (project p)) (projectDB (project p) dbval)
		dump (pathArg as </> standaloneCache) (standaloneDB dbval)
		return ok,
	cmd ["cache", "load", "cabal"] [] "load cache for cabal packages" [
		sandbox, cacheDir, wait] $ \as _ db -> do
			cabal <- getCabal $ askOpt "cabal" as
			cacheLoad db $ load (pathArg as </> cabalCache cabal)
			waitDb as db
			return ok,
	cmd ["cache", "load", "project"] ["projects..."] "load cache for projects" [cacheDir, wait] $ \as ns db -> do
		ps <- mapM getProj ns
		forM_ ps $ \p -> cacheLoad db (load (pathArg as </> projectCache p))
		waitDb as db
		return ok,
	cmd ["cache", "load", "standalone"] [] "load cache for standalone files" [cacheDir, wait] $ \as _ db -> do
		cacheLoad db (load $ pathArg as </> standaloneCache)
		waitDb as db
		return ok,
	cmd ["cache", "load"] [] "load cache from directories" [cacheDir, wait] $ \as _ db -> do
		cts <- liftM concat $ mapM (liftM (filter ((== ".json") . takeExtension)) . getDirectoryContents') [pathArg as, pathArg as </> "cabal", pathArg as </> "projects"]
		forM_ cts $ \c -> do
			e <- doesFileExist c
			when e $ cacheLoad db (load c)
		waitDb as db
		return ok,
	cmd_ ["exit"] [] "exit" $ \_ _ -> return ResultExit]
	where
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

		scanFile_ src stat f = do
			v <- f
			stat $ "file " ++ src ++ " scanned"
			return v
			`catchError`
			\e -> do
				stat $ "error scanning file " ++ src ++ ": " ++ e
				return mempty

		getCabal :: Maybe String -> IO Cabal
		getCabal = liftM (maybe Cabal Sandbox) . traverse canonicalizePath

		getProj :: String -> IO Project
		getProj = fmap project . canonicalizePath

		asCabal "" = return Cabal
		asCabal p = fmap Sandbox $ canonicalizePath p

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

		wait = Option ['w'] ["wait"] (NoArg $ opt "wait" "") "wait for operation to complete"
		status = Option ['s'] ["status"] (NoArg $ opt "status" "") "show status of operation, works only with --wait"
		cacheDir = Option ['p'] ["path"] (ReqArg (opt "path") "path") "cache path"
		sandbox = Option ['c'] ["cabal"] (OptArg (opt "cabal" . fromMaybe "") "path") "path to cabal sandbox"
		sourced = Option [] ["src"] (NoArg $ opt "source" "") "source files"
		standaloned = Option [] ["stand"] (NoArg $ opt "standalone" "") "standalone files"
		projectArg = Option ['p'] ["project"] (ReqArg (opt "project") "path")
		fileArg = Option ['f'] ["file"] (ReqArg (opt "file") "path")
		moduleArg = Option ['m'] ["module"] (ReqArg (opt "module") "name")
		dataArg = Option [] ["data"] (ReqArg (opt "data") "contents") "data to pass to command"
		contextFile = fileArg "context source file"
		packSz = Option ['z'] ["packsize"] (ReqArg (opt "packsize") "packsize") "debug"

		pathArg = fromMaybe "." . askOpt "path"
		packArg a = fromMaybe 50 $ askOpt "packsize" a >>= readMaybe

		ghcOpts = Option ['g'] ["ghcopt"] (ReqArg (opt "ghc") "ghc flags") "flags to pass to GHC"

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
