module HsDev.Scan.Browse (
	-- * List all packages
	browsePackages, browsePackagesDeps,
	-- * Scan cabal modules
	listModules, browseModules,
	-- * Helpers
	withPackages, withPackages_,
	readPackage, readPackageConfig, ghcPackageDb, ghcModuleLocation,
	packageDbCandidate, packageDbCandidate_,
	packageConfigs, packageDbModules, lookupModule_,

	module Control.Monad.Except
	) where

import Control.Arrow
import Control.Monad.Catch (MonadCatch, catch, SomeException)
import Control.Monad.Except
import Data.List (isPrefixOf)
import Data.Maybe
import Data.String (fromString)
import Data.Text (Text)
import Data.Version
import Language.Haskell.Exts.Fixity
import Language.Haskell.Exts.Syntax (Assoc(..), QName(..), Name(Ident), ModuleName(..))
import System.Directory
import System.FilePath
import System.Log.Simple.Monad (MonadLog)

import Data.Deps
import HsDev.PackageDb
import HsDev.Symbols
import HsDev.Error
import HsDev.Tools.Base (inspect)
import HsDev.Tools.Ghc.Worker (GhcM, runGhcM, SessionTarget(..), workerSession)
import HsDev.Tools.Ghc.Compat as Compat

import qualified ConLike as GHC
import qualified DataCon as GHC
import qualified DynFlags as GHC
import qualified GHC
import qualified GHC.PackageDb as GHC
import qualified GhcMonad as GHC (liftIO)
import GhcMonad (GhcMonad)
import qualified GHC.Paths as GHC
import qualified BasicTypes as GHC
import qualified Name as GHC
import qualified IdInfo as GHC
import qualified Outputable as GHC
import qualified Packages as GHC
import qualified PatSyn as GHC
import qualified TyCon as GHC
import qualified Type as GHC
import qualified Var as GHC
import qualified Pretty

-- | Browse packages
browsePackages :: MonadLog m => [String] -> PackageDbStack -> m [PackageConfig]
browsePackages opts dbs = withPackages_ (packageDbStackOpts dbs ++ opts) $
	liftM (map readPackageConfig) packageConfigs

-- | Get packages with deps
browsePackagesDeps :: MonadLog m => [String] -> PackageDbStack -> m (Deps PackageConfig)
browsePackagesDeps opts dbs = withPackages (packageDbStackOpts dbs ++ opts) $ \df -> do
	cfgs <- packageConfigs
	return $ mapDeps (toPkg df) $ mconcat $ map (uncurry deps) $
		map (Compat.unitId &&& Compat.depends df) cfgs
	where
		toPkg df' = readPackageConfig . getPackageDetails df'

listModules :: [String] -> PackageDbStack -> GhcM [ModuleLocation]
listModules opts dbs = do
	workerSession $ SessionGhc (packageDbStackOpts dbs ++ opts)
	ms <- packageDbModules
	return [ghcModuleLocation p m | (p, m) <- ms]

browseModules :: [String] -> PackageDbStack -> [ModuleLocation] -> GhcM [InspectedModule]
browseModules opts dbs mlocs = do
	workerSession $ SessionGhc (packageDbStackOpts dbs ++ opts)
	ms <- packageDbModules
	liftM catMaybes $ sequence [browseModule' p m | (p, m) <- ms, ghcModuleLocation p m `elem` mlocs]
	where
		browseModule' :: GHC.PackageConfig -> GHC.Module -> GhcM (Maybe InspectedModule)
		browseModule' p m = tryT $ inspect (ghcModuleLocation p m) (return $ InspectionAt 0 opts) (browseModule p m)

browseModule :: GHC.PackageConfig -> GHC.Module -> GhcM Module
browseModule package' m = do
	df <- GHC.getSessionDynFlags
	mi <- GHC.getModuleInfo m >>= maybe (hsdevError $ BrowseNoModuleInfo thisModule) return
	ds <- mapM (toDecl df mi) (GHC.modInfoExports mi)
	let
		dirAssoc GHC.InfixL = AssocLeft ()
		dirAssoc GHC.InfixR = AssocRight ()
		dirAssoc GHC.InfixN = AssocNone ()
		fixName o = Qual () (ModuleName () thisModule) (Ident () (GHC.occNameString o))
	return Module {
		_moduleId = mloc df m,
		_moduleDocs = Nothing,
		_moduleExports = ds,
		_moduleFixities = [Fixity (dirAssoc dir) pr (fixName oname) | (oname, GHC.Fixity _ pr dir) <- maybe [] GHC.mi_fixities (GHC.modInfoIface mi)],
		_moduleScope = mempty,
		_moduleSource = Nothing }
	where
		thisModule = GHC.moduleNameString (GHC.moduleName m)
		mloc df m' = ModuleId (fromString mname') $
			ghcModuleLocation (fromMaybe package' $ GHC.lookupPackage df (moduleUnitId m')) m'
			where
				mname' = GHC.moduleNameString $ GHC.moduleName m'
		toDecl df minfo n = do
			tyInfo <- GHC.modInfoLookupName minfo n
			tyResult <- maybe (inModuleSource n) (return . Just) tyInfo
			dflag <- GHC.getSessionDynFlags
			return $ Symbol {
				_symbolId = SymbolId (fromString $ GHC.getOccString n) (mloc df (GHC.nameModule n)),
				_symbolDocs = Nothing,
				_symbolPosition = Nothing,
				_symbolInfo = fromMaybe (Function Nothing) (tyResult >>= showResult dflag) }
		showResult :: GHC.DynFlags -> GHC.TyThing -> Maybe SymbolInfo
		showResult dflags (GHC.AnId i) = case GHC.idDetails i of
			GHC.RecSelId p _ -> Just $ Selector (Just $ formatType dflags $ GHC.varType i) parent ctors where
				parent = fromString $ case p of
					GHC.RecSelData p' -> GHC.getOccString p'
					GHC.RecSelPatSyn p' -> GHC.getOccString p'
				ctors = map fromString $ case p of
					GHC.RecSelData p' -> map GHC.getOccString (GHC.tyConDataCons p')
					GHC.RecSelPatSyn p' -> [GHC.getOccString p']
			GHC.ClassOpId cls -> Just $ Method (Just $ formatType dflags $ GHC.varType i) (fromString $ GHC.getOccString cls)
			_ -> Just $ Function (Just $ formatType dflags $GHC.varType i)
		showResult dflags (GHC.AConLike c) = case c of
			GHC.RealDataCon d -> Just $ Constructor
				(map (formatType dflags) $ GHC.dataConOrigArgTys d)
				(fromString $ GHC.getOccString (GHC.dataConTyCon d))
			GHC.PatSynCon p -> Just $ PatConstructor
				(map (formatType dflags) $ GHC.patSynArgs p)
				Nothing
			-- TODO: Deal with `patSynFieldLabels` and `patSynFieldType`
		showResult dflags (GHC.ATyCon t)
			| GHC.isTypeSynonymTyCon t = Just $ Type args ctx
			| GHC.isNewTyCon t = Just $ NewType args ctx
			| GHC.isDataTyCon t = Just $ Data args ctx
			| GHC.isClassTyCon t = Just $ Class args ctx
			| GHC.isTypeFamilyTyCon t = Just $ TypeFam args ctx Nothing
			| GHC.isDataFamilyTyCon t = Just $ DataFam args ctx Nothing
			| otherwise = Nothing
			where
				args = map (formatType dflags . GHC.mkTyVarTy) $ GHC.tyConTyVars t
				ctx = case GHC.tyConClass_maybe t of
					Nothing -> []
					Just cls -> map (formatType dflags) $ GHC.classSCTheta cls
		showResult _ _ = Nothing

withInitializedPackages :: MonadLog m => [String] -> (GHC.DynFlags -> GhcM a) -> m a
withInitializedPackages ghcOpts cont = runGhcM (Just GHC.libdir) $ do
	workerSession $ SessionGhc ghcOpts
	fs <- GHC.getSessionDynFlags
	cleanupHandler fs $ do
		(fs', _, _) <- GHC.parseDynamicFlags fs (map GHC.noLoc ghcOpts)
		_ <- GHC.setSessionDynFlags fs'
		(result, _) <- GHC.liftIO $ GHC.initPackages fs'
		cont result

withPackages :: MonadLog m => [String] -> (GHC.DynFlags -> GhcM a) -> m a
withPackages = withInitializedPackages

withPackages_ :: MonadLog m => [String] -> GhcM a -> m a
withPackages_ ghcOpts act = withPackages ghcOpts (const act)

inModuleSource :: GhcMonad m => GHC.Name -> m (Maybe GHC.TyThing)
inModuleSource nm = GHC.getModuleInfo (GHC.nameModule nm) >> GHC.lookupGlobalName nm

formatType :: GHC.DynFlags -> GHC.Type -> Text
formatType dflag t = fromString $ showOutputable dflag (removeForAlls t)

removeForAlls :: GHC.Type -> GHC.Type
removeForAlls ty = removeForAlls' ty' tty' where
	ty'  = GHC.dropForAlls ty
	tty' = GHC.splitFunTy_maybe ty'

removeForAlls' :: GHC.Type -> Maybe (GHC.Type, GHC.Type) -> GHC.Type
removeForAlls' ty Nothing = ty
removeForAlls' ty (Just (pre, ftype))
	| GHC.isPredTy pre = GHC.mkFunTy pre (GHC.dropForAlls ftype)
	| otherwise = ty

showOutputable :: GHC.Outputable a => GHC.DynFlags -> a -> String
showOutputable dflag = unwords . lines . showUnqualifiedPage dflag . GHC.ppr

showUnqualifiedPage :: GHC.DynFlags -> GHC.SDoc -> String
showUnqualifiedPage dflag = renderStyle Pretty.LeftMode 0 . GHC.withPprStyleDoc dflag styleUnqualified

styleUnqualified :: GHC.PprStyle
styleUnqualified = GHC.mkUserStyle GHC.neverQualify GHC.AllTheWay

tryT :: MonadCatch m => m a -> m (Maybe a)
tryT act = catch (fmap Just act) (const (return Nothing) . (id :: SomeException -> SomeException))

readPackage :: GHC.PackageConfig -> ModulePackage
readPackage pc = ModulePackage (GHC.packageNameString pc) (showVersion (GHC.packageVersion pc))

readPackageConfig :: GHC.PackageConfig -> PackageConfig
readPackageConfig pc = PackageConfig
	(readPackage pc)
	(map (fromString . GHC.moduleNameString . GHC.exposedName) $ GHC.exposedModules pc)
	(GHC.exposed pc)

ghcModuleLocation :: GHC.PackageConfig -> GHC.Module -> ModuleLocation
ghcModuleLocation p m = InstalledModule (GHC.libraryDirs p) (Just $ readPackage p) (GHC.moduleNameString $ GHC.moduleName m)

ghcPackageDb :: GHC.PackageConfig -> IO PackageDb
ghcPackageDb = maybe (return GlobalDb) packageDbCandidate_ . listToMaybe . GHC.libraryDirs

-- | Get package-db for package library directory
-- Haskish way
-- global-db - library is in <path>/lib/<package> and there exists <path>/lib/package.conf.d
-- user-db - library is in cabal user directory
-- package-db
--   cabal-sandbox - library in .../.cabal-sandbox/.../<platform-ghc-ver>/<package>
--     then package-db is .../.cabal-sandbox/<platform-ghc-ver>-package.conf.d
--   stack (snapshots or .stack-work) - library in <path>/lib/<platform-ghc-ver>/<package>
--     then package-db is <path>/pkgdb
packageDbCandidate :: FilePath -> IO (Maybe PackageDb)
packageDbCandidate fpath = liftM Just (msum [global', user', sandbox', stack']) `mplus` return Nothing where
	global' = do
		guard (takeFileName (takeDirectory fpath') == "lib")
		guardExist (takeDirectory fpath' </> "package.conf.d")
		return GlobalDb
	user' = do
		cabalDir <- getAppUserDataDirectory "cabal"
		guard (splitDirectories cabalDir `isPrefixOf` splitDirectories fpath')
		return UserDb
	sandbox' = do
		guard (".cabal-sandbox" `elem` splitDirectories fpath')
		let
			sandboxPath = joinPath $ reverse $ dropWhile (/= ".cabal-sandbox") $ reverse $ splitDirectories fpath'
			platform = takeFileName (takeDirectory fpath')
			dbPath = sandboxPath </> (platform ++ "-packages.conf.d")
		guardExist dbPath
		return $ PackageDb dbPath
	stack' = do
		guard (takeFileName (takeDirectory (takeDirectory fpath')) == "lib")
		let
			pkgDb = takeDirectory (takeDirectory $ takeDirectory fpath') </> "pkgdb"
		guardExist pkgDb
		return $ PackageDb pkgDb
	guardExist = doesDirectoryExist >=> guard
	fpath' = normalise fpath

-- | Use global as default
packageDbCandidate_ :: FilePath -> IO PackageDb
packageDbCandidate_ = packageDbCandidate >=> maybe (return GlobalDb) return

packageConfigs :: GhcM [GHC.PackageConfig]
packageConfigs = liftM (fromMaybe [] . pkgDatabase) GHC.getSessionDynFlags

packageDbModules :: GhcM [(GHC.PackageConfig, GHC.Module)]
packageDbModules = do
	pkgs <- packageConfigs
	dflags <- GHC.getSessionDynFlags
	return [(p, m) |
		p <- pkgs,
		mn <- map GHC.exposedName (GHC.exposedModules p),
		m <- lookupModule_ dflags mn]

-- Lookup module everywhere
lookupModule_ :: GHC.DynFlags -> GHC.ModuleName -> [GHC.Module]
lookupModule_ d mn = case GHC.lookupModuleWithSuggestions d mn Nothing of
	GHC.LookupFound m' _ -> [m']
	GHC.LookupMultiple ms -> map fst ms
	GHC.LookupHidden ls rs -> map fst $ ls ++ rs
	GHC.LookupNotFound _ -> []
