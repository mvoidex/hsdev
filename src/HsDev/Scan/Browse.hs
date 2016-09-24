module HsDev.Scan.Browse (
	-- * List all packages
	browsePackages, browsePackagesDeps,
	-- * Scan cabal modules
	listModules, browseModules, browse, browseDb,
	-- * Helpers
	withPackages, withPackages_,
	readPackage, readPackageConfig, ghcPackageDb, ghcModuleLocation,
	packageDbCandidate, packageDbCandidate_,
	packageConfigs, packageDbModules, lookupModule_,

	module Control.Monad.Except
	) where

import Control.Arrow
import Control.Lens (view, preview, _Just)
import Control.Monad.Catch (MonadCatch, catch, SomeException)
import Control.Monad.Except
import Data.List (isPrefixOf)
import Data.Maybe
import Data.String (fromString)
import Data.Version
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
import HsDev.Util (ordNub)

import qualified ConLike as GHC
import qualified DataCon as GHC
import qualified DynFlags as GHC
import qualified GHC
import qualified GHC.PackageDb as GHC
import qualified GhcMonad as GHC (liftIO)
import GhcMonad (GhcMonad)
import qualified GHC.Paths as GHC
import qualified Name as GHC
import qualified Outputable as GHC
import qualified Packages as GHC
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

listModules :: MonadLog m => [String] -> PackageDbStack -> m [ModuleLocation]
listModules opts dbs = withPackages_ (packageDbStackOpts dbs ++ opts) $ do
	ms <- packageDbModules
	pdbs <- mapM (liftIO . ghcPackageDb . fst) ms
	return [ghcModuleLocation pdb p m | (pdb, (p, m)) <- zip pdbs ms]

browseModules :: MonadLog m => [String] -> PackageDbStack -> [ModuleLocation] -> m [InspectedModule]
browseModules opts dbs mlocs = withPackages_ (packageDbStackOpts dbs ++ opts) $ do
	ms <- packageDbModules
	pdbs <- mapM (liftIO . ghcPackageDb . fst) ms
	liftM catMaybes $ sequence [browseModule' pdb p m | (pdb, (p, m)) <- zip pdbs ms, ghcModuleLocation pdb p m `elem` mlocs]
	where
		browseModule' :: PackageDb -> GHC.PackageConfig -> GHC.Module -> GhcM (Maybe InspectedModule)
		browseModule' pdb p m = tryT $ inspect (ghcModuleLocation pdb p m) (return $ InspectionAt 0 opts) (browseModule pdb p m)

-- | Browse modules, if third argument is True - browse only modules in top of package-db stack
browse :: MonadLog m => [String] -> PackageDbStack -> m [InspectedModule]
browse opts dbs = listModules opts dbs >>= browseModules opts dbs

-- | Browse modules in top of package-db stack
browseDb :: MonadLog m => [String] -> PackageDbStack -> m [InspectedModule]
browseDb opts dbs = listModules opts dbs >>= browseModules opts dbs . filter inTop where
	inTop = (== Just (topPackageDb dbs)) . preview modulePackageDb

browseModule :: PackageDb -> GHC.PackageConfig -> GHC.Module -> GhcM Module
browseModule pdb package' m = do
	df <- GHC.getSessionDynFlags
	mi <- GHC.getModuleInfo m >>= maybe (hsdevError $ BrowseNoModuleInfo thisModule) return
	ds <- mapM (toDecl df mi) (GHC.modInfoExports mi)
	return Module {
		_moduleName = fromString thisModule,
		_moduleDocs = Nothing,
		_moduleLocation = thisLoc df,
		_moduleExports = Just [ExportName Nothing (view declarationName d) ThingNothing | d <- ds],
		_moduleImports = [import_ iname | iname <- ordNub (mapMaybe (preview definedModule) ds), iname /= fromString thisModule],
		_moduleDeclarations = sortDeclarations ds }
	where
		thisModule = GHC.moduleNameString (GHC.moduleName m)
		thisLoc df = view moduleIdLocation $ mloc df m
		mloc df m' = ModuleId (fromString mname') $
			ghcModuleLocation pdb (fromMaybe package' $ GHC.lookupPackage df (GHC.moduleUnitId m')) m'
			where
				mname' = GHC.moduleNameString $ GHC.moduleName m'
		toDecl df minfo n = do
			tyInfo <- GHC.modInfoLookupName minfo n
			tyResult <- maybe (inModuleSource n) (return . Just) tyInfo
			dflag <- GHC.getSessionDynFlags
			let
				decl' = decl (fromString $ GHC.getOccString n) $ fromMaybe
					(Function Nothing [] Nothing)
					(tyResult >>= showResult dflag)
			return $ decl' `definedIn` mloc df (GHC.nameModule n)
		definedModule = declarationDefined . _Just . moduleIdName
		showResult :: GHC.DynFlags -> GHC.TyThing -> Maybe DeclarationInfo
		showResult dflags (GHC.AnId i) = Just $ Function (Just $ fromString $ formatType dflags GHC.varType i) [] Nothing
		showResult dflags (GHC.AConLike c) = case c of
			GHC.RealDataCon d -> Just $ Function (Just $ fromString $ formatType dflags GHC.dataConRepType d) [] Nothing
			GHC.PatSynCon p -> Just $ Function (Just $ fromString $ formatType dflags patSynType p) [] Nothing
		showResult _ (GHC.ATyCon t) = Just $ tcon $ TypeInfo Nothing (map (fromString . GHC.getOccString) $ GHC.tyConTyVars t) Nothing [] where
			tcon
				| GHC.isAlgTyCon t && not (GHC.isNewTyCon t) && not (GHC.isClassTyCon t) = Data
				| GHC.isNewTyCon t = NewType
				| GHC.isClassTyCon t = Class
				| GHC.isTypeSynonymTyCon t = Type
				| otherwise = Type
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
withPackages ghcOpts cont = withInitializedPackages ghcOpts cont

withPackages_ :: MonadLog m => [String] -> GhcM a -> m a
withPackages_ ghcOpts act = withPackages ghcOpts (const act)

inModuleSource :: GhcMonad m => GHC.Name -> m (Maybe GHC.TyThing)
inModuleSource nm = GHC.getModuleInfo (GHC.nameModule nm) >> GHC.lookupGlobalName nm

formatType :: GHC.DynFlags -> (a -> GHC.Type) -> a -> String
formatType dflag f x = showOutputable dflag (removeForAlls $ f x)

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
tryT act = catch (liftM Just act) (const (return Nothing) . (id :: SomeException -> SomeException))

readPackage :: GHC.PackageConfig -> ModulePackage
readPackage pc = ModulePackage (GHC.packageNameString pc) (showVersion (GHC.packageVersion pc))

readPackageConfig :: GHC.PackageConfig -> PackageConfig
readPackageConfig pc = PackageConfig
	(readPackage pc)
	(map (fromString . GHC.moduleNameString . GHC.exposedName) $ GHC.exposedModules pc)
	(GHC.exposed pc)

ghcModuleLocation :: PackageDb -> GHC.PackageConfig -> GHC.Module -> ModuleLocation
ghcModuleLocation pdb p m = InstalledModule pdb (Just $ readPackage p) (GHC.moduleNameString $ GHC.moduleName m)

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
