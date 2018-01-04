module HsDev.Scan.Browse (
	-- * List all packages
	browsePackages, browsePackagesDeps,
	-- * Scan cabal modules
	listModules, browseModules, browseModules',
	-- * Helpers
	readPackage, readPackageConfig, ghcModuleLocation,
	packageDbCandidate, packageDbCandidate_,
	packageConfigs, packageDbModules, lookupModule_,
	modulesPackages, modulesPackagesGroups,

	module Control.Monad.Except
	) where

import Control.Arrow
import Control.Lens (preview)
import Control.Monad.Catch (MonadCatch, catch, SomeException)
import Control.Monad.Except
import Data.Function (on)
import Data.List (isPrefixOf, groupBy, sort)
import Data.Maybe
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Set as S
import Data.Version
import Language.Haskell.Exts.Fixity
import Language.Haskell.Exts.Syntax (Assoc(..), QName(..), Name(Ident), ModuleName(..))
import System.Directory
import System.FilePath

import Data.Deps
import Data.LookupTable
import HsDev.PackageDb
import HsDev.Symbols
import HsDev.Error
import HsDev.Tools.Base (inspect)
import HsDev.Tools.Ghc.Worker (GhcM, tmpSession)
import HsDev.Tools.Ghc.Compat as Compat
import HsDev.Util (ordNub)
import System.Directory.Paths

import qualified ConLike as GHC
import qualified DataCon as GHC
import qualified DynFlags as GHC
import qualified GHC
import qualified GHC.PackageDb as GHC
import qualified GhcMonad as GHC (liftIO)
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
browsePackages :: [String] -> PackageDbStack -> GhcM [PackageConfig]
browsePackages opts dbs = do
	tmpSession (packageDbStackOpts dbs ++ opts)
	liftM (map readPackageConfig) packageConfigs

-- | Get packages with deps
browsePackagesDeps :: [String] -> PackageDbStack -> GhcM (Deps PackageConfig)
browsePackagesDeps opts dbs = do
	tmpSession (packageDbStackOpts dbs ++ opts)
	df <- GHC.getSessionDynFlags
	cfgs <- packageConfigs
	return $ mapDeps (toPkg df) $ mconcat $ map (uncurry deps) $
		map (Compat.unitId &&& Compat.depends df) cfgs
	where
		toPkg df' = readPackageConfig . getPackageDetails df'

-- | List modules from ghc, accepts ghc-opts, stack of package-db to get modules for
-- and list of packages to explicitely expose them with '-package' flag,
-- otherwise hidden packages won't be loaded
listModules :: [String] -> PackageDbStack -> [ModulePackage] -> GhcM [ModuleLocation]
listModules opts dbs pkgs = do
	tmpSession (packageDbStackOpts dbs ++ opts ++ packagesOpts)
	ms <- packageDbModules
	return [ghcModuleLocation p m | (p, m) <- ms]
	where
		packagesOpts = ["-package " ++ show p | p <- pkgs]

-- | Like @browseModules@, but groups modules by package and inspects each package separately
-- Trying to fix error: when there are several same packages (of different version), only @Module@ from
-- one of them can be lookuped and therefore modules from different version packages won't be actually inspected
browseModules :: [String] -> PackageDbStack -> [ModuleLocation] -> GhcM [InspectedModule]
browseModules opts dbs mlocs = do
	tmpSession (packageDbStackOpts dbs ++ opts)
	liftM concat . mapM (browseModules' opts) . map snd . modulesPackagesGroups $ mlocs

-- | Inspect installed modules, doesn't set session!
browseModules' :: [String] -> [ModuleLocation] -> GhcM [InspectedModule]
browseModules' opts mlocs = do
	-- we set package flags separately in order not to drop previous temporary session with consecutive call to this function
	setPackagesOpts

	ms <- packageDbModules
	midTbl <- newLookupTable
	sidTbl <- newLookupTable
	let
		lookupModuleId p' m' = lookupTable (ghcModuleLocation p' m') (ghcModuleId p' m') midTbl
	liftM catMaybes $ sequence [browseModule' lookupModuleId (cacheInTableM sidTbl) p m | (p, m) <- ms, ghcModuleLocation p m `S.member` mlocs']
	where
		browseModule' :: (GHC.PackageConfig -> GHC.Module -> GhcM ModuleId) -> (GHC.Name -> GhcM Symbol -> GhcM Symbol) -> GHC.PackageConfig -> GHC.Module -> GhcM (Maybe InspectedModule)
		browseModule' modId' sym' p m = tryT $ inspect (ghcModuleLocation p m) (return $ InspectionAt 0 (map fromString opts)) (browseModule modId' sym' p m)
		mlocs' = S.fromList mlocs
		packagesOpts = "-hide-all-packages" : ["-package " ++ show p | p <- modulesPackages mlocs]
		setPackagesOpts = void $ do
			fs <- GHC.getSessionDynFlags
			(fs', _, _) <- GHC.parseDynamicFlags (fs { GHC.packageFlags = [] }) (map GHC.noLoc packagesOpts)
			(fs'', _) <- GHC.liftIO $ GHC.initPackages fs'
			GHC.setSessionDynFlags fs''

browseModule :: (GHC.PackageConfig -> GHC.Module -> GhcM ModuleId) -> (GHC.Name -> GhcM Symbol -> GhcM Symbol) -> GHC.PackageConfig -> GHC.Module -> GhcM Module
browseModule modId lookSym package' m = do
	df <- GHC.getSessionDynFlags
	mi <- GHC.getModuleInfo m >>= maybe (hsdevError $ BrowseNoModuleInfo thisModule) return
	ds <- mapM (\n -> lookSym n (toDecl df mi n)) (GHC.modInfoExports mi)
	myModId <- modId package' m
	let
		dirAssoc GHC.InfixL = AssocLeft ()
		dirAssoc GHC.InfixR = AssocRight ()
		dirAssoc GHC.InfixN = AssocNone ()
		fixName o = Qual () (ModuleName () thisModule) (Ident () (GHC.occNameString o))
	return Module {
		_moduleId = myModId,
		_moduleDocs = Nothing,
		_moduleImports = [],
		_moduleExports = ds,
		_moduleFixities = [Fixity (dirAssoc dir) pr (fixName oname) | (oname, (pr, dir)) <- map (second Compat.getFixity) (maybe [] GHC.mi_fixities (GHC.modInfoIface mi))],
		_moduleScope = mempty,
		_moduleSource = Nothing }
	where
		thisModule = GHC.moduleNameString (GHC.moduleName m)
		mloc df m' = do
			pkg' <- maybe (hsdevError $ OtherError $ "Error getting module package: " ++ GHC.moduleNameString (GHC.moduleName m')) return $
				GHC.lookupPackage df (moduleUnitId m')
			modId pkg' m'
		toDecl df minfo n = do
			tyInfo <- GHC.modInfoLookupName minfo n
			tyResult <- maybe (GHC.lookupName n) (return . Just) tyInfo
			dflag <- GHC.getSessionDynFlags
			declModId <- mloc df (GHC.nameModule n)
			return $ Symbol {
				_symbolId = SymbolId (fromString $ GHC.getOccString n) declModId,
				_symbolDocs = Nothing,
				_symbolPosition = Nothing,
				_symbolInfo = fromMaybe (Function Nothing) (tyResult >>= showResult dflag) }
		showResult :: GHC.DynFlags -> GHC.TyThing -> Maybe SymbolInfo
		showResult dflags (GHC.AnId i) = case GHC.idDetails i of
			GHC.RecSelId p _ -> Just $ Selector (Just $ formatType dflags $ GHC.varType i) parent ctors where
				parent = fromString $ Compat.recSelParent p
				ctors = map fromString $ Compat.recSelCtors p
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
			| GHC.isPrimTyCon t = Just $ Type [] []
			| GHC.isNewTyCon t = Just $ NewType args ctx
			| GHC.isDataTyCon t = Just $ Data args ctx
			| GHC.isClassTyCon t = Just $ Class args ctx
			| GHC.isTypeFamilyTyCon t = Just $ TypeFam args ctx Nothing
			| GHC.isDataFamilyTyCon t = Just $ DataFam args ctx Nothing
			| otherwise = Just $ Type [] []
			where
				args = map (formatType dflags . GHC.mkTyVarTy) $ GHC.tyConTyVars t
				ctx = case GHC.tyConClass_maybe t of
					Nothing -> []
					Just cls -> map (formatType dflags) $ GHC.classSCTheta cls
		showResult _ _ = Nothing

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
showUnqualifiedPage dflag = renderStyle Pretty.LeftMode 0 . GHC.withPprStyleDoc dflag (Compat.unqualStyle dflag)

tryT :: MonadCatch m => m a -> m (Maybe a)
tryT act = catch (fmap Just act) (const (return Nothing) . (id :: SomeException -> SomeException))

readPackage :: GHC.PackageConfig -> ModulePackage
readPackage pc = ModulePackage (fromString $ GHC.packageNameString pc) (fromString $ showVersion (GHC.packageVersion pc))

readPackageConfig :: GHC.PackageConfig -> PackageConfig
readPackageConfig pc = PackageConfig
	(readPackage pc)
	(map (fromString . GHC.moduleNameString . Compat.exposedModuleName) $ GHC.exposedModules pc)
	(GHC.exposed pc)

ghcModuleLocation :: GHC.PackageConfig -> GHC.Module -> ModuleLocation
ghcModuleLocation p m = InstalledModule (map fromString $ GHC.libraryDirs p) (readPackage p) (fromString $ GHC.moduleNameString $ GHC.moduleName m)

ghcModuleId :: GHC.PackageConfig -> GHC.Module -> ModuleId
ghcModuleId p m = ModuleId (fromString mname') (ghcModuleLocation p m) where
	mname' = GHC.moduleNameString $ GHC.moduleName m

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
		return $ PackageDb $ fromFilePath dbPath
	stack' = do
		guard (takeFileName (takeDirectory (takeDirectory fpath')) == "lib")
		let
			pkgDb = takeDirectory (takeDirectory $ takeDirectory fpath') </> "pkgdb"
		guardExist pkgDb
		return $ PackageDb $ fromFilePath pkgDb
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
		mn <- map Compat.exposedModuleName (GHC.exposedModules p),
		m <- lookupModule_ dflags mn]

-- Lookup module everywhere
lookupModule_ :: GHC.DynFlags -> GHC.ModuleName -> [GHC.Module]
lookupModule_ d mn = case GHC.lookupModuleWithSuggestions d mn Nothing of
	GHC.LookupFound m' _ -> [m']
	GHC.LookupMultiple ms -> map fst ms
	GHC.LookupHidden ls rs -> map fst $ ls ++ rs
	GHC.LookupNotFound _ -> []

-- | Get modules packages
modulesPackages :: [ModuleLocation] -> [ModulePackage]
modulesPackages = ordNub . mapMaybe (preview modulePackage)

-- | Group modules by packages
modulesPackagesGroups :: [ModuleLocation] -> [(ModulePackage, [ModuleLocation])]
modulesPackagesGroups = map (first head . unzip) . groupBy ((==) `on` fst) . sort . mapMaybe (\m -> (,) <$> preview modulePackage m <*> pure m)
