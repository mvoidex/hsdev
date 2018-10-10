module HsDev.Scan.Browse (
	-- * List all packages
	browsePackages, browsePackagesDeps,
	-- * Scan cabal modules
	listModules,
	browseModules, browseModules',
	-- * Helpers
	uniqueModuleLocations,
	readPackage, readPackageConfig, ghcModuleLocation,
	packageConfigs, packageDbModules, lookupModule_,
	modulesPackages, modulesPackagesGroups, withEachPackage,

	module Control.Monad.Except
	) where

import Control.Arrow
import Control.Lens (preview)
import Control.Monad.Catch (MonadCatch, catch, SomeException)
import Control.Monad.Except
import Data.Function (on)
import Data.List (groupBy, sort)
import Data.Maybe
import Data.String (fromString)
import qualified Data.Set as S
import Data.Version
import Language.Haskell.Exts.Fixity
import Language.Haskell.Exts.Syntax (Assoc(..), QName(..), Name(Ident), ModuleName(..))

import Data.Deps
import Data.LookupTable
import HsDev.PackageDb
import HsDev.Symbols
import HsDev.Error
import HsDev.Tools.Ghc.Worker (GhcM, tmpSession, formatType)
import HsDev.Tools.Ghc.Compat as Compat
import HsDev.Util (ordNub, uniqueBy)
import System.Directory.Paths (fromFilePath, normalize)

import qualified ConLike as GHC
import qualified DataCon as GHC
import qualified DynFlags as GHC
import qualified GHC
import qualified GHC.PackageDb as GHC
import qualified GhcMonad as GHC (liftIO)
import qualified Name as GHC
import qualified IdInfo as GHC
import qualified Packages as GHC
import qualified PatSyn as GHC
import qualified TyCon as GHC
import qualified Type as GHC
import qualified Var as GHC

-- | Browse packages
browsePackages :: [String] -> PackageDbStack -> GhcM [PackageConfig]
browsePackages opts dbs = do
	tmpSession dbs opts
	liftM (map readPackageConfig) packageConfigs

-- | Get packages with deps
browsePackagesDeps :: [String] -> PackageDbStack -> GhcM (Deps PackageConfig)
browsePackagesDeps opts dbs = do
	tmpSession dbs opts
	df <- GHC.getSessionDynFlags
	cfgs <- packageConfigs
	return $ mapDeps (toPkg df) $ mconcat [deps (Compat.unitId cfg) (Compat.depends df cfg) | cfg <- cfgs]
	where
		toPkg df' = readPackageConfig . getPackageDetails df'

-- | List modules from ghc, accepts ghc-opts, stack of package-db to get modules for
-- and list of packages to explicitely expose them with '-package' flag,
-- otherwise hidden packages won't be loaded
listModules :: [String] -> PackageDbStack -> [ModulePackage] -> GhcM [ModuleLocation]
listModules opts dbs pkgs = do
	tmpSession dbs (opts ++ packagesOpts)
	ms <- packageDbModules
	return $ ordNub [ghcModuleLocation p m e | (p, m, e) <- ms]
	where
		packagesOpts = ["-package " ++ show p | p <- pkgs]

-- | Like @browseModules@, but groups modules by package and inspects each package separately
-- Trying to fix error: when there are several same packages (of different version), only @Module@ from
-- one of them can be lookuped and therefore modules from different version packages won't be actually inspected
browseModules :: [String] -> PackageDbStack -> [ModuleLocation] -> GhcM [InspectedModule]
browseModules opts dbs mlocs = do
	tmpSession dbs opts
	liftM concat . withEachPackage (const $ browseModules' opts) $ ordNub mlocs

-- | Inspect installed modules, doesn't set session and package flags!
browseModules' :: [String] -> [ModuleLocation] -> GhcM [InspectedModule]
browseModules' opts mlocs = do
	ms <- packageDbModules
	midTbl <- newLookupTable
	sidTbl <- newLookupTable
	let
		lookupModuleId p' m' e' = lookupTable (ghcModuleLocation p' m' e') (ghcModuleId p' m' e') midTbl
	liftM catMaybes $ sequence [browseModule' lookupModuleId (cacheInTableM sidTbl) p m e | (p, m, e) <- ms, ghcModuleLocation p m e `S.member` mlocs']
	where
		browseModule' :: (GHC.PackageConfig -> GHC.Module -> Bool -> GhcM ModuleId) -> (GHC.Name -> GhcM Symbol -> GhcM Symbol) -> GHC.PackageConfig -> GHC.Module -> Bool -> GhcM (Maybe InspectedModule)
		browseModule' modId' sym' p m e = tryT $ runInspect (ghcModuleLocation p m e) $ inspect_ (return $ InspectionAt 0 (map fromString opts)) (browseModule modId' sym' p m e)
		mlocs' = S.fromList mlocs

browseModule :: (GHC.PackageConfig -> GHC.Module -> Bool -> GhcM ModuleId) -> (GHC.Name -> GhcM Symbol -> GhcM Symbol) -> GHC.PackageConfig -> GHC.Module -> Bool -> GhcM Module
browseModule modId lookSym package' m exposed' = do
	df <- GHC.getSessionDynFlags
	mi <- GHC.getModuleInfo m >>= maybe (hsdevError $ BrowseNoModuleInfo thisModule) return
	ds <- mapM (\n -> lookSym n (toDecl df mi n)) (GHC.modInfoExports mi)
	myModId <- modId package' m exposed'
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
			modId pkg' m' (GHC.moduleName m `notElem` GHC.hiddenModules pkg')
		toDecl df minfo n = do
			tyInfo <- GHC.modInfoLookupName minfo n
			tyResult <- maybe (GHC.lookupName n) (return . Just) tyInfo
			declModId <- mloc df (GHC.nameModule n)
			return Symbol {
				_symbolId = SymbolId (fromString $ GHC.getOccString n) declModId,
				_symbolDocs = Nothing,
				_symbolPosition = Nothing,
				_symbolInfo = fromMaybe (Function Nothing) (tyResult >>= showResult df) }
		showResult :: GHC.DynFlags -> GHC.TyThing -> Maybe SymbolInfo
		showResult dflags (GHC.AnId i) = case GHC.idDetails i of
			GHC.RecSelId p _ -> Just $ Selector (Just $ fromString $ formatType dflags $ GHC.varType i) parent ctors where
				parent = fromString $ Compat.recSelParent p
				ctors = map fromString $ Compat.recSelCtors p
			GHC.ClassOpId cls -> Just $ Method (Just $ fromString $ formatType dflags $ GHC.varType i) (fromString $ GHC.getOccString cls)
			_ -> Just $ Function (Just $ fromString $ formatType dflags $GHC.varType i)
		showResult dflags (GHC.AConLike c) = case c of
			GHC.RealDataCon d -> Just $ Constructor
				(map (fromString . formatType dflags) $ GHC.dataConOrigArgTys d)
				(fromString $ GHC.getOccString (GHC.dataConTyCon d))
			GHC.PatSynCon p -> Just $ PatConstructor
				(map (fromString . formatType dflags) $ GHC.patSynArgs p)
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
				args = map (fromString . formatType dflags . GHC.mkTyVarTy) $ GHC.tyConTyVars t
				ctx = case GHC.tyConClass_maybe t of
					Nothing -> []
					Just cls -> map (fromString . formatType dflags) $ GHC.classSCTheta cls
		showResult _ _ = Nothing

tryT :: MonadCatch m => m a -> m (Maybe a)
tryT act = catch (fmap Just act) (const (return Nothing) . (id :: SomeException -> SomeException))

-- | There can be same modules (same package name, version and module name) installed in different locations
-- Select first one of such modules
uniqueModuleLocations :: [ModuleLocation] -> [ModuleLocation]
uniqueModuleLocations = uniqueBy nameId' where
	nameId' mloc = (,) <$> (preview modulePackage mloc) <*> (preview installedModuleName mloc)

readPackage :: GHC.PackageConfig -> ModulePackage
readPackage pc = ModulePackage (fromString $ GHC.packageNameString pc) (fromString $ showVersion (GHC.packageVersion pc))

readPackageConfig :: GHC.PackageConfig -> PackageConfig
readPackageConfig pc = PackageConfig
	(readPackage pc)
	(map (fromString . GHC.moduleNameString . Compat.exposedModuleName) $ GHC.exposedModules pc)
	(GHC.exposed pc)

ghcModuleLocation :: GHC.PackageConfig -> GHC.Module -> Bool -> ModuleLocation
ghcModuleLocation p m = InstalledModule (map (normalize . fromFilePath) $ GHC.libraryDirs p) (readPackage p) (fromString $ GHC.moduleNameString $ GHC.moduleName m)

ghcModuleId :: GHC.PackageConfig -> GHC.Module -> Bool -> ModuleId
ghcModuleId p m e = ModuleId (fromString mname') (ghcModuleLocation p m e) where
	mname' = GHC.moduleNameString $ GHC.moduleName m

packageConfigs :: GhcM [GHC.PackageConfig]
packageConfigs = liftM (fromMaybe [] . pkgDatabase) GHC.getSessionDynFlags

packageDbModules :: GhcM [(GHC.PackageConfig, GHC.Module, Bool)]
packageDbModules = do
	pkgs <- packageConfigs
	dflags <- GHC.getSessionDynFlags
	return [(p, m, exposed') |
		p <- pkgs,
		(mn, exposed') <- zip (map Compat.exposedModuleName (GHC.exposedModules p)) (repeat True) ++ zip (GHC.hiddenModules p) (repeat False),
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

-- | Run action for each package with prepared '-package' flags
withEachPackage :: (ModulePackage -> [ModuleLocation] -> GhcM a) -> [ModuleLocation] -> GhcM [a]
withEachPackage act = mapM (uncurry act') . modulesPackagesGroups where
	act' mpkg mlocs = setPackagesOpts >> act mpkg mlocs where
		packagesOpts = "-hide-all-packages" : ["-package " ++ show p | p <- modulesPackages mlocs]
		setPackagesOpts = void $ do
			fs <- GHC.getSessionDynFlags
			(fs', _, _) <- GHC.parseDynamicFlags (fs { GHC.packageFlags = [] }) (map GHC.noLoc packagesOpts)
			(fs'', _) <- GHC.liftIO $ GHC.initPackages fs'
			GHC.setSessionDynFlags fs''
