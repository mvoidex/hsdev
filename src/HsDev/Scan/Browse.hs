module HsDev.Scan.Browse (
	-- * List all packages
	browsePackages,
	-- * Scan cabal modules
	listModules, browseModules, browse,
	-- * Helpers
	withPackages, withPackages_, packageDbModules, lookupModule_,

	module Control.Monad.Except
	) where

import Control.Lens (view, preview, _Just)
import Control.Monad.Except
import Data.List (sortBy, intersect)
import Data.Maybe
import Data.String (fromString)
import System.FilePath
import Text.Read (readMaybe)

import HsDev.Cabal
import HsDev.Symbols
import HsDev.Tools.Base (inspect)
import HsDev.Util (liftIOErrors, ordNub)

import Data.Version
import qualified ConLike as GHC
import qualified DataCon as GHC
import qualified DynFlags as GHC
import qualified GHC
import qualified GHC.PackageDb as GHC
import qualified GhcMonad as GHC (liftIO)
import qualified GHC.Paths as GHC
import qualified Name as GHC
import qualified Outputable as GHC
import qualified Packages as GHC
import qualified PatSyn as GHC
import qualified TyCon as GHC
import qualified Type as GHC
import qualified Var as GHC
import Pretty

-- | Browse packages
browsePackages :: [String] -> SandboxStack -> ExceptT String IO [ModulePackage]
browsePackages opts sboxes = liftIOErrors $ withPackages (sandboxStackOpt sboxes ++ opts) $ \dflags ->
	return $ mapMaybe readPackage $ fromMaybe [] $ GHC.pkgDatabase dflags

listModules :: [String] -> SandboxStack -> ExceptT String IO [ModuleLocation]
listModules opts sboxes = liftIOErrors $ withPackages_ (sandboxStackOpt sboxes ++ opts) $ do
	ms <- lift packageDbModules
	return $ map (uncurry $ ghcModuleLocation sboxes) ms

browseModules :: [String] -> SandboxStack -> [ModuleLocation] -> ExceptT String IO [InspectedModule]
browseModules opts sboxes mlocs = liftIOErrors $ withPackages_ (sandboxStackOpt sboxes ++ opts) $ do
	ms <- lift packageDbModules
	liftM catMaybes $ mapM (uncurry browseModule') [(p, m) | (p, m) <- ms, ghcModuleLocation sboxes p m `elem` mlocs]
	where
		browseModule' :: GHC.PackageConfig -> GHC.Module -> ExceptT String GHC.Ghc (Maybe InspectedModule)
		browseModule' p m = tryT $ inspect (ghcModuleLocation sboxes p m) (return $ InspectionAt 0 opts) (browseModule sboxes p m)

-- | Browse all modules
browse :: [String] -> SandboxStack -> ExceptT String IO [InspectedModule]
browse opts sboxes = listModules opts sboxes >>= browseModules opts sboxes

browseModule :: SandboxStack -> GHC.PackageConfig -> GHC.Module -> ExceptT String GHC.Ghc Module
browseModule sboxes package m = do
	mi <- lift (GHC.getModuleInfo m) >>= maybe (throwError "Can't find module info") return
	ds <- mapM (toDecl mi) (GHC.modInfoExports mi)
	let
		thisModule = GHC.moduleNameString (GHC.moduleName m)
	return Module {
		_moduleName = fromString thisModule,
		_moduleDocs = Nothing,
		_moduleLocation = thisLoc,
		_moduleExports = Just [ExportName Nothing (view declarationName d) ExportNothing | d <- ds],
		_moduleImports = [import_ iname | iname <- ordNub (mapMaybe (preview definedModule) ds), iname /= fromString thisModule],
		_moduleDeclarations = sortDeclarations ds }
	where
		thisLoc = view moduleIdLocation $ mloc m
		mloc m' = ModuleId (fromString mname') $
			ghcModuleLocation sboxes package m'
			where
				mname' = GHC.moduleNameString $ GHC.moduleName m'
		toDecl minfo n = do
			tyInfo <- lift $ GHC.modInfoLookupName minfo n
			tyResult <- lift $ maybe (inModuleSource n) (return . Just) tyInfo
			dflag <- lift GHC.getSessionDynFlags
			let
				decl' = decl (fromString $ GHC.getOccString n) $ fromMaybe
					(Function Nothing [] Nothing)
					(tyResult >>= showResult dflag)
			return $ decl' `definedIn` mloc (GHC.nameModule n)
		definedModule = declarationDefined . _Just . moduleIdName
		showResult :: GHC.DynFlags -> GHC.TyThing -> Maybe DeclarationInfo
		showResult dflags (GHC.AnId i) = Just $ Function (Just $ fromString $ formatType dflags GHC.varType i) [] Nothing
		showResult dflags (GHC.AConLike c) = case c of
			GHC.RealDataCon d -> Just $ Function (Just $ fromString $ formatType dflags GHC.dataConRepType d) [] Nothing
			GHC.PatSynCon p -> Just $ Function (Just $ fromString $ formatType dflags GHC.patSynType p) [] Nothing
		showResult _ (GHC.ATyCon t) = Just $ tcon $ TypeInfo Nothing (map (fromString . GHC.getOccString) $ GHC.tyConTyVars t) Nothing [] where
			tcon
				| GHC.isAlgTyCon t && not (GHC.isNewTyCon t) && not (GHC.isClassTyCon t) = Data
				| GHC.isNewTyCon t = NewType
				| GHC.isClassTyCon t = Class
				| GHC.isTypeSynonymTyCon t = Type
				| otherwise = Type
		showResult _ _ = Nothing

withInitializedPackages :: [String] -> (GHC.DynFlags -> GHC.Ghc a) -> IO a
withInitializedPackages ghcOpts cont = GHC.runGhc (Just GHC.libdir) $ do
	fs <- GHC.getSessionDynFlags
	GHC.defaultCleanupHandler fs $ do
		(fs', _, _) <- GHC.parseDynamicFlags fs (map GHC.noLoc ghcOpts)
		_ <- GHC.setSessionDynFlags fs'
		(result, _) <- GHC.liftIO $ GHC.initPackages fs'
		cont result

withPackages :: [String] -> (GHC.DynFlags -> ExceptT String GHC.Ghc a) -> ExceptT String IO a
withPackages ghcOpts cont = ExceptT $ withInitializedPackages ghcOpts (runExceptT . cont)

withPackages_ :: [String] -> ExceptT String GHC.Ghc a -> ExceptT String IO a
withPackages_ ghcOpts act = withPackages ghcOpts (const act)

inModuleSource :: GHC.Name -> GHC.Ghc (Maybe GHC.TyThing)
inModuleSource nm = GHC.getModuleInfo (GHC.nameModule nm) >> GHC.lookupGlobalName nm

formatType :: GHC.NamedThing a => GHC.DynFlags -> (a -> GHC.Type) -> a -> String
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
showUnqualifiedPage dflag = Pretty.showDoc Pretty.LeftMode 0 . GHC.withPprStyleDoc dflag styleUnqualified

styleUnqualified :: GHC.PprStyle
styleUnqualified = GHC.mkUserStyle GHC.neverQualify GHC.AllTheWay

tryT :: Monad m => ExceptT e m a -> ExceptT e m (Maybe a)
tryT act = catchError (liftM Just act) (const $ return Nothing)

readPackage :: GHC.PackageConfig -> Maybe ModulePackage
readPackage pc = readMaybe $ GHC.packageNameString pc ++ "-" ++ showVersion (GHC.packageVersion pc)

ghcModuleLocation :: SandboxStack -> GHC.PackageConfig -> GHC.Module -> ModuleLocation
ghcModuleLocation sboxes p m = CabalModule cabal (readPackage p) (GHC.moduleNameString $ GHC.moduleName m) where
	cabal = maybe Cabal Sandbox $ listToMaybe $ sortBy (flip compare) (intersect sboxes pkgs)
	dirs = GHC.libraryDirs p
	pkgs = ordNub $ mapMaybe pkgDb dirs
	pkgDb path'
		| "lib" `elem` paths' = Just $ (joinPath . reverse . drop 1 . dropWhile (/= "lib") . reverse $ paths') </> "pkgdb"
		| otherwise = Nothing
		where
			paths' = splitDirectories path'

packageDbModules :: GHC.GhcMonad m => m [(GHC.PackageConfig, GHC.Module)]
packageDbModules = do
	dflags <- GHC.getSessionDynFlags
	let
		pkgs = fromMaybe [] $ GHC.pkgDatabase dflags
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
