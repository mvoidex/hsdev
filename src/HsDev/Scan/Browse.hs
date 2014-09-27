module HsDev.Scan.Browse (
	-- * Scan cabal modules
	browseFilter, browse
	) where

import Control.Arrow
import Control.Monad.Error
import Data.List (nub)
import Data.Maybe
import qualified Data.Map as M
import Text.Read (readMaybe)

import HsDev.Cabal
import HsDev.Symbols
import HsDev.Tools.Base (inspect)

import qualified ConLike as GHC
import qualified DataCon as GHC
import qualified DynFlags as GHC
import qualified GHC
import qualified GhcMonad as GHC (liftIO)
import qualified GHC.Paths as GHC
import qualified Name as GHC
import qualified Module as GHC
import qualified Outputable as GHC
import qualified Packages as GHC
import qualified PatSyn as GHC
import qualified TyCon as GHC
import qualified Type as GHC
import qualified Var as GHC
import Pretty

-- | Browse modules
browseFilter :: [String] -> Cabal -> (ModuleLocation -> ErrorT String IO Bool) -> ErrorT String IO [InspectedModule]
browseFilter opts cabal f = withPackages_ (cabalOpt cabal ++ opts) $ do
	ms <- lift $ GHC.packageDbModules False
	ms' <- filterM (mapErrorT GHC.liftIO . f . loc) ms
	liftM catMaybes $ mapM browseModule' ms'
	where
		loc :: GHC.Module -> ModuleLocation
		loc m = CabalModule cabal (readMaybe $ GHC.packageIdString $ GHC.modulePackageId m) (GHC.moduleNameString $ GHC.moduleName m)

		browseModule' :: GHC.Module -> ErrorT String GHC.Ghc (Maybe InspectedModule)
		browseModule' m = tryT $ inspect (loc m) (return $ InspectionAt 0 opts) (browseModule cabal m)

-- | Browse all modules
browse :: [String] -> Cabal -> ErrorT String IO [InspectedModule]
browse opts cabal = browseFilter opts cabal (const $ return True)

browseModule :: Cabal -> GHC.Module -> ErrorT String GHC.Ghc Module
browseModule cabal m = do
	mi <- lift (GHC.getModuleInfo m) >>= maybe (throwError "Can't find module info") return
	ds <- mapM (toDecl mi) (GHC.modInfoExports mi)
	let
		thisModule = GHC.moduleNameString (GHC.moduleName m)
	return Module {
		moduleName = thisModule,
		moduleDocs = Nothing,
		moduleLocation = mloc,
		moduleExports = Just $ map (ExportName . declarationName . snd) ds,
		moduleImports = [import_ iname | iname <- nub (map fst ds), iname /= thisModule],
		moduleDeclarations = M.fromList (map ((declarationName &&& id) . snd) ds) }
	where
		mloc = CabalModule cabal (readMaybe $ GHC.packageIdString $ GHC.modulePackageId m) (GHC.moduleNameString $ GHC.moduleName m)
		toDecl minfo n = do
			tyInfo <- lift $ GHC.modInfoLookupName minfo n
			tyResult <- lift $ maybe (inModuleSource n) (return . Just) tyInfo
			dflag <- lift GHC.getSessionDynFlags
			let
				srcMod = GHC.moduleNameString $ GHC.moduleName $ GHC.nameModule n
				decl' = decl (GHC.getOccString n) $ fromMaybe
					(Function Nothing [])
					(tyResult >>= showResult dflag)
			return (srcMod, decl')
		showResult :: GHC.DynFlags -> GHC.TyThing -> Maybe DeclarationInfo
		showResult dflags (GHC.AnId i) = Just $ Function (Just $ formatType dflags GHC.varType i) []
		showResult dflags (GHC.AConLike c) = case c of
			GHC.RealDataCon d -> Just $ Function (Just $ formatType dflags GHC.dataConRepType d) []
			GHC.PatSynCon p -> Just $ Function (Just $ formatType dflags GHC.patSynType p) []
		showResult _ (GHC.ATyCon t) = Just $ tcon $ TypeInfo Nothing (map GHC.getOccString $ GHC.tyConTyVars t) Nothing where
			tcon
				| GHC.isAlgTyCon t && not (GHC.isNewTyCon t) && not (GHC.isClassTyCon t) = Data
				| GHC.isNewTyCon t = NewType
				| GHC.isClassTyCon t = Class
				| GHC.isSynTyCon t = Type
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

withPackages :: [String] -> (GHC.DynFlags -> ErrorT String GHC.Ghc a) -> ErrorT String IO a
withPackages ghcOpts cont = ErrorT $ withInitializedPackages ghcOpts (runErrorT . cont)

withPackages_ :: [String] -> ErrorT String GHC.Ghc a -> ErrorT String IO a
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

tryT :: (Monad m, Error e) => ErrorT e m a -> ErrorT e m (Maybe a)
tryT act = catchError (liftM Just $ act) (const $ return Nothing)
