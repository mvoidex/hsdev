module HsDev.Scan.Browse (
	browse
	) where

import Control.Arrow
import Control.Monad.Error
import Data.Maybe
import Data.Either
import qualified Data.Map as M
import Text.Read (readMaybe)

import HsDev.Cabal
import HsDev.Symbols
import HsDev.Tools.Base (inspect)

import qualified DataCon as GHC
import qualified DynFlags as GHC
import qualified Exception as GHC
import qualified FastString as GHC
import qualified GHC
import qualified GhcMonad as GHC (liftIO)
import qualified GHC.Paths as GHC
import qualified Module as GHC
import qualified Name as GHC
import qualified Name as GHC
import qualified Outputable as GHC
import qualified Packages as GHC
import qualified TyCon as GHC
import qualified Type as GHC
import qualified Var as GHC
import Pretty

browse :: [String] -> Cabal -> ErrorT String IO [InspectedModule]
browse opts cabal = ErrorT $ withInitializedPackages (cabalOpt cabal ++ opts) (runErrorT . browse') where
	browse' :: GHC.DynFlags -> ErrorT String GHC.Ghc [InspectedModule]
	browse' d = do
		ms <- lift $ modules d
		liftM catMaybes $ mapM browseModule' ms
	inspection :: Monad m => m Inspection
	inspection = return $ InspectionAt 0 opts

	browseModule' :: GHC.Module -> ErrorT String GHC.Ghc (Maybe InspectedModule)
	browseModule' m = tryT $ inspect
		(CabalModule cabal (readMaybe $ GHC.packageIdString $ GHC.modulePackageId m) (GHC.moduleNameString $ GHC.moduleName m))
		(return $ InspectionAt 0 opts)
		(browseModule cabal m)

browseModule :: Cabal -> GHC.Module -> ErrorT String GHC.Ghc Module
browseModule cabal m = do
	mi <- lift (GHC.getModuleInfo m) >>= maybe (throwError "Can't find module info") return
	ds <- mapM (toDecl mi) (GHC.modInfoExports mi)
	return $ Module {
		moduleName = GHC.moduleNameString (GHC.moduleName m),
		moduleDocs = Nothing,
		moduleLocation = mloc,
		moduleExports = [],
		moduleImports = [],
		moduleDeclarations = M.fromList (map (declarationName &&& id) ds) }
	where
		mloc = CabalModule cabal (readMaybe $ GHC.packageIdString $ GHC.modulePackageId m) (GHC.moduleNameString $ GHC.moduleName m)
		toDecl minfo n = do
			tyInfo <- lift $ GHC.modInfoLookupName minfo n
			tyResult <- lift $ maybe (inOtherModule n) (return . Just) tyInfo
			dflag <- lift GHC.getSessionDynFlags
			return $ Declaration
				(GHC.getOccString n)
				Nothing
				Nothing
				(fromMaybe (Function Nothing) (tyResult >>= showResult dflag))
		showResult :: GHC.DynFlags -> GHC.TyThing -> Maybe DeclarationInfo
		showResult dflags (GHC.AnId i) = Just $ Function $ Just $ formatType dflags GHC.varType i
		showResult dflags (GHC.ADataCon d) = Just $ Function $ Just $ formatType dflags GHC.dataConRepType d
		showResult _ (GHC.ATyCon t) = Just $ tcon $ TypeInfo Nothing (map GHC.getOccString $ GHC.tyConTyVars t) Nothing where
			tcon
				| GHC.isAlgTyCon t && not (GHC.isNewTyCon t) && not (GHC.isClassTyCon t) = Data
				| GHC.isNewTyCon t = NewType
				| GHC.isClassTyCon t = Class
				| GHC.isSynTyCon t = Type
				| otherwise = Type

withInitializedPackages :: [String] -> (GHC.DynFlags -> GHC.Ghc a) -> IO a
withInitializedPackages ghcOpts cont = GHC.runGhc (Just GHC.libdir) $ do
		fs <- GHC.getSessionDynFlags
		GHC.defaultCleanupHandler fs $ do
			(fs', _, _) <- GHC.parseDynamicFlags fs (map GHC.noLoc ghcOpts)
			GHC.setSessionDynFlags fs'
			(result, _) <- GHC.liftIO $ GHC.initPackages fs'
			cont result

modules :: GHC.GhcMonad m => GHC.DynFlags -> m [GHC.Module]
modules d = liftM (rights :: [Either GHC.SomeException GHC.Module] -> [GHC.Module]) $ sequence [
	GHC.gtry $ GHC.findModule nm (Just $ GHC.mkFastString $ GHC.display $ GHC.pkgName $ GHC.sourcePackageId pc) |
	pc <- fromMaybe [] (GHC.pkgDatabase d),
	nm <- GHC.exposedModules pc]

inOtherModule :: GHC.Name -> GHC.Ghc (Maybe GHC.TyThing)
inOtherModule nm = GHC.getModuleInfo (GHC.nameModule nm) >> GHC.lookupGlobalName nm

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
showUnqualifiedPage dflag = Pretty.showDocWith Pretty.PageMode . GHC.withPprStyleDoc dflag styleUnqualified

styleUnqualified :: GHC.PprStyle
styleUnqualified = GHC.mkUserStyle GHC.neverQualify GHC.AllTheWay

tryT :: (Monad m, Error e) => ErrorT e m a -> ErrorT e m (Maybe a)
tryT act = catchError (liftM Just $ act) (const $ return Nothing)
