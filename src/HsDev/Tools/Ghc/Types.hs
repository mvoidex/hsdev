{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, RankNTypes, TemplateHaskell, OverloadedStrings #-}

module HsDev.Tools.Ghc.Types (
	TypedExpr(..), typedExpr, typedType,
	moduleTypes, fileTypes,
	setModuleTypes, inferTypes
	) where

import Control.DeepSeq
import Control.Lens (over, view, set, each, preview, makeLenses, _Just)
import Control.Monad
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class
import Data.Aeson
import Data.Generics
import Data.List (find)
import Data.Maybe
import Data.String (fromString)
import System.Directory
import System.FilePath
import System.Log.Simple (MonadLog(..), scope)

import GHC hiding (exprType, Module, moduleName)
import GHC.SYB.Utils (everythingStaged, Stage(TypeChecker))
import GhcPlugins (mkFunTys)
import CoreUtils
import Desugar (deSugarExpr)
import TcHsSyn (hsPatType)
import Outputable
import PprTyThing
import Pretty

import System.Directory.Paths (canonicalize)
import HsDev.Error
import HsDev.Scan.Browse (browsePackages)
import HsDev.PackageDb
import HsDev.Symbols
import HsDev.Tools.Ghc.Worker
import HsDev.Tools.Types
import HsDev.Util hiding (withCurrentDirectory)

class HasType a where
	getType :: GhcMonad m => TypecheckedModule -> a -> m (Maybe (SrcSpan, Type))

instance HasType (LHsExpr Id) where
	getType _ e = do
		env <- getSession
		mbe <- liftIO $ liftM snd $ deSugarExpr env e
		return $ do
			ex <- mbe
			return (getLoc e, exprType ex)

instance HasType (LHsBind Id) where
	getType _ (L spn FunBind { fun_matches = m}) = return $ Just (spn, typ) where
		typ = mkFunTys (mg_arg_tys m) (mg_res_ty m)
	getType _ _ = return Nothing

instance HasType (LPat Id) where
	getType _ (L spn pat) = return $ Just (spn, hsPatType pat)

locatedTypes :: Typeable a => TypecheckedSource -> [Located a]
locatedTypes = types' p where
	types' :: Typeable r => (r -> Bool) -> GenericQ [r]
	types' p' = everythingStaged TypeChecker (++) [] ([] `mkQ` (\x -> [x | p' x]))
	p (L spn _) = isGoodSrcSpan spn

moduleTypes :: GhcMonad m => FilePath -> m [(SrcSpan, Type)]
moduleTypes fpath = do
	fpath' <- liftIO $ canonicalize fpath
	mg <- getModuleGraph
	[m] <- liftIO $ flip filterM mg $ \m -> do
		mfile <- traverse (liftIO . canonicalize) $ ml_hs_file (ms_location m)
		return (Just fpath' == mfile)
	p <- parseModule m
	tm <- typecheckModule p
	let
		ts = tm_typechecked_source tm
	liftM (catMaybes . concat) $ sequence [
		mapM (getType tm) (locatedTypes ts :: [LHsBind Id]),
		mapM (getType tm) (locatedTypes ts :: [LHsExpr Id]),
		mapM (getType tm) (locatedTypes ts :: [LPat Id])]

data TypedExpr = TypedExpr {
	_typedExpr :: String,
	_typedType :: String }
		deriving (Eq, Ord, Read, Show)

makeLenses ''TypedExpr

instance NFData TypedExpr where
	rnf (TypedExpr e t) = rnf e `seq` rnf t

instance ToJSON TypedExpr where
	toJSON (TypedExpr e t) = object [
		"expr" .= e,
		"type" .= t]

instance FromJSON TypedExpr where
	parseJSON = withObject "typed-expr" $ \v -> TypedExpr <$>
		v .:: "expr" <*>
		v .:: "type"

-- | Get all types in module
fileTypes :: (MonadLog m, GhcMonad m, MonadThrow m) => [String] -> PackageDbStack -> Module -> Maybe String -> m [Note TypedExpr]
fileTypes opts pdbs m msrc = scope "types" $ case view moduleLocation m of
	FileModule file proj -> do
		file' <- liftIO $ canonicalize file
		cts <- maybe (liftIO $ readFileUtf8 file') return msrc
		pkgs <- browsePackages opts pdbs
		let
			dir = fromMaybe
				(sourceModuleRoot (view moduleName m) file') $
				preview (_Just . projectPath) proj
		dirExist <- liftIO $ doesDirectoryExist dir
		withFlags $ (if dirExist then withCurrentDirectory dir else id) $ do
			_ <- setCmdOpts $ concat [
				packageDbStackOpts pdbs,
				moduleOpts pkgs m,
				opts]
			target <- makeTarget (makeRelative dir file') msrc
			loadTargets [target]
			ts <- moduleTypes file'
			df <- getSessionDynFlags
			return $ map (setExpr cts . recalcTabs cts 8 . uncurry (toNote df)) ts
	_ -> hsdevError $ ModuleNotSource (view moduleLocation m)
	where
		toNote :: DynFlags -> SrcSpan -> Type -> Note String
		toNote df spn tp = Note {
			_noteSource = noLocation,
			_noteRegion = spanRegion spn,
			_noteLevel = Nothing,
			_note = showType df tp }
		setExpr :: String -> Note String -> Note TypedExpr
		setExpr cts n = over note (TypedExpr (regionStr (view noteRegion n) cts)) n
		showType :: DynFlags -> Type -> String
		showType df = showDoc OneLineMode 80 . withPprStyleDoc df unqualStyle . pprTypeForUser
		unqualStyle :: PprStyle
		unqualStyle = mkUserStyle neverQualify AllTheWay

-- | Set types to module
setModuleTypes :: [Note TypedExpr] -> Module -> Module
setModuleTypes ts = over (moduleDeclarations . each) setType where
	setType :: Declaration -> Declaration
	setType d = fromMaybe d $ do
		pos <- view declarationPosition d
		tnote <- find ((== pos) . view (noteRegion . regionFrom)) ts
		return $ set (declaration . functionType) (Just $ fromString $ view (note . typedType) tnote) d

-- | Infer types in module
inferTypes :: (MonadLog m, GhcMonad m, MonadThrow m) => [String] -> PackageDbStack -> Module -> Maybe String -> m Module
inferTypes opts pdbs m msrc = scope "infer" $ liftM (`setModuleTypes` m) $ fileTypes opts pdbs m msrc
