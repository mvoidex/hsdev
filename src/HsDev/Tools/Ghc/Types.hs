{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, RankNTypes, TemplateHaskell, OverloadedStrings #-}

module HsDev.Tools.Ghc.Types (
	TypedExpr(..), typedExpr, typedType,
	moduleTypes, fileTypes,
	setModuleTypes, inferTypes
	) where

import Control.DeepSeq
import Control.Lens (over, view, set, each, preview, makeLenses, _Just)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Generics
import Data.List (find)
import Data.Maybe
import Data.String (fromString)
import Data.Text (Text)
import System.Log.Simple (MonadLog(..), scope)

import GHC hiding (exprType, Module, moduleName)
import GHC.SYB.Utils (everythingStaged, Stage(TypeChecker))
import GhcPlugins (mkFunTys)
import CoreUtils
import Desugar (deSugarExpr)
import TcHsSyn (hsPatType)
import Outputable
import PprTyThing
import qualified Pretty

import System.Directory.Paths
import HsDev.Error
import HsDev.Symbols
import HsDev.Tools.Ghc.Worker as Ghc
import HsDev.Tools.Ghc.Compat
import HsDev.Tools.Types
import HsDev.Util

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
	getType _ (L _ FunBind { fun_id = fid, fun_matches = m}) = return $ Just (getLoc fid, typ) where
		typ = mkFunTys (mg_arg_tys m) (mg_res_ty m)
	getType _ _ = return Nothing

instance HasType (LPat Id) where
	getType _ (L spn pat) = return $ Just (spn, hsPatType pat)

locatedTypes :: Typeable a => TypecheckedSource -> [Located a]
locatedTypes = types' p where
	types' :: Typeable r => (r -> Bool) -> GenericQ [r]
	types' p' = everythingStaged TypeChecker (++) [] ([] `mkQ` (\x -> [x | p' x]))
	p (L spn _) = isGoodSrcSpan spn

moduleTypes :: GhcMonad m => Path -> m [(SrcSpan, Type)]
moduleTypes fpath = do
	fpath' <- liftIO $ canonicalize fpath
	mg <- getModuleGraph
	[m] <- liftIO $ flip filterM mg $ \m -> do
		mfile <- traverse (liftIO . canonicalize) $ ml_hs_file (ms_location m)
		return (Just (view path fpath') == mfile)
	p <- parseModule m
	tm <- typecheckModule p
	let
		ts = tm_typechecked_source tm
	liftM (catMaybes . concat) $ sequence [
		mapM (getType tm) (locatedTypes ts :: [LHsBind Id]),
		mapM (getType tm) (locatedTypes ts :: [LHsExpr Id]),
		mapM (getType tm) (locatedTypes ts :: [LPat Id])]

data TypedExpr = TypedExpr {
	_typedExpr :: Text,
	_typedType :: Text }
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
fileTypes :: (MonadLog m, GhcMonad m) => [String] -> Module -> Maybe Text -> m [Note TypedExpr]
fileTypes opts m msrc = scope "types" $ case view (moduleId . moduleLocation) m of
	FileModule file proj -> do
		file' <- liftIO $ canonicalize file
		cts <- maybe (liftIO $ readFileUtf8 (view path file')) return msrc
		let
			dir = fromMaybe
				(sourceModuleRoot (view (moduleId . moduleName) m) file') $
				preview (_Just . projectPath) proj
		ex <- liftIO $ dirExists dir
		withFlags $ (if ex then Ghc.withCurrentDirectory (view path dir) else id) $ do
			addCmdOpts opts
			target <- makeTarget (relPathTo dir file') msrc
			loadTargets [target]
			ts <- moduleTypes file'
			df <- getSessionDynFlags
			return $ map (setExpr cts . recalcTabs cts 8 . uncurry (toNote df)) ts
	_ -> hsdevError $ ModuleNotSource (view (moduleId . moduleLocation) m)
	where
		toNote :: DynFlags -> SrcSpan -> Type -> Note Text
		toNote df spn tp = Note {
			_noteSource = noLocation,
			_noteRegion = spanRegion spn,
			_noteLevel = Nothing,
			_note = fromString $ showType df tp }
		setExpr :: Text -> Note Text -> Note TypedExpr
		setExpr cts n = over note (TypedExpr (regionStr (view noteRegion n) cts)) n
		showType :: DynFlags -> Type -> String
		showType df = renderStyle Pretty.OneLineMode 80 . withPprStyleDoc df (unqualStyle df) . pprTypeForUser

-- | Set types to module
setModuleTypes :: [Note TypedExpr] -> Module -> Module
setModuleTypes ts = over (moduleScope . each . each) setType . over (moduleExports . each) setType where
	setType :: Symbol -> Symbol
	setType d = fromMaybe d $ do
		pos <- view symbolPosition d
		tnote <- find ((== pos) . view (noteRegion . regionFrom)) ts
		return $ set (symbolInfo . functionType) (Just $ view (note . typedType) tnote) d

-- | Infer types in module
inferTypes :: (MonadLog m, GhcMonad m) => [String] -> Module -> Maybe Text -> m Module
inferTypes opts m msrc = scope "infer" $ liftM (`setModuleTypes` m) $ fileTypes opts m msrc
