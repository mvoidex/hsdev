{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, RankNTypes, TemplateHaskell, OverloadedStrings #-}

module HsDev.Tools.Ghc.Types (
	TypedExpr(..), typedExpr, typedType,
	moduleTypes, fileTypes,
	setModuleTypes, inferTypes
	) where

import Control.DeepSeq
import Control.Lens (over, view, set, each, preview, makeLenses, _Just)
import Control.Monad
import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Generics
import Data.List (find)
import Data.Maybe
import Data.String (fromString)
import Data.Text (Text)
import System.Log.Simple (MonadLog(..), scope)

import GHC hiding (exprType, Module, moduleName)
import GhcPlugins (mkFunTys)
import CoreUtils as C
import NameSet (NameSet)
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
	getType :: GhcMonad m => a -> m (Maybe (SrcSpan, Type))

instance HasType (LHsExpr TcId) where
	getType e = do
		env <- getSession
		mbe <- liftIO $ liftM snd $ deSugarExpr env e
		return $ do
			ex <- mbe
			return (getLoc e, C.exprType ex)

instance HasType (LHsBind TcId) where
	getType (L _ FunBind { fun_id = fid, fun_matches = m}) = return $ do
		argTys <- mgArgTys m
		resTy <- mgResTy m
		return (getLoc fid, mkFunTys argTys resTy)
	getType _ = return Nothing

instance HasType (LPat TcId) where
	getType (L spn pat) = return $ Just (spn, hsPatType pat)

locatedTypes :: Typeable a => TypecheckedSource -> [Located a]
locatedTypes = types' p where
	types' :: Typeable r => (r -> Bool) -> GenericQ [r]
	types' p' = everythingTyped (++) [] ([] `mkQ` (\x -> [x | p' x]))
	p (L spn _) = isGoodSrcSpan spn

everythingTyped :: (r -> r -> r) -> r -> GenericQ r -> GenericQ r
everythingTyped k z f x
	| (const False `extQ` nameSet) x = z
	| otherwise = foldl k (f x) (gmapQ (everythingTyped k z f) x)
	where
		nameSet :: NameSet -> Bool
		nameSet = const True

moduleTypes :: (MonadFail m, GhcMonad m) => Path -> m [(SrcSpan, Type)]
moduleTypes fpath = do
	fpath' <- liftIO $ canonicalize fpath
	mg <- getModuleGraph
	[m] <- liftIO $ flip filterM (modSummaries mg) $ \m -> do
		mfile <- traverse (liftIO . canonicalize) $ ml_hs_file (ms_location m)
		return (Just (view path fpath') == mfile)
	p <- parseModule m
	tm <- typecheckModule p
	let
		ts = tm_typechecked_source tm
	liftM (catMaybes . concat) $ sequence [
		mapM getType (locatedTypes ts :: [LHsExpr TcId]),
		mapM getType (locatedTypes ts :: [LHsBind TcId]),
		mapM getType (locatedTypes ts :: [LPat TcId])]

data TypedExpr = TypedExpr {
	_typedExpr :: Maybe Text,
	_typedType :: Text }
		deriving (Eq, Ord, Read, Show)

makeLenses ''TypedExpr

instance NFData TypedExpr where
	rnf (TypedExpr e t) = rnf e `seq` rnf t

instance ToJSON TypedExpr where
	toJSON (TypedExpr e t) = object $ noNulls [
		"expr" .= e,
		"type" .= t]

instance FromJSON TypedExpr where
	parseJSON = withObject "typed-expr" $ \v -> TypedExpr <$>
		v .::? "expr" <*>
		v .:: "type"

-- | Get all types in module
fileTypes :: (MonadLog m, MonadFail m, GhcMonad m) => Module -> Maybe Text -> m [Note TypedExpr]
fileTypes m msrc = scope "types" $ case view (moduleId . moduleLocation) m of
	FileModule file proj -> do
		file' <- liftIO $ canonicalize file
		cts <- maybe (liftIO $ readFileUtf8 (view path file')) return msrc
		let
			dir = fromMaybe
				(sourceModuleRoot (view (moduleId . moduleName) m) file') $
				preview (_Just . projectPath) proj
		ex <- liftIO $ dirExists dir
		(if ex then Ghc.withCurrentDirectory (view path dir) else id) $ do
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
		setExpr cts n = over note (TypedExpr (Just (regionStr (view noteRegion n) cts))) n
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
inferTypes :: (MonadLog m, MonadFail m, GhcMonad m) => Module -> Maybe Text -> m Module
inferTypes m msrc = scope "infer" $ liftM (`setModuleTypes` m) $ fileTypes m msrc
