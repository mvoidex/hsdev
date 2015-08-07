{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, RankNTypes, TemplateHaskell, OverloadedStrings #-}

module HsDev.Tools.Ghc.Types (
	TypedExpr(..), typedExpr, typedType,
	moduleTypes, fileTypes
	) where

import Control.DeepSeq
import Control.Lens (over, view, preview, makeLenses, _Just)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Generics
import Data.Maybe
import System.Directory
import System.FilePath

import GHC hiding (exprType, Module, moduleName)
import GHC.SYB.Utils (everythingStaged, Stage(TypeChecker))
import GhcPlugins (mkFunTys)
import CoreUtils
import Desugar (deSugarExpr)
import TcHsSyn (hsPatType)
import Outputable
import PprTyThing
import Pretty

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

fileTypes :: [String] -> Cabal -> Module -> Maybe String -> ExceptT String Ghc [Note TypedExpr]
fileTypes opts cabal m msrc = case view moduleLocation m of
	FileModule file proj -> do
		file' <- liftIO $ canonicalize file
		cts <- maybe (liftIO $ readFileUtf8 file') return msrc
		pkgs <- lift listPackages
		let
			dir = fromMaybe
				(sourceModuleRoot (view moduleName m) file') $
				preview (_Just . projectPath) proj
		dirExist <- liftIO $ doesDirectoryExist dir
		lift $ withFlags $ (if dirExist then withCurrentDirectory dir else id) $ do
			_ <- addCmdOpts $ concat [
				cabalOpt cabal,
				moduleOpts pkgs m,
				opts]
			target <- makeTarget (makeRelative dir file') msrc
			loadTargets [target]
			ts <- moduleTypes file'
			df <- getSessionDynFlags
			return $ map (setExpr cts . recalcTabs cts 8 . uncurry (toNote df)) ts
	_ -> throwError "Module is not source"
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
