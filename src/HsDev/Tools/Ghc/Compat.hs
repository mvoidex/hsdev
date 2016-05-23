{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Tools.Ghc.Compat (
	pkgDatabase, UnitId, unitId, depends, getPackageDetails, patSynType, cleanupHandler, renderStyle,
	LogAction, setLogAction
	) where

import qualified DynFlags as GHC
import qualified ErrUtils
import qualified GHC
import qualified Module
import qualified Packages as GHC
import qualified PatSyn as GHC
import qualified Pretty

#if __GLASGOW_HASKELL__ == 710
import Exception (ExceptionMonad)
import Control.Monad.Reader
#endif

pkgDatabase :: GHC.DynFlags -> Maybe [GHC.PackageConfig]
#if __GLASGOW_HASKELL__ == 800
pkgDatabase = fmap (concatMap snd) . GHC.pkgDatabase
#endif
#if __GLASGOW_HASKELL__ == 710
pkgDatabase = GHC.pkgDatabase
#endif

#if __GLASGOW_HASKELL__ == 800
type UnitId = Module.UnitId
#endif
#if __GLASGOW_HASKELL__ == 710
type UnitId = Module.PackageKey
#endif

unitId :: GHC.PackageConfig -> UnitId
#if __GLASGOW_HASKELL__ == 800
unitId = GHC.unitId
#endif
#if __GLASGOW_HASKELL__ == 710
unitId = GHC.packageKey
#endif

depends :: GHC.DynFlags -> GHC.PackageConfig -> [UnitId]
#if __GLASGOW_HASKELL__ == 800
depends _ = GHC.depends
#endif
#if __GLASGOW_HASKELL__ == 710
depends df = map (GHC.resolveInstalledPackageId df) . GHC.depends
#endif

getPackageDetails :: GHC.DynFlags -> UnitId -> GHC.PackageConfig
getPackageDetails = GHC.getPackageDetails

patSynType :: GHC.PatSyn -> GHC.Type
patSynType p = GHC.patSynInstResTy p (GHC.patSynArgs p)

#if __GLASGOW_HASKELL__ == 800
cleanupHandler :: GHC.DynFlags -> m a -> m a
cleanupHandler _ = id
#endif
#if __GLASGOW_HASKELL__ == 710
cleanupHandler :: (ExceptionMonad m) => GHC.DynFlags -> m a -> m a
cleanupHandler = GHC.defaultCleanupHandler
#endif

renderStyle :: Pretty.Mode -> Int -> Pretty.Doc -> String
#if __GLASGOW_HASKELL__ == 800
renderStyle m cols = Pretty.renderStyle (Pretty.Style m cols 1.5)
#endif
#if __GLASGOW_HASKELL__ == 710
renderStyle = Pretty.showDoc
#endif

type LogAction = GHC.DynFlags -> GHC.Severity -> GHC.SrcSpan -> ErrUtils.MsgDoc -> IO ()

setLogAction :: LogAction -> GHC.DynFlags -> GHC.DynFlags
setLogAction act fs = fs { GHC.log_action = act' } where
	act' :: GHC.LogAction
#if __GLASGOW_HASKELL__ == 800
	act' df _ sev src _ msg = act df sev src msg
#endif
#if __GLASGOW_HASKELL__ == 710
	act' df sev src _ msg = act df sev src msg
#endif

#if __GLASGOW_HASKELL__ == 710
instance (Monad m, GHC.HasDynFlags m) => GHC.HasDynFlags (ReaderT r m) where
	getDynFlags = lift GHC.getDynFlags
#endif
