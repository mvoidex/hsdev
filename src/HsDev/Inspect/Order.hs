module HsDev.Inspect.Order (
	orderBy, order
	) where

import Control.Lens
import Data.Maybe
import Data.String
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Language.Haskell.Exts as H

import Data.Deps
import HsDev.Inspect
import HsDev.Symbols.Types
import System.Directory.Paths

-- | Order source files so that dependencies goes first and we are able to resolve symbols and set fixities
orderBy :: (a -> Maybe Preloaded) -> [a] -> Either (DepsError Path) [a]
orderBy fn ps = do
	order' <- linearize pdeps
	return $ mapMaybe (`M.lookup` pm) order'
	where
		pdeps = mconcat $ mapMaybe (fmap getDeps . fn) ps
		pm = M.fromList [(pfile, p) | p <- ps, pfile <- fn p ^.. _Just . preloadedId . moduleLocation . moduleFile]
		files = S.fromList $ map fn ps ^.. each . _Just . preloadedId . moduleLocation . moduleFile
		getDeps :: Preloaded -> Deps Path
		getDeps p = deps mfile [ifile | ifile <- ifiles, S.member ifile files] where
			H.Module _ _ _ idecls _ = _preloadedModule p
			imods = [fromString iname | H.ModuleName _ iname <- map H.importModule idecls]
			mfile = _preloadedId p ^?! moduleLocation . moduleFile
			projRoot = _preloadedId p ^? moduleLocation . moduleProject . _Just . projectPath
			mroot = fromMaybe
				(sourceModuleRoot (view moduleName $ _preloadedId p) mfile)
				projRoot
			dirs = do
				proj <- _preloadedId p ^.. moduleLocation . moduleProject . _Just
				i <- fileTargets proj mfile
				view infoSourceDirs i
			ifiles = [normPath (joinPaths [mroot, dir, importPath imod]) | imod <- imods, dir <- if null dirs then [fromFilePath "."] else dirs]

order :: [Preloaded] -> Either (DepsError Path) [Preloaded]
order = orderBy Just
