module HsDev.Inspect.Order (
	order
	) where

import Control.Lens
import Data.List
import Data.Maybe
import Data.String
import qualified Data.Set as S
import qualified Language.Haskell.Exts as H

import Data.Deps
import HsDev.Inspect
import HsDev.Symbols.Types
import System.Directory.Paths

-- | Order source files so that dependencies goes first and we are able to resolve symbols and set fixities
order :: [Preloaded] -> Either (DepsError Path) [Preloaded]
order ps = do
	order' <- linearize pdeps
	return $ mapMaybe byFile order'
	where
		pdeps = mconcat $ map getDeps ps
		byFile f = find (\p -> (_preloadedId p ^? moduleLocation . moduleFile) == Just f) ps
		files = S.fromList $ map _preloadedId ps ^.. each . moduleLocation . moduleFile
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
