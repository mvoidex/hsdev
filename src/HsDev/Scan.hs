module HsDev.Scan (
	scanCabal,
	scanFile,
	scanProject
	) where

import Control.Monad.Error
import Data.Monoid

import HsDev.Symbols
import HsDev.Database
import HsDev.Tools.GhcMod
import HsDev.Tools.HDocs
import HsDev.Inspect
import HsDev.Project

-- | Scan cabal for modules
scanCabal :: Cabal -> ErrorT String IO Database
scanCabal cabal = withConfig (config { configCabal = cabal }) $ do
	ms <- list
	fmap mconcat $ mapM loadModule ms
	where
		loadModule s = catchError (fmap fromModule $ browse' s) (const $ return mempty)
		browse' s = do
			m <- browse s
			liftIO $ loadDocs [] m

-- | Scan file
scanFile :: FilePath -> ErrorT String IO Database
scanFile = fmap fromModule . inspectFile

-- | Scan project
scanProject :: Project -> ErrorT String IO Database
scanProject p = do
	(p', ms) <- inspectProject p
	return $ mconcat (fromProject p' : map fromModule ms)
