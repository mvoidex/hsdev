module HsDev.Scan (
	scanCabal,
	scanModule,
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
scanCabal cabal = do
	ms <- withConfig (config { configCabal = cabal }) list
	fmap mconcat $ mapM (scanModule cabal) ms

-- | Scan cabal module
scanModule :: Cabal -> String -> ErrorT String IO Database
scanModule cabal name = withConfig (config { configCabal = cabal }) $ do
	catchError (fmap fromModule $ browse' name) (const $ return mempty)
	where
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
