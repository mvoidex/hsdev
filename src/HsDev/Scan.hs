module HsDev.Scan (
	scanCabal,
	scanModule,
	scanFile,
	scanProject,
	rescanModule
	) where

import Control.Applicative
import Control.Monad.Error
import Data.Monoid

import HsDev.Symbols
import HsDev.Symbols.Util
import HsDev.Database
import HsDev.Tools.GhcMod
import HsDev.Tools.HDocs
import HsDev.Inspect
import HsDev.Project

-- | Scan cabal for modules
scanCabal :: Cabal -> ErrorT String IO Database
scanCabal cabal = do
	ms <- list
	fmap mconcat $ mapM (\m -> scanModule cabal m <|> return mempty) ms

-- | Scan cabal module
scanModule :: Cabal -> String -> ErrorT String IO Database
scanModule _ = fmap fromModule . (browse >=> liftIO . loadDocs [])

-- | Scan file
scanFile :: FilePath -> ErrorT String IO Database
scanFile = fmap fromModule . inspectFile

-- | Scan project
scanProject :: Project -> ErrorT String IO Database
scanProject p = do
	(p', ms) <- inspectProject p
	return $ mconcat (fromProject p' : map fromModule ms)

-- | Rescan module
rescanModule :: Symbol Module -> ErrorT String IO Database
rescanModule m
	| bySources m = do
		loc <- maybe (throwError "Location not specified") return $ symbolLocation m
		actual <- liftIO $ isActual m
		if actual then return mempty else scanFile (locationFile loc)
	| otherwise = return mempty
