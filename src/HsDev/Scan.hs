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
scanCabal :: [String] -> Cabal -> ErrorT String IO Database
scanCabal opts cabal = do
	ms <- list opts
	fmap mconcat $ mapM (\m -> scanModule opts cabal m <|> return mempty) ms

-- | Scan cabal module
scanModule :: [String] -> Cabal -> String -> ErrorT String IO Database
scanModule opts _ = fmap fromModule . (browse opts >=> liftIO . loadDocs opts)

-- | Scan file
scanFile :: [String] -> FilePath -> ErrorT String IO Database
scanFile opts = fmap fromModule . inspectFile opts

-- | Scan project
scanProject :: [String] -> Project -> ErrorT String IO Database
scanProject opts p = do
	(p', ms) <- inspectProject opts p
	return $ mconcat (fromProject p' : map fromModule ms)

-- | Rescan module
rescanModule :: [String] -> Symbol Module -> ErrorT String IO Database
rescanModule opts m
	| bySources m = do
		loc <- maybe (throwError "Location not specified") return $ symbolLocation m
		actual <- liftIO $ isActual m
		if actual then return mempty else scanFile opts (locationFile loc)
	| otherwise = return mempty
