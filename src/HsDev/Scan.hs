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
scanModule opts _ = fmap fromModule . (browse opts >=> loadDocs') where
	loadDocs' m = case inspectionResult m of
		Left _ -> return m
		Right m' -> do
			m'' <- liftIO (loadDocs opts m')
			return $ m {
				inspectionResult = Right m'' }

-- | Scan file
scanFile :: [String] -> FilePath -> ErrorT String IO Database
scanFile opts = fmap fromModule . inspectFile opts

-- | Scan project
scanProject :: [String] -> Project -> ErrorT String IO Database
scanProject opts p = do
	(p', ms) <- inspectProject opts p
	return $ mconcat (fromProject p' : map fromModule ms)

-- | Rescan module
rescanModule :: [String] -> InspectedModule -> ErrorT String IO Database
rescanModule opts (InspectedModule i m _) = case m of
	FileModule f _ -> do
		i' <- fileInspection f
		if i == i' then return mempty else scanFile opts f
	CabalModule c _ n -> do
		i' <- browseInspection opts n
		if i == i' then return mempty else scanModule opts c n
	MemoryModule _ -> return mempty
