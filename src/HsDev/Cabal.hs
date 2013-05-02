module HsDev.Cabal (
	scan
	) where

import Control.Monad.Error
import Data.Monoid

import HsDev.Symbols
import HsDev.Database
import HsDev.Tools.GhcMod

-- | Scan cabal for modules
scan :: Cabal -> ErrorT String IO Database
scan cabal = withConfig (config { configCabal = cabal }) $ do
	ms <- list
	fmap mconcat $ mapM loadModule ms
	where
		loadModule s = catchError (fmap moduleDatabase $ browse s) (const $ return mempty)
