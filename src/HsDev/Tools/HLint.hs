module HsDev.Tools.HLint (
	hlint
	) where

import Control.Monad.Except
import Language.Haskell.HLint3

hlint :: FilePath -> ExceptT String IO [Idea]
hlint file = do
	(flags, classify, hint) <- liftIO autoSettings
	p <- liftIO $ parseModuleEx flags file Nothing
	m <- either (throwError . parseErrorMessage) return p
	return $ applyHints classify hint [m]
