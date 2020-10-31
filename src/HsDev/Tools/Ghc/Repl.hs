{-# LANGUAGE OverloadedStrings, TypeApplications, PackageImports #-}

module HsDev.Tools.Ghc.Repl (
	importModules, preludeModules,
	evaluate,
	expressionType,

	ReplResult(..),
	tryRepl
	) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Aeson
import Data.Dynamic
import Text.Format

import "ghc" GhcMonad
import "ghc" GHC

import HsDev.Error
import HsDev.Tools.Ghc.Base
import qualified HsDev.Tools.Ghc.Compat as C
import HsDev.Util

-- | Import some modules
importModules :: GhcMonad m => [String] -> m ()
importModules mods = mapM parseImportDecl ["import " ++ m | m <- mods] >>= setContext . map IIDecl

-- | Default interpreter modules
preludeModules :: [String]
preludeModules = ["Prelude", "Data.List", "Control.Monad"]

-- | Evaluate expression
evaluate :: GhcMonad m => String -> m String
evaluate expr = do
	v <- liftM fromDynamic (dynCompileExpr $ "show ({})" ~~ expr)
	maybe (liftIO $ hsdevError $ OtherError "evaluate fail") return v

-- | Get expression type as string
expressionType :: GhcMonad m => String -> m String
expressionType expr = do
	dflags <- getSessionDynFlags
	ty <- C.exprType expr
	return $ formatType dflags ty

data ReplResult a = ReplError String | ReplOk a deriving (Eq, Ord, Read, Show)

instance ToJSON a => ToJSON (ReplResult a) where
	toJSON (ReplError e) = object ["error" .= e]
	toJSON (ReplOk v) = object ["ok" .= v]

instance FromJSON a => FromJSON (ReplResult a) where
	parseJSON = withObject "repl-result" $ \v -> msum [
		ReplError <$> v .:: "error",
		ReplOk <$> v .:: "ok"]

tryRepl :: (GhcMonad m, MonadCatch m) => m a -> m (ReplResult a)
tryRepl = fmap (either (ReplError . displayException @SomeException) ReplOk) . try
