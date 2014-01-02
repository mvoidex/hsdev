{-# LANGUAGE OverloadedStrings #-}

module HsDev.Cabal (
	Cabal(..),
	findPackageDb, locateSandbox,
	cabalOpt
	) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Error
import Data.Aeson
import Data.List
import Data.Maybe (maybeToList)
import System.Directory
import System.FilePath

-- | Cabal or sandbox
data Cabal = Cabal | Sandbox FilePath deriving (Eq, Ord)

instance NFData Cabal where
	rnf Cabal = ()
	rnf (Sandbox p) = rnf p

instance Show Cabal where
	show Cabal = "<cabal>"
	show (Sandbox p) = p

instance ToJSON Cabal where
	toJSON Cabal = toJSON ("cabal" :: String)
	toJSON (Sandbox p) = toJSON $ object [
		"sandbox" .= p]

instance FromJSON Cabal where
	parseJSON v = cabalP v <|> sandboxP v where
		cabalP = withText "cabal" cabalText where
			cabalText "cabal" = return Cabal
			cabalText _ = fail "Unknown cabal string"
		sandboxP = withObject "sandbox" sandboxPath where
			sandboxPath obj = fmap Sandbox $ obj .: "sandbox"

-- | Find -package-db path for sandbox
findPackageDb :: FilePath -> IO (Maybe FilePath)
findPackageDb sand = do
	cts <- getDirectoryContents sand
	return $ fmap (sand </>) $
		find cabalDev cts <|> find cabalSandbox cts
	where
		cabalDev p = "packages-" `isPrefixOf` p && ".conf" `isSuffixOf` p
		cabalSandbox p = "-packages.conf.d" `isSuffixOf` p

-- | Create sandbox by parent directory
locateSandbox :: FilePath -> ErrorT String IO Cabal
locateSandbox p = liftIO (findPackageDb p) >>= maybe
	(throwError $ "Can't locate package-db in sandbox: " ++ p)
	(return . Sandbox)

-- | Cabal ghc option
cabalOpt :: Cabal -> [String]
cabalOpt Cabal = []
cabalOpt (Sandbox p) = ["-package-db " ++ p]
