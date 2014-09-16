{-# LANGUAGE OverloadedStrings #-}

module HsDev.Cabal (
	Cabal(..), sandbox,
	findPackageDb, locateSandbox, getSandbox,
	cabalOpt
	) where

import Control.Applicative
import Control.DeepSeq (NFData(..))
import Control.Monad.Error
import Data.Aeson
import Data.List
import System.Directory
import System.FilePath

-- | Cabal or sandbox
data Cabal = Cabal | Sandbox FilePath deriving (Eq, Ord)

-- | Get sandbox
sandbox :: Cabal -> Maybe FilePath
sandbox Cabal = Nothing
sandbox (Sandbox f) = Just f

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
	sand' <- canonicalizePath sand
	isDir <- doesDirectoryExist sand'
	if isDir then locateHere sand' else locateParent (takeDirectory sand')
	where
		locateHere path = do
			cts <- filter (not . null . takeBaseName) <$> getDirectoryContents path
			return $ fmap (path </>) $ find cabalDev cts <|> find cabalSandbox cts
		locateParent dir = do
			cts <- filter (not . null . takeBaseName) <$> getDirectoryContents dir
			case find cabalDev cts <|> find cabalSandbox cts of
				Nothing -> if isDrive dir then return Nothing else locateParent (takeDirectory dir)
				Just packagef -> return $ Just (dir </> packagef)
		cabalDev p = "packages-" `isPrefixOf` p && ".conf" `isSuffixOf` p
		cabalSandbox p = "-packages.conf.d" `isSuffixOf` p

-- | Create sandbox by parent directory
locateSandbox :: FilePath -> ErrorT String IO Cabal
locateSandbox p = liftIO (findPackageDb p) >>= maybe
	(throwError $ "Can't locate package-db in sandbox: " ++ p)
	(return . Sandbox)

-- | Try find sandbox by parent directory
getSandbox :: FilePath -> IO Cabal
getSandbox = liftM (either (const Cabal) id) . runErrorT . locateSandbox

-- | Cabal ghc option
cabalOpt :: Cabal -> [String]
cabalOpt Cabal = []
cabalOpt (Sandbox p) = ["-package-db " ++ p]
