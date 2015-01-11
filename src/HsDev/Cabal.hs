{-# LANGUAGE OverloadedStrings, MultiWayIf #-}

module HsDev.Cabal (
	Cabal(..), sandbox,
	isPackageDb, findPackageDb, locateSandbox, getSandbox, searchSandbox,
	cabalOpt
	) where

import Control.Applicative
import Control.DeepSeq (NFData(..))
import Control.Monad.Error
import Data.Aeson
import Data.List
import System.Directory
import System.FilePath

import HsDev.Util (searchPath)

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

-- | Is -package-db file
isPackageDb :: FilePath -> Bool
isPackageDb p = cabalDev p || cabalSandbox p where
	cabalDev dir = "packages-" `isPrefixOf` dir && ".conf" `isSuffixOf` dir
	cabalSandbox dir = "-packages.conf.d" `isSuffixOf` dir

-- | Is .cabal-sandbox
cabalSandboxDir :: FilePath -> Bool
cabalSandboxDir p = takeFileName p == ".cabal-sandbox"

-- | Find -package-db path for sandbox directory or package-db file itself
findPackageDb :: FilePath -> IO (Maybe FilePath)
findPackageDb sand = do
	sand' <- canonicalizePath sand
	isDir <- doesDirectoryExist sand'
	if
		| isDir && isPackageDb sand' -> return $ Just sand'
		| isDir -> do
			cts <- getDirectoryContents sand'
			(dir', cts') <- case find cabalSandboxDir cts of
				Nothing -> return (sand', cts)
				Just sbox -> (,) <$> pure (sand' </> sbox) <*> getDirectoryContents (sand' </> sbox)
			return $ fmap (dir' </>) $ find isPackageDb cts'
		| otherwise -> return Nothing

-- | Create sandbox by directory or package-db file
locateSandbox :: FilePath -> ErrorT String IO Cabal
locateSandbox p = liftIO (findPackageDb p) >>= maybe
	(throwError $ "Can't locate package-db in sandbox: " ++ p)
	(return . Sandbox)

-- | Try find sandbox by parent directory
getSandbox :: FilePath -> IO Cabal
getSandbox = liftM (either (const Cabal) id) . runErrorT . locateSandbox

-- | Search sandbox
searchSandbox :: FilePath -> IO Cabal
searchSandbox p = runErrorT (searchPath p locateSandbox) >>= either (const $ return Cabal) return

-- | Cabal ghc option
cabalOpt :: Cabal -> [String]
cabalOpt Cabal = []
cabalOpt (Sandbox p) = ["-package-db " ++ p]
