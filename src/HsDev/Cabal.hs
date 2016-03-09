{-# LANGUAGE OverloadedStrings, MultiWayIf, FlexibleContexts, TemplateHaskell #-}

module HsDev.Cabal (
	Cabal(..), sandbox, SandboxStack, sandboxStack, sandboxStacks, sandboxCabals, topSandbox,
	isPackageDb, findPackageDb, locateSandbox, getSandbox, searchSandbox,
	cabalOpt, sandboxStackOpt
	) where

import Control.Applicative
import Control.DeepSeq (NFData(..))
import Control.Monad.Except
import Control.Lens (makeLenses, Lens', lens)
import Data.Aeson
import Data.List
import System.Directory
import System.FilePath

import HsDev.Util (searchPath, liftE)

-- | Cabal or sandbox
data Cabal = Cabal | Sandbox { _sandbox :: FilePath } deriving (Eq, Ord)

makeLenses ''Cabal

type SandboxStack = [FilePath]

sandboxStack :: Cabal -> SandboxStack
sandboxStack Cabal = []
sandboxStack (Sandbox f) = [f]

sandboxStacks :: SandboxStack -> [SandboxStack]
sandboxStacks = tails

sandboxCabals :: SandboxStack -> [Cabal]
sandboxCabals = (Cabal :) . map Sandbox . reverse

topSandbox :: Lens' SandboxStack Cabal
topSandbox = lens g s where
	g :: SandboxStack -> Cabal
	g [] = Cabal
	g (f:_) = Sandbox f
	s :: SandboxStack -> Cabal -> SandboxStack
	s _ Cabal = []
	s [] (Sandbox f) = [f]
	s (_:fs) (Sandbox f) = f:fs

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
isPackageDb p = cabalDev p || cabalSandbox p || stackDb p where
	cabalDev dir = "packages-" `isPrefixOf` takeFileName dir && ".conf" `isSuffixOf` takeFileName dir
	cabalSandbox dir = "-packages.conf.d" `isSuffixOf` takeFileName dir
	stackDb dir = takeFileName dir == "pkgdb"

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
locateSandbox :: FilePath -> ExceptT String IO Cabal
locateSandbox p = liftE (findPackageDb p) >>= maybe
	(throwError $ "Can't locate package-db in sandbox: " ++ p)
	(return . Sandbox)

-- | Try find sandbox by parent directory
getSandbox :: FilePath -> IO Cabal
getSandbox = liftM (either (const Cabal) id) . runExceptT . locateSandbox

-- | Search sandbox
searchSandbox :: FilePath -> IO Cabal
searchSandbox p = runExceptT (searchPath p locateSandbox) >>= either (const $ return Cabal) return

-- | Cabal ghc option
cabalOpt :: Cabal -> [String]
cabalOpt Cabal = []
cabalOpt (Sandbox p) = ["-package-db " ++ p]

sandboxStackOpt :: SandboxStack -> [String]
sandboxStackOpt [] = []
sandboxStackOpt paths' = "-no-user-package-db" : reverse ["-package-db " ++ p | p <- paths']
