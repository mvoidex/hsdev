{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module HsDev.Sandbox (
	SandboxType(..), Sandbox(..), sandboxType, sandbox,
	isSandbox, guessSandboxType, sandboxFromPath,
	findSandbox, searchSandbox, sandboxPackageDbStack, searchPackageDbStack, restorePackageDbStack,

	-- * cabal-sandbox util
	cabalSandboxLib, cabalSandboxPackageDb
	-- * stack-work util
	) where

import Control.Arrow
import Control.Applicative
import Control.DeepSeq (NFData(..))
import Control.Monad.Trans.Maybe
import Control.Monad.Except
import Control.Lens (view, makeLenses, Lens', lens)
import Data.Aeson
import Data.Maybe (isJust, fromMaybe)
import Data.List ((\\))
import qualified Data.Text as T (unpack)
import Distribution.Compiler
import Distribution.System
import qualified Distribution.Text as T (display)
import System.FilePath
import System.Directory

import System.Directory.Paths
import HsDev.PackageDb
import HsDev.Scan.Browse (withPackages)
import HsDev.Stack
import HsDev.Util (searchPath)

import qualified GHC
import qualified Packages as GHC

data SandboxType = CabalSandbox | StackWork deriving (Eq, Ord, Read, Show, Enum, Bounded)

data Sandbox = Sandbox { _sandboxType :: SandboxType, _sandbox :: FilePath } deriving (Eq, Ord)

makeLenses ''Sandbox

instance NFData SandboxType where
	rnf CabalSandbox = ()
	rnf StackWork = ()

instance NFData Sandbox where
	rnf (Sandbox t p) = rnf t `seq` rnf p

instance Show Sandbox where
	show (Sandbox _ p) = p

instance ToJSON Sandbox where
	toJSON (Sandbox _ p) = toJSON p

instance FromJSON Sandbox where
	parseJSON = withText "sandbox" sandboxPath where
		sandboxPath = maybe (fail "Not a sandbox") return . sandboxFromPath . T.unpack

instance Paths Sandbox where
	paths f (Sandbox st p) = Sandbox st <$> f p

isSandbox :: FilePath -> Bool
isSandbox = isJust . guessSandboxType

guessSandboxType :: FilePath -> Maybe SandboxType
guessSandboxType fpath
	| takeFileName fpath == ".cabal-sandbox" = Just CabalSandbox
	| takeFileName fpath == ".stack-work" = Just StackWork
	| otherwise = Nothing

sandboxFromPath :: FilePath -> Maybe Sandbox
sandboxFromPath fpath = Sandbox <$> guessSandboxType fpath <*> pure fpath

-- | Find sandbox in path
findSandbox :: FilePath -> IO (Maybe Sandbox)
findSandbox fpath = do
	fpath' <- canonicalize fpath
	isDir <- doesDirectoryExist fpath'
	if isDir
		then do
			dirs <- liftM ((fpath' :) . map (fpath' </>) . (\\ [".", ".."])) $ getDirectoryContents fpath'
			return $ msum $ map sandboxFromDir dirs
		else return Nothing
	where
		sandboxFromDir :: FilePath -> Maybe Sandbox
		sandboxFromDir fpath
			| takeFileName fpath == "stack.yaml" = sandboxFromPath (takeDirectory fpath </> ".stack-work")
			| otherwise = sandboxFromPath fpath

-- | Search sandbox by parent directory
searchSandbox :: FilePath -> IO (Maybe Sandbox)
searchSandbox p = runMaybeT $ searchPath p (MaybeT . findSandbox)

-- | Get package-db stack for sandbox
sandboxPackageDbStack :: Sandbox -> ExceptT String IO PackageDbStack
sandboxPackageDbStack (Sandbox CabalSandbox fpath) = do
	dir <- cabalSandboxPackageDb
	return $ PackageDbStack [PackageDb $ fpath </> dir]
sandboxPackageDbStack (Sandbox StackWork fpath) = maybeToExceptT "Can't locate stack environment" $
	liftM (view stackPackageDbStack) $ projectEnv $ takeDirectory fpath

-- | Search package-db stack with user-db as default
searchPackageDbStack :: FilePath -> IO PackageDbStack
searchPackageDbStack p = do
	mbox <- searchSandbox p
	case mbox of
		Nothing -> return userDb
		Just sbox -> liftM (either (const userDb) id) $ runExceptT $ sandboxPackageDbStack sbox
	where
		userDb = PackageDbStack [UserDb]

-- | Restore package-db stack by package-db
restorePackageDbStack :: PackageDb -> IO PackageDbStack
restorePackageDbStack GlobalDb = return globalDb
restorePackageDbStack UserDb = return userDb
restorePackageDbStack (PackageDb p) = liftM (fromMaybe $ fromPackageDb p) $ runMaybeT $ do
	sbox <- MaybeT $ searchSandbox p
	exceptToMaybeT $ sandboxPackageDbStack sbox

-- | Get actual sandbox build path: <arch>-<platform>-<compiler>-<version>
cabalSandboxLib :: ExceptT String IO FilePath
cabalSandboxLib = do
	res <- withPackages ["-no-user-package-db"] $
		return .
		map (GHC.packageNameString &&& GHC.packageVersion) .
		fromMaybe [] .
		GHC.pkgDatabase
	let
		compiler = T.display buildCompilerFlavor
		CompilerId _ version = buildCompilerId
		ver = maybe (T.display version) T.display $ lookup compiler res
	return $ T.display buildPlatform ++ "-" ++ compiler ++ "-" ++ ver

-- | Get sandbox package-db: <arch>-<platform>-<compiler>-<version>-packages.conf.d
cabalSandboxPackageDb :: ExceptT String IO FilePath
cabalSandboxPackageDb = liftM (++ "-packages.conf.d") cabalSandboxLib
