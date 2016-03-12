{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module HsDev.PackageDb (
	PackageDb(..), packageDb,
	PackageDbStack(..), packageDbStack, globalDb, userDb, fromPackageDbs,
	topPackageDb, packageDbs, packageDbStacks,

	packageDbOpt, packageDbStackOpts
	) where

import Control.Applicative
import Control.Monad (guard)
import Control.Lens (makeLenses, each)
import Control.DeepSeq (NFData(..))
import Data.Aeson
import Data.List (tails)

import System.Directory.Paths
import HsDev.Util ((.::))

data PackageDb = GlobalDb | UserDb | PackageDb { _packageDb :: FilePath } deriving (Eq, Ord, Read, Show)

makeLenses ''PackageDb

instance NFData PackageDb where
	rnf GlobalDb = ()
	rnf UserDb = ()
	rnf (PackageDb p) = rnf p

instance ToJSON PackageDb where
	toJSON GlobalDb = "global"
	toJSON UserDb = "user'"
	toJSON (PackageDb p) = object ["package-db" .= p]

instance FromJSON PackageDb where
	parseJSON v = globalP v <|> userP v <|> dbP v where
		globalP = withText "global" (\s -> guard (s == "global") >> return GlobalDb)
		userP = withText "user" (\s -> guard (s == "user") >> return UserDb)
		dbP = withObject "package-db" pathP where
			pathP obj = PackageDb <$> obj .:: "package-db"

instance Paths PackageDb where
	paths _ GlobalDb = pure GlobalDb
	paths _ UserDb = pure UserDb
	paths f (PackageDb p) = PackageDb <$> f p

-- | Stack of PackageDb in reverse order
newtype PackageDbStack = PackageDbStack { _packageDbStack :: [PackageDb] } deriving (Eq, Ord, Read, Show)

makeLenses ''PackageDbStack

instance NFData PackageDbStack where
	rnf (PackageDbStack ps) = rnf ps

instance ToJSON PackageDbStack where
	toJSON (PackageDbStack ps) = toJSON ps

instance FromJSON PackageDbStack where
	parseJSON = fmap PackageDbStack . parseJSON

instance Paths PackageDbStack where
	paths f (PackageDbStack ps) = PackageDbStack <$> (each . paths) f ps

-- | Global db stack
globalDb :: PackageDbStack
globalDb = PackageDbStack []

-- | User db stack
userDb :: PackageDbStack
userDb = PackageDbStack [UserDb]

-- | Make package-db stack from paths
fromPackageDbs :: [FilePath] -> PackageDbStack
fromPackageDbs = PackageDbStack . map PackageDb . reverse

-- | Get top package-db for package-db stack
topPackageDb :: PackageDbStack -> PackageDb
topPackageDb (PackageDbStack []) = GlobalDb
topPackageDb (PackageDbStack (d:_)) = d

-- | Get list of package-db in stack, adds additional global-db at bottom
packageDbs :: PackageDbStack -> [PackageDb]
packageDbs = (GlobalDb :) . reverse . _packageDbStack

-- | Get stacks for each package-db in stack
packageDbStacks :: PackageDbStack -> [PackageDbStack]
packageDbStacks = map PackageDbStack . tails . _packageDbStack

-- | Get ghc options for package-db
packageDbOpt :: PackageDb -> String
packageDbOpt GlobalDb = "-global-package-db"
packageDbOpt UserDb = "-user-package-db"
packageDbOpt (PackageDb p) = "-package-db " ++ p

-- | Get ghc options for package-db stack
packageDbStackOpts :: PackageDbStack -> [String]
packageDbStackOpts (PackageDbStack ps) = "-no-user-package-db" : map packageDbOpt (reverse ps)
