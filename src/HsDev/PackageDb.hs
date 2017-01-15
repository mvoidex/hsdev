{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module HsDev.PackageDb (
	PackageDb(..), packageDb,
	PackageDbStack(..), packageDbStack, globalDb, userDb, fromPackageDb, fromPackageDbs,
	topPackageDb, packageDbs, packageDbStacks,
	isSubStack,

	packageDbOpt, packageDbStackOpts
	) where

import Control.Applicative
import Control.Monad (guard)
import Control.Lens (makeLenses, each)
import Control.DeepSeq (NFData(..))
import Data.Aeson
import Data.List (tails, isSuffixOf)

import System.Directory.Paths
import HsDev.Util ((.::))

data PackageDb = GlobalDb | UserDb | PackageDb { _packageDb :: FilePath } deriving (Eq, Ord)

makeLenses ''PackageDb

instance NFData PackageDb where
	rnf GlobalDb = ()
	rnf UserDb = ()
	rnf (PackageDb p) = rnf p

instance Show PackageDb where
	show GlobalDb = "global-db"
	show UserDb = "user-db"
	show (PackageDb p) = "package-db:" ++ p

instance ToJSON PackageDb where
	toJSON GlobalDb = "global-db"
	toJSON UserDb = "user-db"
	toJSON (PackageDb p) = object ["package-db" .= p]

instance FromJSON PackageDb where
	parseJSON v = globalP v <|> userP v <|> dbP v where
		globalP = withText "global-db" (\s -> guard (s == "global-db") >> return GlobalDb)
		userP = withText "user-db" (\s -> guard (s == "user-db") >> return UserDb)
		dbP = withObject "package-db" pathP where
			pathP obj = PackageDb <$> obj .:: "package-db"

instance Paths PackageDb where
	paths _ GlobalDb = pure GlobalDb
	paths _ UserDb = pure UserDb
	paths f (PackageDb p) = PackageDb <$> f p

-- | Stack of PackageDb in reverse order
newtype PackageDbStack = PackageDbStack { _packageDbStack :: [PackageDb] } deriving (Eq, Ord, Show)

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

-- | Make package-db from one package-db
fromPackageDb :: FilePath -> PackageDbStack
fromPackageDb = PackageDbStack . return . PackageDb

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

-- | Is one package-db stack substack of another
isSubStack :: PackageDbStack -> PackageDbStack -> Bool
isSubStack (PackageDbStack l) (PackageDbStack r) = l `isSuffixOf` r

-- | Get ghc options for package-db
packageDbOpt :: PackageDb -> String
packageDbOpt GlobalDb = "-global-package-db"
packageDbOpt UserDb = "-user-package-db"
packageDbOpt (PackageDb p) = "-package-db " ++ p

-- | Get ghc options for package-db stack
packageDbStackOpts :: PackageDbStack -> [String]
packageDbStackOpts (PackageDbStack ps)
	| "-user-package-db" `elem` opts' = opts'
	| otherwise = "-no-user-package-db" : opts'
	where
		opts' = map packageDbOpt (reverse ps)
