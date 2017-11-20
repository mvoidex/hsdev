{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module HsDev.PackageDb.Types (
	PackageDb(..), packageDb,
	PackageDbStack(..), packageDbStack, globalDb, userDb, fromPackageDbs,
	topPackageDb, packageDbs, packageDbStacks,
	isSubStack,

	packageDbOpt, packageDbStackOpts
	) where

import Control.Applicative
import Control.Monad (guard)
import Control.Lens (makeLenses, each, (^.))
import Control.DeepSeq (NFData(..))
import Data.Aeson
import Data.List (tails, isSuffixOf)
import qualified Data.Text as T
import Data.String

import System.Directory.Paths

data PackageDb = GlobalDb | UserDb | PackageDb { _packageDb :: Path } deriving (Eq, Ord)

makeLenses ''PackageDb

instance NFData PackageDb where
	rnf GlobalDb = ()
	rnf UserDb = ()
	rnf (PackageDb p) = rnf p

instance Show PackageDb where
	show GlobalDb = "global-db"
	show UserDb = "user-db"
	show (PackageDb p) = "package-db:" ++ p ^. path

instance ToJSON PackageDb where
	toJSON GlobalDb = "global-db"
	toJSON UserDb = "user-db"
	toJSON (PackageDb p) = fromString $ "package-db:" ++ p ^. path

instance FromJSON PackageDb where
	parseJSON v = globalP v <|> userP v <|> dbP v where
		globalP = withText "global-db" (\s -> guard (s == "global-db") >> return GlobalDb)
		userP = withText "user-db" (\s -> guard (s == "user-db") >> return UserDb)
		dbP = withText "package-db" $ \s -> case T.stripPrefix "package-db:" s of
			Nothing -> fail ("Can't parse package-db: " ++ T.unpack s)
			Just p' -> return $ PackageDb p'

instance Paths PackageDb where
	paths _ GlobalDb = pure GlobalDb
	paths _ UserDb = pure UserDb
	paths f (PackageDb p) = PackageDb <$> paths f p

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

-- | Make package-db stack from paths
fromPackageDbs :: [Path] -> PackageDbStack
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
packageDbOpt (PackageDb p) = "-package-db " ++ p ^. path

-- | Get ghc options for package-db stack
packageDbStackOpts :: PackageDbStack -> [String]
packageDbStackOpts (PackageDbStack ps)
	| "-user-package-db" `elem` opts' = opts'
	| otherwise = "-no-user-package-db" : opts'
	where
		opts' = map packageDbOpt (reverse ps)
