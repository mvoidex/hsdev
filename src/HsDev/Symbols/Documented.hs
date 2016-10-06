{-# LANGUAGE DefaultSignatures #-}

module HsDev.Symbols.Documented (
	Documented(..),
	defaultDetailed
	) where

import Control.Lens (view, (^..), (^?))
import Data.Maybe (maybeToList)
import Data.Text (unpack)

import Text.Format
import HsDev.Symbols.Class
import HsDev.Project.Types

-- | Documented symbol
class Documented a where
	brief :: a -> String
	detailed :: a -> String
	default detailed :: Sourced a => a -> String
	detailed = unlines . defaultDetailed

-- | Default detailed docs
defaultDetailed :: (Sourced a, Documented a) => a -> [String]
defaultDetailed s = concat [header, docs, loc] where
	header = [brief s, ""]
	docs = map unpack $ s ^.. sourcedDocs
	loc
		| null mloc = []
		| otherwise = ["Defined at " ++ mloc]
	mloc = maybe "" show (s ^? sourcedLocation)

instance Documented ModulePackage where
	brief = show
	detailed = brief

instance Documented ModuleLocation where
	brief (FileModule f _) = f
	brief (InstalledModule _ mpkg n) = case mpkg of
		Nothing -> n
		Just pkg -> format "{} from {}" ~~ n ~~ brief pkg
	brief (OtherLocation src) = src
	brief NoLocation = "<no-location>"
	detailed (FileModule f mproj) = case mproj of
		Nothing -> f
		Just proj -> format "{} in project {}" ~~ f ~~ brief proj
	detailed (InstalledModule pdb mpkg n) = case mpkg of
		Nothing -> format "{} ({})" ~~ n ~~ show pdb
		Just pkg ->format  "{} from {} ({})" ~~ n ~~ brief pkg ~~ show pdb
	detailed l = brief l

instance Documented Project where
	brief p = format "{} ({})" ~~ view projectName p ~~ view projectPath p
	detailed p = unlines (brief p : desc) where
		desc = concat [
			do
				d <- mdescr
				_ <- maybeToList $ view projectLibrary d
				return "\tlibrary",
			do
				d <- mdescr
				exe <- view projectExecutables d
				return $ format "\texecutable: {}" ~~ view executableName exe,
			do
				d <- mdescr
				test <- view projectTests d
				return $ format "\ttest: {}" ~~ view testName test]
		mdescr = maybeToList $ view projectDescription p
