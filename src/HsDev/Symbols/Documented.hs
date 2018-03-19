{-# LANGUAGE DefaultSignatures, OverloadedStrings #-}

module HsDev.Symbols.Documented (
	Documented(..),
	defaultDetailed
	) where

import Control.Lens (view, (^..), (^?))
import Data.Maybe (maybeToList)
import Data.Text (Text, pack)
import qualified Data.Text as T

import Text.Format
import HsDev.Symbols.Class
import HsDev.Project.Types

-- | Documented symbol
class Documented a where
	brief :: a -> Text
	detailed :: a -> Text
	default detailed :: Sourced a => a -> Text
	detailed = T.unlines . defaultDetailed

-- | Default detailed docs
defaultDetailed :: (Sourced a, Documented a) => a -> [Text]
defaultDetailed s = concat [header, docs, loc] where
	header = [brief s, ""]
	docs = s ^.. sourcedDocs
	loc = maybe [] (\l -> ["Defined at " `T.append` pack (show l)]) (s ^? sourcedLocation)

instance Documented ModulePackage where
	brief = pack . show
	detailed = brief

instance Documented ModuleLocation where
	brief (FileModule f _) = f
	brief (InstalledModule _ pkg n _) = format "{} from {}" ~~ n ~~ brief pkg
	brief (OtherLocation src) = src
	brief NoLocation = "<no-location>"
	detailed (FileModule f mproj) = case mproj of
		Nothing -> f
		Just proj -> format "{} in project {}" ~~ f ~~ brief proj
	detailed (InstalledModule pdb pkg n _) = format "{} from {} ({})" ~~ n ~~ brief pkg ~~ show pdb
	detailed l = brief l

instance Documented Project where
	brief p = format "{} ({})" ~~ view projectName p ~~ view projectPath p
	detailed p = T.unlines (brief p : desc) where
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
