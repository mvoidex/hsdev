{-# LANGUAGE OverloadedStrings #-}

module HsDev.Symbols.Location (
	ModulePackage(..), ModuleLocation(..), moduleSource,
	Position(..),
	Location(..),

	packageOpt,

	module HsDev.Cabal
	) where

import Control.Applicative
import Control.DeepSeq (NFData(..))
import Control.Monad (join)
import Data.Aeson
import Data.Char (isSpace, isDigit)
import Data.Maybe
import Text.Read (readMaybe)

import HsDev.Cabal
import HsDev.Project
import HsDev.Util ((.::))

data ModulePackage = ModulePackage {
	packageName :: String,
	packageVersion :: String }
		deriving (Eq, Ord)

instance NFData ModulePackage where
	rnf (ModulePackage n v) = rnf n `seq` rnf v

instance Show ModulePackage where
	show (ModulePackage n "") = n
	show (ModulePackage n v) = n ++ "-" ++ v

instance Read ModulePackage where
	readsPrec _ str = case pkg of
		"" -> []
		_ -> [(ModulePackage n v, str')]
		where
			(pkg, str') = break isSpace str
			(rv, rn) = span versionChar $ reverse pkg
			v = reverse rv
			n = reverse $ dropWhile (== '-') rn

			versionChar ch = isDigit ch || ch == '.'


-- | Location of module
data ModuleLocation =
	FileModule { moduleFile :: FilePath, moduleProject :: Maybe Project } |
	CabalModule { moduleCabal :: Cabal, modulePackage :: Maybe ModulePackage, cabalModuleName :: String } |
	OtherModuleSource { moduleOtherSource :: Maybe String }
		deriving (Eq, Ord)

moduleSource :: ModuleLocation -> Maybe FilePath
moduleSource (FileModule f _) = Just f
moduleSource _ = Nothing

instance NFData ModuleLocation where
	rnf (FileModule f p) = rnf f `seq` rnf p
	rnf (CabalModule c p n) = rnf c `seq` rnf p `seq` rnf n
	rnf (OtherModuleSource m) = rnf m

instance Show ModuleLocation where
	show (FileModule f p) = f ++ maybe "" (" in " ++) (fmap projectPath p)
	show (CabalModule c p _) = show c ++ maybe "" (" in package " ++) (fmap show p)
	show (OtherModuleSource m) = fromMaybe "" m

instance ToJSON ModuleLocation where
	toJSON (FileModule f p) = object ["file" .= f, "project" .= fmap projectCabal p]
	toJSON (CabalModule c p n) = object ["cabal" .= c, "package" .= fmap show p, "name" .= n]
	toJSON (OtherModuleSource s) = object ["source" .= s]

instance FromJSON ModuleLocation where
	parseJSON = withObject "module location" $ \v ->
		(FileModule <$> v .:: "file" <*> ((fmap project) <$> (v .:: "project"))) <|>
		(CabalModule <$> v .:: "cabal" <*> (fmap (join . fmap readMaybe) (v .:: "package")) <*> v .:: "name") <|>
		(OtherModuleSource <$> v .:: "source")

data Position = Position {
	positionLine :: Int,
	positionColumn :: Int }
		deriving (Eq, Ord)

instance NFData Position where
	rnf (Position l c) = rnf l `seq` rnf c

instance Show Position where
 	show (Position l c) = show l ++ ":" ++ show c

instance ToJSON Position where
	toJSON (Position l c) = object [
		"line" .= l,
		"column" .= c]

instance FromJSON Position where
	parseJSON = withObject "position" $ \v -> Position <$>
		v .:: "line" <*>
		v .:: "column"

-- | Location of symbol
data Location = Location {
	locationModule :: ModuleLocation,
	locationPosition :: Maybe Position }
		deriving (Eq, Ord)

instance NFData Location where
	rnf (Location m p) = rnf m `seq` rnf p

instance Show Location where
	show (Location m p) = show m ++ ":" ++ show p

instance ToJSON Location where
	toJSON (Location ml p) = object [
		"module" .= ml,
		"pos" .= p]

instance FromJSON Location where
	parseJSON = withObject "location" $ \v -> Location <$>
		v .:: "module" <*>
		v .:: "pos"

packageOpt :: Maybe ModulePackage -> [String]
packageOpt = maybeToList . fmap (("-package " ++) . packageName)
