{-# LANGUAGE OverloadedStrings #-}

module HsDev.Symbols.Location (
	ModuleLocation(..), moduleSource,
	Position(..),
	Location(..),
	Cabal(..)
	) where

import Control.Applicative
import Control.DeepSeq
import Data.Aeson
import Data.Maybe (fromMaybe)

import HsDev.Util ((.::))

-- | Location of module
data ModuleLocation =
	FileModule { moduleFile :: FilePath, moduleProject :: Maybe FilePath } |
	CabalModule { moduleCabal :: Cabal, modulePackage :: Maybe String, cabalModuleName :: String } |
	MemoryModule { moduleMemory :: Maybe String }
		deriving (Eq, Ord)

moduleSource :: ModuleLocation -> Maybe FilePath
moduleSource (FileModule f _) = Just f
moduleSource _ = Nothing

instance NFData ModuleLocation where
	rnf (FileModule f p) = rnf f `seq` rnf p
	rnf (CabalModule c p n) = rnf c `seq` rnf p `seq` rnf n
	rnf (MemoryModule m) = rnf m

instance Show ModuleLocation where
	show (FileModule f p) = f ++ maybe "" (" in " ++) p
	show (CabalModule c p _) = show c ++ maybe "" (" in package " ++) p
	show (MemoryModule m) = "<" ++ fromMaybe "null" m ++ ">"

instance ToJSON ModuleLocation where
	toJSON (FileModule f p) = object ["file" .= f, "project" .= p]
	toJSON (CabalModule c p n) = object ["cabal" .= c, "package" .= p, "name" .= n]
	toJSON (MemoryModule s) = object ["mem" .= s]

instance FromJSON ModuleLocation where
	parseJSON = withObject "module location" $ \v ->
		(FileModule <$> v .:: "file" <*> v .:: "project") <|>
		(CabalModule <$> v .:: "cabal" <*> v .:: "package" <*> v .:: "name") <|>
		(MemoryModule <$> v .:: "mem")

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

-- | Cabal or sandbox
data Cabal = Cabal | Sandbox FilePath deriving (Eq, Ord)

instance NFData Cabal where
	rnf Cabal = ()
	rnf (Sandbox p) = rnf p

instance Show Cabal where
	show Cabal = "<cabal>"
	show (Sandbox p) = p

instance ToJSON Cabal where
	toJSON Cabal = toJSON ("<cabal>" :: String)
	toJSON (Sandbox p) = toJSON p

instance FromJSON Cabal where
	parseJSON v = do
		p <- parseJSON v
		return $ if p == "<cabal>" then Cabal else Sandbox p
