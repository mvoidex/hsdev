module HsDev.Symbols.Location (
	ModuleLocation(..),
	Position(..),
	Location(..),
	Cabal(..)
	) where

import Control.DeepSeq
import Data.Maybe (fromMaybe)

-- | Location of module
data ModuleLocation =
	FileModule { moduleFile :: FilePath, moduleProject :: Maybe FilePath } |
	CabalModule { moduleCabal :: Cabal, modulePackage :: Maybe String, cabalModuleName :: String } |
	MemoryModule { moduleMemory :: Maybe String }
		deriving (Eq, Ord)

instance NFData ModuleLocation where
	rnf (FileModule f p) = rnf f `seq` rnf p
	rnf (CabalModule c p n) = rnf c `seq` rnf p `seq` rnf n
	rnf (MemoryModule m) = rnf m

instance Show ModuleLocation where
	show (FileModule f p) = f ++ maybe "" (" in " ++) p
	show (CabalModule c p _) = show c ++ maybe "" (" in package " ++) p
	show (MemoryModule m) = "<" ++ fromMaybe "null" m ++ ">"

data Position = Position {
	positionLine :: Int,
	positionColumn :: Int }
		deriving (Eq, Ord)

instance NFData Position where
	rnf (Position l c) = rnf l `seq` rnf c

instance Show Position where
 	show (Position l c) = show l ++ ":" ++ show c

-- | Location of symbol
data Location = Location {
	locationModule :: ModuleLocation,
	locationPosition :: Maybe Position }
		deriving (Eq, Ord)

instance NFData Location where
	rnf (Location m p) = rnf m `seq` rnf p

instance Show Location where
	show (Location m p) = show m ++ ":" ++ show p

-- | Cabal or sandbox
data Cabal = Cabal | Sandbox FilePath deriving (Eq, Ord)

instance NFData Cabal where
	rnf Cabal = ()
	rnf (Sandbox p) = rnf p

instance Show Cabal where
	show Cabal = "<cabal>"
	show (Sandbox p) = p
