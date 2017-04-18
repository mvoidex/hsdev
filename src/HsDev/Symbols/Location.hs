{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module HsDev.Symbols.Location (
	ModulePackage(..), mkPackage, PackageConfig(..), ModuleLocation(..), moduleStandalone, locationId, noLocation,
	ModuleId(..), moduleName, moduleLocation,
	SymbolId(..), symbolName, symbolModule,
	Position(..), Region(..), region, regionAt, regionLines, regionStr,
	Location(..),

	packageName, packageVersion,
	package, packageModules, packageExposed,
	moduleFile, moduleProject, moduleInstallDirs, modulePackage, installedModuleName, otherLocationName,
	positionLine, positionColumn,
	regionFrom, regionTo,
	locationModule, locationPosition,

	sourceModuleRoot,
	importPath,
	moduleNameByFile,
	packageOpt,
	RecalcTabs(..),

	module HsDev.PackageDb.Types
	) where

import Control.Applicative
import Control.DeepSeq (NFData(..))
import Control.Lens (makeLenses, preview, view, (^..), (^.), each, _Just)
import Control.Monad (msum, mplus)
import Data.Aeson
import Data.Char (isSpace, isDigit)
import Data.List (intercalate, findIndex, stripPrefix)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T (split, unpack)
import System.FilePath
import Text.Read (readMaybe)

import System.Directory.Paths
import HsDev.PackageDb.Types
import HsDev.Project.Types
import HsDev.Util ((.::), (.::?!), objectUnion, ordNub)

-- | Just package name and version without its location
data ModulePackage = ModulePackage {
	_packageName :: String,
	_packageVersion :: String }
		deriving (Eq, Ord)

makeLenses ''ModulePackage

mkPackage :: String -> ModulePackage
mkPackage n = ModulePackage n ""

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

instance ToJSON ModulePackage where
	toJSON (ModulePackage n v) = object [
		"name" .= n,
		"version" .= v]

instance FromJSON ModulePackage where
	parseJSON = withObject "module package" $ \v ->
		ModulePackage <$> (v .:: "name") <*> (v .:: "version")

data PackageConfig = PackageConfig {
	_package :: ModulePackage,
	_packageModules :: [Text],
	_packageExposed :: Bool }
		deriving (Eq, Ord, Read, Show)

makeLenses ''PackageConfig

instance NFData PackageConfig where
	rnf (PackageConfig p ms e) = rnf p `seq` rnf ms `seq` rnf e

instance ToJSON PackageConfig where
	toJSON (PackageConfig p ms e) = toJSON p `objectUnion` object ["modules" .= ms, "exposed" .= e]

instance FromJSON PackageConfig where
	parseJSON = withObject "package-config" $ \v -> PackageConfig <$>
		parseJSON (Object v) <*>
		(v .::?! "modules") <*>
		(v .:: "exposed" <|> pure False)

-- | Location of module
data ModuleLocation =
	FileModule { _moduleFile :: FilePath, _moduleProject :: Maybe Project } |
	InstalledModule { _moduleInstallDirs :: [FilePath], _modulePackage :: Maybe ModulePackage, _installedModuleName :: String } |
	OtherLocation { _otherLocationName :: String } |
	NoLocation

instance Eq ModuleLocation where
	FileModule lfile _ == FileModule rfile _ = lfile == rfile
	InstalledModule ldirs _ _ == InstalledModule rdirs _ _ = ldirs == rdirs
	OtherLocation l == OtherLocation r = l == r
	NoLocation == NoLocation = True
	_ == _ = False

instance Ord ModuleLocation where
	compare l r = compare (locType l, locNames l) (locType r, locNames r) where
		locType :: ModuleLocation -> Int
		locType (FileModule _ _) = 0
		locType (InstalledModule _ _ _) = 1
		locType (OtherLocation _) = 2
		locType NoLocation = 3
		locNames (FileModule f _) = [f]
		locNames (InstalledModule dirs _ nm) = nm : dirs
		locNames (OtherLocation n) = [n]
		locNames NoLocation = []

makeLenses ''ModuleLocation

moduleStandalone :: ModuleLocation -> Bool
moduleStandalone = (== Just Nothing) . preview moduleProject

locationId :: ModuleLocation -> String
locationId (FileModule fpath _) = fpath
locationId (InstalledModule dirs mpack nm) = intercalate ":" (take 1 dirs ++ [maybe "" show mpack, nm])
locationId (OtherLocation src) = src
locationId NoLocation = "<no-location>"

instance NFData ModuleLocation where
	rnf (FileModule f p) = rnf f `seq` rnf p
	rnf (InstalledModule d p n) = rnf d `seq` rnf p `seq` rnf n
	rnf (OtherLocation s) = rnf s
	rnf NoLocation = ()

instance Show ModuleLocation where
	show = locationId

instance ToJSON ModuleLocation where
	toJSON (FileModule f p) = object ["file" .= f, "project" .= fmap (view projectCabal) p]
	toJSON (InstalledModule c p n) = object ["dirs" .= c, "package" .= fmap show p, "name" .= n]
	toJSON (OtherLocation s) = object ["source" .= s]
	toJSON NoLocation = object []

instance FromJSON ModuleLocation where
	parseJSON = withObject "module location" $ \v ->
		(FileModule <$> v .:: "file" <*> (fmap project <$> (v .:: "project"))) <|>
		(InstalledModule <$> v .::?! "dirs" <*> ((v .:: "package") >>= traverse readPackage) <*> v .:: "name") <|>
		(OtherLocation <$> v .:: "source") <|>
		(pure NoLocation)
		where
			readPackage s = maybe (fail $ "can't parse package: " ++ s) return . readMaybe $ s

instance Paths ModuleLocation where
	paths f (FileModule fpath p) = FileModule <$> f fpath <*> traverse (paths f) p
	paths f (InstalledModule c p n) = InstalledModule <$> traverse f c <*> pure p <*> pure n
	paths _ (OtherLocation s) = pure $ OtherLocation s
	paths _ NoLocation = pure NoLocation

noLocation :: ModuleLocation
noLocation = NoLocation

data ModuleId = ModuleId {
	_moduleName :: Text,
	_moduleLocation :: ModuleLocation }
		deriving (Eq, Ord)

makeLenses ''ModuleId

instance NFData ModuleId where
	rnf (ModuleId n l) = rnf n `seq` rnf l

instance Show ModuleId where
	show (ModuleId n l) = show l ++ ":" ++ T.unpack n

instance ToJSON ModuleId where
	toJSON m = object [
		"name" .= _moduleName m,
		"location" .= _moduleLocation m]

instance FromJSON ModuleId where
	parseJSON = withObject "module-id" $ \v -> ModuleId <$>
		v .:: "name" <*>
		v .:: "location"

-- | Symbol
data SymbolId = SymbolId {
	_symbolName :: Text,
	_symbolModule :: ModuleId }
		deriving (Eq, Ord)

makeLenses ''SymbolId

instance NFData SymbolId where
	rnf (SymbolId n m) = rnf n `seq` rnf m

instance Show SymbolId where
	show (SymbolId n m) = show m ++ ":" ++ T.unpack n

instance ToJSON SymbolId where
	toJSON s = object [
		"name" .= _symbolName s,
		"module" .= _symbolModule s]

instance FromJSON SymbolId where
	parseJSON = withObject "symbol-id" $ \v -> SymbolId <$>
		v .:: "name" <*>
		v .:: "module"

data Position = Position {
	_positionLine :: Int,
	_positionColumn :: Int }
		deriving (Eq, Ord, Read)

makeLenses ''Position

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

data Region = Region {
	_regionFrom :: Position,
	_regionTo :: Position }
		deriving (Eq, Ord, Read)

makeLenses ''Region

region :: Position -> Position -> Region
region f t = Region (min f t) (max f t)

regionAt :: Position -> Region
regionAt f = region f f

regionLines :: Region -> Int
regionLines (Region f t) = succ (view positionLine t - view positionLine f)

-- | Get string at region
regionStr :: Region -> String -> String
regionStr r@(Region f t) s = intercalate "\n" $ drop (pred $ view positionColumn f) fline : tl where
	s' = take (regionLines r) $ drop (pred (view positionLine f)) $ lines s
	(fline:tl) = init s' ++ [take (pred $ view positionColumn t) (last s')]

instance NFData Region where
	rnf (Region f t) = rnf f `seq` rnf t

instance Show Region where
	show (Region f t) = show f ++ "-" ++ show t

instance ToJSON Region where
	toJSON (Region f t) = object [
		"from" .= f,
		"to" .= t]

instance FromJSON Region where
	parseJSON = withObject "region" $ \v -> Region <$>
		v .:: "from" <*>
		v .:: "to"

-- | Location of symbol
data Location = Location {
	_locationModule :: ModuleLocation,
	_locationPosition :: Maybe Position }
		deriving (Eq, Ord)

makeLenses ''Location

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

-- | Get source module root directory, i.e. for "...\src\Foo\Bar.hs" with module 'Foo.Bar' will return "...\src"
sourceModuleRoot :: Text -> FilePath -> FilePath
sourceModuleRoot mname = 
	normalise . joinPath .
	reverse . drop (length $ T.split (== '.') mname) . reverse .
	splitDirectories

-- | Path to module source
-- >importPath "Quux.Blah" = "Quux/Blah.hs"
importPath :: Text -> FilePath
importPath = (`addExtension` "hs") . joinPath . map T.unpack . T.split (== '.')

-- | Get supposed module name by its file and project
moduleNameByFile :: FilePath -> Maybe Project -> String
moduleNameByFile fpath Nothing = takeBaseName fpath
moduleNameByFile fpath (Just proj) = maybe (takeBaseName fpath) (intercalate ".") $ do
	suff <- stripPrefix (splitDirectories (proj ^. projectPath)) (splitDirectories fpath)
	-- try cut any of source-dirs
	flip mplus (return suff) $ msum [
		stripPrefix (splitDirectories dir) suff
		| dir <- ordNub (proj ^.. projectDescription . _Just . infos . infoSourceDirs . each)]

packageOpt :: Maybe ModulePackage -> [String]
packageOpt = maybeToList . fmap (("-package " ++) . view packageName)

-- | Recalc positions to interpret '\t' as one symbol instead of N
class RecalcTabs a where
	-- | Interpret '\t' as one symbol instead of N
	recalcTabs :: String -> Int -> a -> a
	-- | Inverse of `recalcTabs`: interpret '\t' as N symbols instead of 1
	calcTabs :: String -> Int -> a -> a

instance RecalcTabs Position where
	recalcTabs cts n (Position l c) = Position l c' where
		line = listToMaybe $ drop (pred l) $ lines cts
		c' = case line of
			Nothing -> c
			Just line' -> let sizes = map charSize line' in
				succ . fromMaybe (length sizes) .
				findIndex (>= pred c) .
				scanl (+) 0 $ sizes
		charSize :: Char -> Int
		charSize '\t' = n
		charSize _ = 1
	calcTabs cts n (Position l c) = Position l c' where
		line = listToMaybe $ drop (pred l) $ lines cts
		c' = maybe c (succ . sum . map charSize . take (pred c)) line
		charSize :: Char -> Int
		charSize '\t' = n
		charSize _ = 1

instance RecalcTabs Region where
	recalcTabs cts n (Region f t) = Region (recalcTabs cts n f) (recalcTabs cts n t)
	calcTabs cts n (Region f t) = Region (calcTabs cts n f) (calcTabs cts n t)
