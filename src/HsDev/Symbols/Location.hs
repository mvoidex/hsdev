{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module HsDev.Symbols.Location (
	ModulePackage(..), mkPackage, PackageConfig(..),
	ModuleLocation(..), locationId, noLocation,
	ModuleId(..), moduleName, moduleLocation,
	SymbolId(..), symbolName, symbolModule,
	Position(..), Region(..), region, regionAt, regionLines, regionStr,
	Location(..),

	packageName, packageVersion,
	package, packageModules, packageExposed,
	moduleFile, moduleProject, moduleInstallDirs, modulePackage, installedModuleName, installedModuleExposed, otherLocationName,
	positionLine, positionColumn,
	regionFrom, regionTo,
	locationModule, locationPosition,

	sourceModuleRoot,
	importPath,
	sourceRoot, sourceRoot_,
	RecalcTabs(..),

	module HsDev.PackageDb.Types
	) where

import Control.Applicative
import Control.DeepSeq (NFData(..))
import Control.Lens (makeLenses, view, preview, over)
import Data.Aeson
import Data.Char (isSpace, isDigit)
import Data.List (findIndex)
import Data.Maybe
import Data.Text (Text, pack, unpack)
import Data.Text.Lens (unpacked)
import qualified Data.Text as T
import System.FilePath
import Text.Read (readMaybe)
import Text.Format

import System.Directory.Paths
import HsDev.Display
import HsDev.PackageDb.Types
import HsDev.Project.Types
import HsDev.Util ((.::), (.::?), (.::?!), objectUnion, noNulls)

-- | Just package name and version without its location
data ModulePackage = ModulePackage {
	_packageName :: Text,
	_packageVersion :: Text }
		deriving (Eq, Ord)

makeLenses ''ModulePackage

mkPackage :: Text -> ModulePackage
mkPackage n = ModulePackage n ""

instance NFData ModulePackage where
	rnf (ModulePackage n v) = rnf n `seq` rnf v

instance Show ModulePackage where
	show (ModulePackage n "") = unpack n
	show (ModulePackage n v) = unpack n ++ "-" ++ unpack v

instance Read ModulePackage where
	readsPrec _ str = case pkg of
		"" -> []
		_ -> [(ModulePackage (pack n) (pack v), str')]
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
	FileModule { _moduleFile :: Path, _moduleProject :: Maybe Project } |
	InstalledModule { _moduleInstallDirs :: [Path], _modulePackage :: ModulePackage, _installedModuleName :: Text, _installedModuleExposed :: Bool } |
	OtherLocation { _otherLocationName :: Text } |
	NoLocation

instance Eq ModuleLocation where
	FileModule lfile _ == FileModule rfile _ = lfile == rfile
	InstalledModule ldirs _ lname _ == InstalledModule rdirs _ rname _ = ldirs == rdirs && lname == rname
	OtherLocation l == OtherLocation r = l == r
	NoLocation == NoLocation = True
	_ == _ = False

instance Ord ModuleLocation where
	compare l r = compare (locType l, locNames l) (locType r, locNames r) where
		locType :: ModuleLocation -> Int
		locType FileModule{} = 0
		locType InstalledModule{} = 1
		locType OtherLocation{} = 2
		locType NoLocation = 3
		locNames (FileModule f _) = [f]
		locNames (InstalledModule dirs _ nm _) = nm : dirs  -- dirs already includes name of package
		locNames (OtherLocation n) = [n]
		locNames NoLocation = []

makeLenses ''ModuleLocation

locationId :: ModuleLocation -> Text
locationId (FileModule fpath _) = fpath
locationId (InstalledModule dirs mpack nm _) = T.intercalate ":" (take 1 dirs ++ [pack (show mpack), nm])
locationId (OtherLocation src) = src
locationId NoLocation = "<no-location>"

instance NFData ModuleLocation where
	rnf (FileModule f p) = rnf f `seq` rnf p
	rnf (InstalledModule d p n e) = rnf d `seq` rnf p `seq` rnf n `seq` rnf e
	rnf (OtherLocation s) = rnf s
	rnf NoLocation = ()

instance Show ModuleLocation where
	show = unpack . locationId

instance Display ModuleLocation where
	display (FileModule f _) = display f
	display (InstalledModule _ _ n _) = view unpacked n
	display (OtherLocation s) = view unpacked s
	display NoLocation = "<no-location>"
	displayType _ = "module"

instance Formattable ModuleLocation where
	formattable = formattable . display

instance ToJSON ModuleLocation where
	toJSON (FileModule f p) = object $ noNulls ["file" .= f, "project" .= fmap (view projectCabal) p]
	toJSON (InstalledModule c p n e) = object $ noNulls ["dirs" .= c, "package" .= show p, "name" .= n, "exposed" .= e]
	toJSON (OtherLocation s) = object ["source" .= s]
	toJSON NoLocation = object []

instance FromJSON ModuleLocation where
	parseJSON = withObject "module location" $ \v ->
		(FileModule <$> v .:: "file" <*> (fmap project <$> (v .::? "project"))) <|>
		(InstalledModule <$> v .::?! "dirs" <*> (readPackage =<< (v .:: "package")) <*> v .:: "name" <*> v .:: "exposed") <|>
		(OtherLocation <$> v .:: "source") <|>
		(pure NoLocation)
		where
			readPackage s = maybe (fail $ "can't parse package: " ++ s) return . readMaybe $ s

instance Paths ModuleLocation where
	paths f (FileModule fpath p) = FileModule <$> paths f fpath <*> traverse (paths f) p
	paths f (InstalledModule c p n e) = InstalledModule <$> traverse (paths f) c <*> pure p <*> pure n <*> pure e
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
	show (ModuleId n l) = show l ++ ":" ++ unpack n

instance ToJSON ModuleId where
	toJSON m = object $ noNulls [
		"name" .= _moduleName m,
		"location" .= _moduleLocation m]

instance FromJSON ModuleId where
	parseJSON = withObject "module-id" $ \v -> ModuleId <$>
		(fromMaybe "" <$> (v .::? "name")) <*>
		(fromMaybe NoLocation <$> (v .::? "location"))

-- | Symbol
data SymbolId = SymbolId {
	_symbolName :: Text,
	_symbolModule :: ModuleId }
		deriving (Eq, Ord)

makeLenses ''SymbolId

instance NFData SymbolId where
	rnf (SymbolId n m) = rnf n `seq` rnf m

instance Show SymbolId where
	show (SymbolId n m) = show m ++ ":" ++ unpack n

instance ToJSON SymbolId where
	toJSON s = object $ noNulls [
		"name" .= _symbolName s,
		"module" .= _symbolModule s]

instance FromJSON SymbolId where
	parseJSON = withObject "symbol-id" $ \v -> SymbolId <$>
		(fromMaybe "" <$> (v .::? "name")) <*>
		(fromMaybe (ModuleId "" NoLocation) <$> (v .::? "module"))

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
regionStr :: Region -> Text -> Text
regionStr r@(Region f t) s = T.intercalate "\n" $ T.drop (pred $ view positionColumn f) fline : tl where
	s' = take (regionLines r) $ drop (pred (view positionLine f)) $ T.lines s
	(fline:tl) = init s' ++ [T.take (pred $ view positionColumn t) (last s')]

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
		v .::? "pos"

-- | Get source module root directory, i.e. for "...\src\Foo\Bar.hs" with module 'Foo.Bar' will return "...\src"
sourceModuleRoot :: Text -> Path -> Path
sourceModuleRoot mname = over paths $
	normalise . joinPath .
	reverse . drop (length $ T.split (== '.') mname) . reverse .
	splitDirectories

-- | Path to module source
-- >importPath "Quux.Blah" = "Quux/Blah.hs"
importPath :: Text -> Path
importPath = fromFilePath . (`addExtension` "hs") . joinPath . map unpack . T.split (== '.')

-- | Root of sources, package dir or root directory of standalone modules
sourceRoot :: ModuleId -> Maybe Path
sourceRoot m = do
	fpath <- preview (moduleLocation . moduleFile) m
	mproj <- preview (moduleLocation . moduleProject) m
	return $ maybe
		(sourceModuleRoot (view moduleName m) fpath)
		(view projectPath)
		mproj

sourceRoot_ :: ModuleId -> Path
sourceRoot_ = fromMaybe (error "sourceRoot_: not a source location") . sourceRoot

-- | Recalc positions to interpret '\t' as one symbol instead of N
class RecalcTabs a where
	-- | Interpret '\t' as one symbol instead of N
	recalcTabs :: Text -> Int -> a -> a
	-- | Inverse of `recalcTabs`: interpret '\t' as N symbols instead of 1
	calcTabs :: Text -> Int -> a -> a

instance RecalcTabs Position where
	recalcTabs cts n (Position l c) = Position l c' where
		line = listToMaybe $ drop (pred l) $ T.lines cts
		c' = case line of
			Nothing -> c
			Just line' -> let sizes = map charSize (unpack line') in
				succ . fromMaybe (length sizes) .
				findIndex (>= pred c) .
				scanl (+) 0 $ sizes
		charSize :: Char -> Int
		charSize '\t' = n
		charSize _ = 1
	calcTabs cts n (Position l c) = Position l c' where
		line = listToMaybe $ drop (pred l) $ T.lines cts
		c' = maybe c (succ . sum . map charSize . take (pred c) . unpack) line
		charSize :: Char -> Int
		charSize '\t' = n
		charSize _ = 1

instance RecalcTabs Region where
	recalcTabs cts n (Region f t) = Region (recalcTabs cts n f) (recalcTabs cts n t)
	calcTabs cts n (Region f t) = Region (calcTabs cts n f) (calcTabs cts n t)
