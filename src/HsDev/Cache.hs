{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Cache (
	escapePath,
	cabalCache,
	projectCache,
	standaloneCache,
	dump,
	load,
	) where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Char
import Data.List
import qualified Data.Map as M
import System.FilePath

import HsDev.Symbols
import HsDev.Project
import HsDev.Project.JSON ()
import HsDev.Database
import HsDev.Util

instance ToJSON ModuleLocation where
	toJSON (FileModule f p) = object ["file" .= f, "project" .= p]
	toJSON (CabalModule c p n) = object ["cabal" .= c, "package" .= p, "name" .= n]
	toJSON (MemoryModule s) = object ["mem" .= s]

instance FromJSON ModuleLocation where
	parseJSON = withObject "module location" $ \v ->
		(FileModule <$> v .:: "file" <*> v .:: "project") <|>
		(CabalModule <$> v .:: "cabal" <*> v .:: "package" <*> v .:: "name") <|>
		(MemoryModule <$> v .:: "mem")

instance ToJSON Position where
	toJSON (Position l c) = object [
		"line" .= l,
		"column" .= c]

instance FromJSON Position where
	parseJSON = withObject "position" $ \v -> Position <$> v .:: "line" <*> v .:: "column"

instance ToJSON Location where
	toJSON (Location ml p) = object [
		"module" .= ml,
		"pos" .= p]

instance FromJSON Location where
	parseJSON = withObject "location" $ \v -> Location <$> v .:: "module" <*> v .:: "pos"

instance ToJSON Import where
	toJSON i = object [
		"name" .= importModuleName i,
		"qualified" .= importIsQualified i,
		"as" .= importAs i,
		"pos" .= importPosition i]

instance FromJSON Import where
	parseJSON = withObject "import" $ \v -> Import <$>
		v .:: "name" <*>
		v .:: "qualified" <*>
		v .:: "as" <*>
		v .:: "pos"

instance ToJSON Cabal where
	toJSON Cabal = toJSON ("<cabal>" :: String)
	toJSON (Sandbox p) = toJSON p

instance FromJSON Cabal where
	parseJSON v = do
		p <- parseJSON v
		return $ if p == "<cabal>" then Cabal else Sandbox p

instance ToJSON Module where
	toJSON m = object [
		"name" .= moduleName m,
		"docs" .= moduleDocs m,
		"location" .= moduleLocation m,
		"exports" .= moduleExports m,
		"imports" .= moduleImports m,
		"declarations" .= moduleDeclarations m]

instance FromJSON Module where
	parseJSON = withObject "module" $ \v -> Module <$>
		v .:: "name" <*>
		v .:: "docs" <*>
		v .:: "location" <*>
		v .:: "exports" <*>
		v .:: "imports" <*>
		v .:: "declarations"

instance ToJSON Inspection where
	toJSON InspectionNone = object ["inspected" .= False]
	toJSON (InspectionTime tm) = object ["mtime" .= (floor tm :: Integer)]
	toJSON (InspectionFlags fs) = object ["flags" .= fs]

instance FromJSON Inspection where
	parseJSON = withObject "inspection" $ \v ->
		((const InspectionNone :: Bool -> Inspection) <$> v .:: "inspected") <|>
		((InspectionTime . fromInteger) <$> v .:: "mtime") <|>
		(InspectionFlags <$> v .:: "flags")

instance ToJSON InspectedModule where
	toJSON im = object [
		"inspection" .= inspection im,
		"location" .= inspectionModule im,
		either ("error" .=) ("module" .=) (inspectionResult im)]

instance FromJSON InspectedModule where
	parseJSON = withObject "inspected module" $ \v -> InspectedModule <$>
		v .:: "inspection" <*>
		v .:: "location" <*>
		((Left <$> v .:: "error") <|> (Right <$> v .:: "module"))

instance ToJSON TypeInfo where
	toJSON t = object [
		"ctx" .= typeInfoContext t,
		"args" .= typeInfoArgs t,
		"def" .= typeInfoDefinition t]

instance FromJSON TypeInfo where
	parseJSON = withObject "type info" $ \v -> TypeInfo <$> v .:: "ctx" <*> v .:: "args" <*> v .:: "def"

instance ToJSON Declaration where
	toJSON d = object [
		"name" .= declarationName d,
		"docs" .= declarationDocs d,
		"pos" .= declarationPosition d,
		"decl" .= declaration d]

instance FromJSON Declaration where
	parseJSON = withObject "declaration" $ \v -> Declaration <$> v .:: "name" <*> v .:: "docs" <*> v .:: "pos" <*> v .:: "decl"

instance ToJSON DeclarationInfo where
	toJSON i = case declarationInfo i of
		Left t -> object ["what" .= ("function" :: String), "type" .= t]
		Right ti -> object ["what" .= declarationTypeName i, "info" .= ti]

instance FromJSON DeclarationInfo where
	parseJSON = withObject "declaration info" $ \v -> do
		w <- fmap (id :: String -> String) $ v .:: "what"
		if w == "function"
			then Function <$> v .:: "type"
			else declarationTypeCtor w <$> v .:: "info"

instance ToJSON Database where
	toJSON (Database ms ps) = object [
		"modules" .= M.elems ms,
		"projects" .= M.elems ps]

instance FromJSON Database where
	parseJSON = withObject "database" $ \v -> (Database <$>
		((M.unions . map mkModule) <$> v .:: "modules") <*>
		((M.unions . map mkProject) <$> v .:: "projects"))
		where
			mkModule m = M.singleton (inspectionModule m) m
			mkProject p = M.singleton (projectCabal p) p

-- | Escape path
escapePath :: FilePath -> FilePath
escapePath = intercalate "." . map (filter isAlphaNum) . splitDirectories

-- | Name of cache for cabal
cabalCache :: Cabal -> FilePath
cabalCache Cabal = "cabal" <.> "json"
cabalCache (Sandbox p) = escapePath p <.> "json"

-- | Name of cache for projects
projectCache :: Project -> FilePath
projectCache p = (escapePath . projectPath $ p) <.> "json"

-- | Name of cache for standalone files
standaloneCache :: FilePath
standaloneCache = "standalone" <.> "json"

-- | Dump database to file
dump :: FilePath -> Database -> IO ()
dump file = BS.writeFile file . encodePretty

-- | Load database from file
load :: FilePath -> IO (Either String Database)
load file = do
	cts <- BS.readFile file
	return $ eitherDecode cts
