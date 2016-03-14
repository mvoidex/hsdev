{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module HsDev.Cache (
	escapePath,
	versionCache,
	packageDbCache,
	projectCache,
	standaloneCache,
	dump,
	load,
	writeVersion,
	readVersion,

	-- * Reexports
	Database
	) where

import Control.DeepSeq (force)
import Control.Lens (view)
import Data.Aeson (encode, eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Char (isAlphaNum)
import Data.List (intercalate)
import System.FilePath
import Text.Read (readMaybe)

import HsDev.PackageDb
import HsDev.Project
import HsDev.Database (Database)
import HsDev.Version
import HsDev.Util (split)

-- | Escape path
escapePath :: FilePath -> FilePath
escapePath = intercalate "." . map (filter isAlphaNum) . splitDirectories

-- | Name of cache for version
versionCache :: FilePath
versionCache = "version" <.> "json"

-- | Name of cache for cabal
packageDbCache :: PackageDb -> FilePath
packageDbCache GlobalDb = "global" <.> "json"
packageDbCache UserDb = "user" <.> "json"
packageDbCache (PackageDb p) = escapePath p <.> "json"

-- | Name of cache for projects
projectCache :: Project -> FilePath
projectCache p = (escapePath . view projectPath $ p) <.> "json"

-- | Name of cache for standalone files
standaloneCache :: FilePath
standaloneCache = "standalone" <.> "json"

-- | Dump database to file
dump :: FilePath -> Database -> IO ()
dump file = BS.writeFile file . encodePretty

-- | Load database from file, strict
load :: FilePath -> IO (Either String Database)
load file = do
	cts <- BS.readFile file
	return $ force $ eitherDecode cts

-- | Write version
writeVersion :: FilePath -> IO ()
writeVersion file = BS.writeFile file $ encode ver where
	ver :: Maybe [Int]
	ver = mapM readMaybe $ split (== '.') $cabalVersion

-- | Read version
readVersion :: FilePath -> IO (Maybe [Int])
readVersion file = do
	cts <- BS.readFile file
	return $ either (const Nothing) id $ eitherDecode cts
