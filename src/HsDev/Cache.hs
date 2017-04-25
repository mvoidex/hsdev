{-# LANGUAGE OverloadedStrings #-}

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
import Control.Exception
import Control.Lens (view, over)
import Data.Aeson (encode, eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Char (isAlphaNum)
import qualified Data.Text as T
import System.FilePath

import HsDev.PackageDb
import HsDev.Project
import HsDev.Database (Database)
import HsDev.Util (version)
import System.Directory.Paths

-- | Escape path
escapePath :: Path -> Path
escapePath = T.intercalate "." . map (T.filter isAlphaNum) . splitPaths

-- | Name of cache for version
versionCache :: Path
versionCache = "version.json"

-- | Name of cache for cabal
packageDbCache :: PackageDb -> Path
packageDbCache GlobalDb = "global.json"
packageDbCache UserDb = "user.json"
packageDbCache (PackageDb p) = over path (<.> "json") $ escapePath p

-- | Name of cache for projects
projectCache :: Project -> Path
projectCache p = over path (<.> "json") (escapePath . view projectPath $ p)

-- | Name of cache for standalone files
standaloneCache :: Path
standaloneCache = "standalone.json"

-- | Dump database to file
dump :: Path -> Database -> IO ()
dump file = BS.writeFile (view path file) . encodePretty

-- | Load database from file, strict
load :: Path -> IO (Either String Database)
load file = handle onIO $ do
	cts <- BS.readFile (view path file)
	return $ force $ eitherDecode cts
	where
		onIO :: IOException -> IO (Either String Database)
		onIO _ = return $ Left $ "IO exception while reading cache from " ++ view path file

-- | Write version
writeVersion :: Path -> IO ()
writeVersion file = BS.writeFile (view path file) $ encode version

-- | Read version
readVersion :: Path -> IO (Maybe [Int])
readVersion file = handle onIO $ do
	cts <- BS.readFile (view path file)
	return $ either (const Nothing) id $ eitherDecode cts
	where
		onIO :: IOException -> IO (Maybe [Int])
		onIO _ = return Nothing
