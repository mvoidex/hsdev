{-# LANGUAGE OverloadedStrings #-}

module HsDev.Cache (
	escapePath,
	cabalCache,
	projectCache,
	standaloneCache,
	dump,
	load,
	) where

import Data.Aeson (eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Char (isAlphaNum)
import Data.List (intercalate)
import System.FilePath

import HsDev.Symbols (Cabal(..))
import HsDev.Project
import HsDev.Database (Database)

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
