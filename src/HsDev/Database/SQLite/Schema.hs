{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module HsDev.Database.SQLite.Schema (
	schema, commands
	) where

import qualified Data.Text as T
import Data.String
import Database.SQLite.Simple (Query)

import HsDev.Database.SQLite.Schema.TH

schema :: T.Text
schema = T.pack $schemaExp

commands :: [Query]
commands =
	map (fromString . T.unpack) $
	filter (not . T.null) $
	map T.strip $ T.splitOn ";" schema