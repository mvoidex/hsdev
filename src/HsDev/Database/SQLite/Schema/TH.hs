{-# LANGUAGE TemplateHaskell #-}

module HsDev.Database.SQLite.Schema.TH (
	schemaExp
	) where

import System.Directory
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

schemaExp :: ExpQ
schemaExp = do
	schemaFile <- runIO $ canonicalizePath "data/hsdev.sql"
	addDependentFile schemaFile
	s <- runIO (readFile "data/hsdev.sql")
	[e| s |]
