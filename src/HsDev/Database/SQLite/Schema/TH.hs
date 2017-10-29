{-# LANGUAGE TemplateHaskell #-}

module HsDev.Database.SQLite.Schema.TH (
	schemaExp
	) where

import Language.Haskell.TH

schemaExp :: ExpQ
schemaExp = do
	s <- runIO (readFile "data/hsdev.sql")
	[e| s |]
