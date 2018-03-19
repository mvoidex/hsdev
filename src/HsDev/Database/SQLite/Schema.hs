{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module HsDev.Database.SQLite.Schema (
	schema, commands
	) where

import qualified Data.Text as T
import Data.List (unfoldr)
import Database.SQLite.Simple (Query(..))

import HsDev.Database.SQLite.Schema.TH

schema :: T.Text
schema = T.pack $schemaExp

commands :: [Query]
commands = map (Query . T.unlines) . unfoldr takeStmt . T.lines $ schema where
	takeStmt :: [T.Text] -> Maybe ([T.Text], [T.Text])
	takeStmt ls = case break endsStmt ls of
		(_, []) -> Nothing
		(hs, t:ts) -> Just (hs ++ [t], ts)
	comment :: T.Text -> Bool
	comment t = "-- " `T.isPrefixOf` T.strip t
	endsStmt :: T.Text -> Bool
	endsStmt t = not (comment t) && ";" `T.isSuffixOf` T.strip t
