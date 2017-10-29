{-# LANGUAGE OverloadedStrings #-}

module HsDev.Database.SQLite.Select (
	Select(..), select_, where_, buildQuery, toQuery
	) where

import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple
import Text.Format

data Select = Select {
	selectColumns :: [Text],
	selectTables :: [Text],
	selectConditions :: [Text] }
		deriving (Eq, Ord, Read, Show)

instance Monoid Select where
	mempty = Select mempty mempty mempty
	Select lc lt lcond `mappend` Select rc rt rcond = Select
		(lc `mappend` rc)
		(lt `mappend` rt)
		(lcond `mappend` rcond)

select_ :: [Text] -> [Text] -> [Text] -> Select
select_ = Select

where_ :: [Text] -> Select
where_ = select_ [] []

buildQuery :: Select -> String
buildQuery (Select cols tables conds) = "select {} from {} where {};"
	~~ T.intercalate ", " cols
	~~ T.intercalate ", " tables
	~~ T.intercalate " and " (map (\cond -> T.concat ["(", cond, ")"]) conds)

toQuery :: Select -> Query
toQuery = fromString . buildQuery
