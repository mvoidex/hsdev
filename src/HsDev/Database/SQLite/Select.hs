{-# LANGUAGE OverloadedStrings #-}

module HsDev.Database.SQLite.Select (
	Select(..), select_, where_, buildQuery, toQuery,
	qSymbolId, qSymbol, qModuleId
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


qSymbolId :: Select
qSymbolId = select_
	[
		"s.name",
		"m.name",
		"m.file",
		"m.cabal",
		"m.install_dirs",
		"m.package_name",
		"m.package_version",
		"m.other_location"]
	["modules as m", "symbols as s"]
	["m.id == s.module_id"]

qSymbol :: Select
qSymbol = qSymbolId `mappend` select_ cols [] [] where
	cols = [
		"s.docs",
		"s.line",
		"s.column",
		"s.what",
		"s.type",
		"s.parent",
		"s.constructors",
		"s.args",
		"s.context",
		"s.associate",
		"s.pat_type",
		"s.pat_constructor"]

qModuleId :: Select
qModuleId = select_
	[
		"mu.name",
		"mu.file",
		"mu.cabal",
		"mu.install_dirs",
		"mu.package_name",
		"mu.package_version",
		"mu.other_location"]
	["modules as mu"]
	[]
