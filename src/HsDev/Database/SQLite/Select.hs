{-# LANGUAGE OverloadedStrings, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module HsDev.Database.SQLite.Select (
	Select(..), select_, from_, where_, buildQuery, toQuery,
	qSymbolId, qSymbol, qModuleLocation, qModuleId, qBuildInfo,
	qNSymbol
	) where

import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple
import Text.Format

data Select a = Select {
	selectColumns :: [a],
	selectTables :: [a],
	selectConditions :: [a] }
		deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

instance Monoid (Select a) where
	mempty = Select mempty mempty mempty
	Select lc lt lcond `mappend` Select rc rt rcond = Select
		(lc `mappend` rc)
		(lt `mappend` rt)
		(lcond `mappend` rcond)

select_ :: [a] -> Select a
select_ cols = Select cols [] []

from_ :: [a] -> Select a
from_ tbls = Select [] tbls []

where_ :: [a] -> Select a
where_ = Select [] []

buildQuery :: Select Text -> String
buildQuery (Select cols tables conds) = "select {} from {} where {}"
	~~ T.intercalate ", " cols
	~~ T.intercalate ", " tables
	~~ T.intercalate " and " (map (\cond -> T.concat ["(", cond, ")"]) conds)

toQuery :: Select Text -> Query
toQuery = fromString . buildQuery

qSymbolId :: Select Text
qSymbolId = mconcat [
	select_ [
		"s.name",
		"m.name",
		"m.file",
		"m.cabal",
		"m.install_dirs",
		"m.package_name",
		"m.package_version",
		"m.installed_name",
		"m.other_location"],
	from_ ["modules as m", "symbols as s"],
	where_ ["m.id == s.module_id"]]

qSymbol :: Select Text
qSymbol = mconcat [
	qSymbolId,
	select_ [
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
		"s.pat_constructor"]]

qModuleLocation :: Select Text
qModuleLocation = mconcat [
	select_ [
		"ml.file",
		"ml.cabal",
		"ml.install_dirs",
		"ml.package_name",
		"ml.package_version",
		"ml.installed_name",
		"ml.other_location"],
	from_ ["modules as ml"]]

qModuleId :: Select Text
qModuleId = mconcat [
	select_ [
		"mu.name",
		"mu.file",
		"mu.cabal",
		"mu.install_dirs",
		"mu.package_name",
		"mu.package_version",
		"mu.installed_name",
		"mu.other_location"],
	from_ ["modules as mu"],
	where_ ["mu.name is not null"]]

qBuildInfo :: Select Text
qBuildInfo = mconcat [
	select_ [
		"bi.depends",
		"bi.language",
		"bi.extensions",
		"bi.ghc_options",
		"bi.source_dirs",
		"bi.other_modules"],
	from_ ["build_infos as bi"]]

-- | Symbol from haskell-names
qNSymbol :: Text -> Text -> Select Text
qNSymbol m s = fmap (`formats` ["m" ~% m, "s" ~% s]) $ mconcat [
	select_ [
		"{s}.what",
		"{m}.name",
		"{s}.name",
		"{s}.parent",
		"{s}.constructors",
		"{s}.associate",
		"{s}.pat_type",
		"{s}.pat_constructor"],
	from_ ["symbols as {s}", "modules as {m}"],
	where_ ["{m}.id = {s}.module_id"]]
