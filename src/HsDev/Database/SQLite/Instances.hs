{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Database.SQLite.Instances (
	) where

import Control.Lens ((^.), (^?), _Just)
import Data.Aeson as A
import Data.Maybe
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as L
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Language.Haskell.Extension
import Text.Format

import System.Directory.Paths
import HsDev.Symbols.Location
import HsDev.Symbols.Types
import HsDev.Util

instance ToField Value where
	toField = SQLBlob . L.toStrict . encode

instance FromField Value where
	fromField fld = case fieldData fld of
		SQLText s -> either fail return . eitherDecode . L.fromStrict . T.encodeUtf8 $ s
		SQLBlob s -> either fail return . eitherDecode . L.fromStrict $ s
		_ -> fail "invalid json field type"

instance FromRow Position where
	fromRow = Position <$> field <*> field

instance ToRow Position where
	toRow (Position l c) = [toField l, toField c]

instance FromRow ModulePackage where
	fromRow = ModulePackage <$> field <*> (fromMaybe T.empty <$> field)

instance ToRow ModulePackage where
	toRow (ModulePackage name ver) = [toField name, toField ver]

instance FromRow ModuleId where
	fromRow = do
		name <- field
		file <- field
		cabal <- field
		dirs <- field
		pname <- field
		pver <- field
		other <- field

		mloc <- maybe (fail $ "Can't parse module location: {}" ~~ show (name, file, cabal, dirs, pname, pver, other)) return $ msum [
			FileModule <$> file <*> pure (project <$> cabal),
			InstalledModule <$> (maybe (pure []) fromJSON' dirs) <*> pure (ModulePackage <$> pname <*> pver) <*> pure name,
			OtherLocation <$> other]

		return $ ModuleId name mloc

instance ToRow ModuleId where
	toRow mid = [
		toField $ mid ^. moduleName,
		toField $ mid ^? moduleLocation . moduleFile,
		toField $ mid ^? moduleLocation . moduleProject . _Just . projectCabal,
		toField $ fmap toJSON $ mid ^? moduleLocation . moduleInstallDirs,
		toField $ mid ^? moduleLocation . modulePackage . _Just . packageName,
		toField $ mid ^? moduleLocation . modulePackage . _Just . packageVersion,
		toField $ mid ^? moduleLocation . otherLocationName]

instance FromRow SymbolId where
	fromRow = SymbolId <$> field <*> fromRow

instance ToRow SymbolId where
	toRow (SymbolId nm mid) = toField nm : toRow mid

instance FromRow Symbol where
	fromRow = Symbol <$> fromRow <*> field <*> pos <*> infoP where
		pos = do
			line <- field
			column <- field
			return $ Position <$> line <*> column
		infoP = do
			what <- str' <$> field
			ty <- field
			parent <- field
			ctors <- field
			args <- field
			ctx <- field
			assoc <- field
			patTy <- field
			patCtor <- field
			maybe (fail $ "Can't parse symbol info: {}" ~~ show (what, ty, parent, ctors, args, ctx, assoc, patTy, patCtor)) return $ case what of
				"function" -> return $ Function ty
				"method" -> Method <$> pure ty <*> parent
				"selector" -> Selector <$> pure ty <*> parent <*> (fromJSON' =<< ctors)
				"ctor" -> Constructor <$> (fromJSON' =<< args) <*> parent
				"type" -> Type <$> (fromJSON' =<< args) <*> (fromJSON' =<< ctx)
				"newtype" -> NewType <$> (fromJSON' =<< args) <*> (fromJSON' =<< ctx)
				"data" -> Data <$> (fromJSON' =<< args) <*> (fromJSON' =<< ctx)
				"class" -> Class <$> (fromJSON' =<< args) <*> (fromJSON' =<< ctx)
				"type-family" -> TypeFam <$> (fromJSON' =<< args) <*> (fromJSON' =<< ctx) <*> pure assoc
				"data-family" -> DataFam <$> (fromJSON' =<< args) <*> (fromJSON' =<< ctx) <*> pure assoc
				"pat-ctor" -> PatConstructor <$> (fromJSON' =<< args) <*> pure patTy
				"pat-selector" -> PatSelector <$> pure ty <*> pure patTy <*> patCtor
				_ -> Nothing
		str' :: String -> String
		str' = id

instance ToRow Symbol where
	toRow sym = concat [
		toRow (sym ^. symbolId),
		[toField $ sym ^. symbolDocs],
		maybe [SQLNull, SQLNull] toRow (sym ^. symbolPosition),
		info]
		where
			info = [
				toField $ symbolType sym,
				toField $ sym ^? symbolInfo . functionType . _Just,
				toField $ msum [sym ^? symbolInfo . parentClass, sym ^? symbolInfo . parentType],
				toField $ toJSON $ sym ^? symbolInfo . selectorConstructors,
				toField $ toJSON $ sym ^? symbolInfo . typeArgs,
				toField $ toJSON $ sym ^? symbolInfo . typeContext,
				toField $ sym ^? symbolInfo . familyAssociate . _Just,
				toField $ sym ^? symbolInfo . patternType . _Just,
				toField $ sym ^? symbolInfo . patternConstructor]

instance FromRow Project where
	fromRow = do
		name <- field
		cabal <- field
		ver <- field
		return $ Project name (takeDir cabal) cabal $ Just $ ProjectDescription ver Nothing [] []

instance FromRow Library where
	fromRow = do
		mods <- field >>= maybe (fail "Error parsing library modules") return . fromJSON'
		binfo <- fromRow
		return $ Library mods binfo

instance FromRow Executable where
	fromRow = Executable <$> field <*> field <*> fromRow

instance FromRow Test where
	fromRow = Test <$> field <*> field <*> field <*> fromRow

instance FromRow Info where
	fromRow = Info <$>
		(field >>= maybe (fail "Error parsing depends") return . fromJSON') <*>
		field <*>
		(field >>= maybe (fail "Error parsing extensions") return . fromJSON') <*>
		(field >>= maybe (fail "Error parsing ghc-options") return . fromJSON') <*>
		(field >>= maybe (fail "Error parsing source-dirs") return . fromJSON') <*>
		(field >>= maybe (fail "Error parsing other-modules") return . fromJSON')

instance FromField Language where
	fromField fld = case fieldData fld of
		SQLText txt -> parseDT "Language" (T.unpack txt)
		_ -> fail "Can't parse language, invalid type"

instance FromField PackageDb where
	fromField fld = case fieldData fld of
		SQLText "global" -> return GlobalDb
		SQLText "user" -> return UserDb
		SQLText txt -> return $ PackageDb txt
		_ -> fail "Can't parse package-db, invalid type"

instance FromRow SymbolUsage where
	fromRow = SymbolUsage <$> fromRow <*> fromRow <*> fromRow
