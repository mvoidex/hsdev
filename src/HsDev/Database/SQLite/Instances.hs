{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Database.SQLite.Instances (
	JSON(..)
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
import HsDev.Tools.Ghc.Types
import HsDev.Util

instance ToField Value where
	toField = SQLBlob . L.toStrict . encode

instance FromField Value where
	fromField fld = case fieldData fld of
		SQLText s -> either fail return . eitherDecode . L.fromStrict . T.encodeUtf8 $ s
		SQLBlob s -> either fail return . eitherDecode . L.fromStrict $ s
		_ -> fail "invalid json field type"

newtype JSON a = JSON { getJSON :: a }
	deriving (Eq, Ord, Read, Show)

instance ToJSON a => ToField (JSON a) where
	toField = SQLBlob . L.toStrict . encode . getJSON

instance FromJSON a => FromField (JSON a) where
	fromField fld = case fieldData fld of
		SQLText s -> either fail (return . JSON) . eitherDecode . L.fromStrict . T.encodeUtf8 $ s
		SQLBlob s -> either fail (return . JSON) . eitherDecode . L.fromStrict $ s
		_ -> fail "invalid json field type"

instance FromRow Position where
	fromRow = Position <$> field <*> field

instance ToRow Position where
	toRow (Position l c) = [toField l, toField c]

instance FromRow Region where
	fromRow = Region <$> fromRow <*> fromRow

instance ToRow Region where
	toRow (Region f t) = toRow f ++ toRow t

instance FromRow ModulePackage where
	fromRow = ModulePackage <$> field <*> (fromMaybe T.empty <$> field)

instance ToRow ModulePackage where
	toRow (ModulePackage name ver) = [toField name, toField ver]

instance FromRow ModuleLocation where
	fromRow = do
		file <- field
		cabal <- field
		dirs <- field
		pname <- field
		pver <- field
		iname <- field
		other <- field

		maybe (fail $ "Can't parse module location: {}" ~~ show (file, cabal, dirs, pname, pver, iname, other)) return $ msum [
			FileModule <$> file <*> pure (project <$> cabal),
			InstalledModule <$> maybe (pure []) fromJSON' dirs <*> (ModulePackage <$> pname <*> pver) <*> iname,
			OtherLocation <$> other,
			pure NoLocation]

instance ToRow ModuleLocation where
	toRow mloc = [
		toField $ mloc ^? moduleFile,
		toField $ mloc ^? moduleProject . _Just . projectCabal,
		toField $ fmap toJSON $ mloc ^? moduleInstallDirs,
		toField $ mloc ^? modulePackage . packageName,
		toField $ mloc ^? modulePackage . packageVersion,
		toField $ mloc ^? installedModuleName,
		toField $ mloc ^? otherLocationName]

instance FromRow ModuleId where
	fromRow = ModuleId <$> field <*> fromRow

instance ToRow ModuleId where
	toRow mid = toField (mid ^. moduleName) : toRow (mid ^. moduleLocation)

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

instance FromRow a => FromRow (Scoped a) where
	fromRow = flip Scoped <$> fromRow <*> field

instance ToRow a => ToRow (Scoped a) where
	toRow (Scoped q s) = toRow s ++ [toField q]

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

instance ToField PackageDb where
	toField GlobalDb = toField ("global-db" :: String)
	toField UserDb = toField ("user-db" :: String)
	toField (PackageDb p) = toField ("package-db:" ++ T.unpack p)

instance FromField PackageDb where
	fromField fld = do
		s <- fromField fld
		case s of
			"global-db" -> return GlobalDb
			"user-db" -> return UserDb
			_ -> case T.stripPrefix "package-db:" s of
				Just p' -> return $ PackageDb p'
				Nothing -> fail $ "Can't parse package-db, invalid string: " ++ T.unpack s

instance FromRow SymbolUsage where
	fromRow = SymbolUsage <$> fromRow <*> fromRow <*> fromRow

instance FromRow Inspection where
	fromRow = do
		tm <- field
		opts <- field
		case (tm, opts) of
			(Nothing, Nothing) -> return InspectionNone
			(_, Just opts') -> InspectionAt (maybe 0 (fromRational . (toRational :: Double -> Rational)) tm) <$>
				maybe (fail "Error parsing inspection opts") return (fromJSON' opts')
			(Just _, Nothing) -> fail "Error parsing inspection data, time is set, but flags are null"

instance ToRow Inspection where
	toRow InspectionNone = [SQLNull, SQLNull]
	toRow (InspectionAt tm opts) = [
		if tm == 0 then SQLNull else toField (fromRational (toRational tm) :: Double),
		toField $ toJSON opts]

instance FromRow TypedExpr where
	fromRow = TypedExpr <$> field <*> field

instance ToRow TypedExpr where
	toRow (TypedExpr e t) = [toField e, toField t]
