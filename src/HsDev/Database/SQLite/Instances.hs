{-# LANGUAGE OverloadedStrings, FlexibleInstances, TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Database.SQLite.Instances (
	JSON(..)
	) where

import Control.Lens ((^.), (^?), _Just)
import Data.Aeson as A hiding (Error)
import Data.Maybe
import Data.Foldable
import Data.Time.Clock.POSIX
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as L
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Language.Haskell.Extension
import qualified Language.Haskell.Names as N
import qualified Language.Haskell.Exts as H
import Text.Format

import System.Directory.Paths
import HsDev.Display
import HsDev.Symbols.Name
import HsDev.Symbols.Location
import HsDev.Symbols.Types
import HsDev.Tools.Ghc.Types
import HsDev.Tools.Types
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
		iexposed <- field
		other <- field

		maybe (fail $ "Can't parse module location: {}" ~~ show (file, cabal, dirs, pname, pver, iname, iexposed, other)) return $ msum [
			FileModule <$> file <*> pure (project <$> cabal),
			InstalledModule <$> maybe (pure []) fromJSON' dirs <*> (ModulePackage <$> pname <*> pver) <*> iname <*> iexposed,
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
		toField $ mloc ^? installedModuleExposed,
		toField $ mloc ^? otherLocationName]

instance FromRow ModuleId where
	fromRow = ModuleId <$> field <*> fromRow

instance ToRow ModuleId where
	toRow mid = toField (mid ^. moduleName) : toRow (mid ^. moduleLocation)

instance FromRow Import where
	fromRow = Import <$> fromRow <*> field <*> field <*> field

instance ToRow Import where
	toRow (Import p n q a) = toRow p ++ [toField n, toField q, toField a]

instance FromRow SymbolId where
	fromRow = SymbolId <$> field <*> fromRow

instance ToRow SymbolId where
	toRow (SymbolId nm mid) = toField nm : toRow mid

instance FromRow SymbolInfo where
	fromRow = do
		what <- field @String
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

instance ToRow SymbolInfo where
	toRow si = [
		toField $ symbolInfoType si,
		toField $ si ^? functionType . _Just,
		toField $ msum [si ^? parentClass, si ^? parentType],
		toField $ toJSON $ si ^? selectorConstructors,
		toField $ toJSON $ si ^? typeArgs,
		toField $ toJSON $ si ^? typeContext,
		toField $ si ^? familyAssociate . _Just,
		toField $ si ^? patternType . _Just,
		toField $ si ^? patternConstructor]

instance FromRow Symbol where
	fromRow = Symbol <$> fromRow <*> field <*> pos <*> fromRow where
		pos = do
			line <- field
			column <- field
			return $ Position <$> line <*> column

instance ToRow Symbol where
	toRow sym = concat [
		toRow (sym ^. symbolId),
		[toField $ sym ^. symbolDocs],
		maybe [SQLNull, SQLNull] toRow (sym ^. symbolPosition),
		toRow (sym ^. symbolInfo)]

instance FromRow a => FromRow (Scoped a) where
	fromRow = flip Scoped <$> fromRow <*> field

instance ToRow a => ToRow (Scoped a) where
	toRow (Scoped q s) = toRow s ++ [toField q]

instance ToField BuildTool where
	toField CabalTool = toField @String "cabal"
	toField StackTool = toField @String "stack"

instance FromField BuildTool where
	fromField = fromField @String >=> fromStr where
		fromStr "cabal" = return CabalTool
		fromStr "stack" = return StackTool
		fromStr s = fail $ "Error parsing build tool: {}" ~~ s

instance ToRow Sandbox where
	toRow (Sandbox t p) = [toField t, toField p]

instance FromRow Sandbox where
	fromRow = Sandbox <$> field <*> field

instance ToRow Project where
	toRow (Project name _ cabal pdesc t dbs) = [toField name, toField cabal, toField $ pdesc ^? _Just . projectVersion, toField t, toField dbs]

instance FromRow Project where
	fromRow = do
		name <- field
		cabal <- field
		ver <- field
		tool <- field
		dbs <- field
		return $ Project name (takeDir cabal) cabal (fmap (\v -> ProjectDescription v Nothing [] []) ver) tool dbs

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

instance ToField PackageDbStack where
	toField = toField . toJSON . packageDbs

instance FromField PackageDbStack where
	fromField = fromField >=> maybe (fail "Error parsing package-db-stack") (return . mkPackageDbStack) . fromJSON'

instance FromRow SymbolUsage where
	fromRow = SymbolUsage <$> fromRow <*> field <*> fromRow <*> fromRow

instance FromField POSIXTime where
	fromField = fmap (fromRational . toRational @Double) . fromField

instance ToField POSIXTime where
	toField = toField . fromRational @Double . toRational

instance FromRow Inspection where
	fromRow = do
		tm <- field
		opts <- field
		case (tm, opts) of
			(Nothing, Nothing) -> return InspectionNone
			(_, Just opts') -> InspectionAt (fromMaybe 0 tm) <$>
				maybe (fail "Error parsing inspection opts") return (fromJSON' opts')
			(Just _, Nothing) -> fail "Error parsing inspection data, time is set, but flags are null"

instance ToRow Inspection where
	toRow InspectionNone = [SQLNull, SQLNull]
	toRow (InspectionAt tm opts) = [
		if tm == 0 then SQLNull else toField tm,
		toField $ toJSON opts]

instance FromRow TypedExpr where
	fromRow = TypedExpr <$> field <*> field

instance ToRow TypedExpr where
	toRow (TypedExpr e t) = [toField e, toField t]

instance FromField (H.Name ()) where
	fromField = fmap toName_ . fromField

instance ToField (H.Name ()) where
	toField = toField . fromName_

instance FromField (H.ModuleName ()) where
	fromField = fmap toModuleName_ . fromField

instance ToField (H.ModuleName ()) where
	toField = toField . fromModuleName_

instance FromRow N.Symbol where
	fromRow = do
		what <- field @T.Text
		mname <- field
		name <- field
		parent <- field
		ctors <- do
			ctorsJson <- field
			return $ fmap (map toName_) (ctorsJson >>= fromJSON')
		assoc <- field
		patType <- field
		patCtor <- field
		let
			m = toModuleName_ mname
			n = toName_ name
		maybe (fail $ "Can't parse symbol: {}" ~~ show (what, mname, name, parent, ctors, assoc, patType, patCtor)) return $ case what of
			"function" -> return $ N.Value m n
			"method" -> N.Method m n <$> parent
			"selector" -> N.Selector m n <$> parent <*> ctors
			"ctor" -> N.Constructor m n <$> parent
			"type" -> return $ N.Type m n
			"newtype" -> return $ N.NewType m n
			"data" -> return $ N.Data m n
			"class" -> return $ N.Class m n
			"type-family" -> return $ N.TypeFam m n assoc
			"data-family" -> return $ N.DataFam m n assoc
			"pat-ctor" -> return $ N.PatternConstructor m n patType
			"pat-selector" -> N.PatternSelector m n patType <$> patCtor
			_ -> Nothing

instance ToRow N.Symbol where
	toRow = padNulls 8 . toRow' where
		toRow' (N.Value m n) = mk "function" [toField m, toField n]
		toRow' (N.Method m n p) = mk "method" [toField m, toField n, toField p]
		toRow' (N.Selector m n p cs) = mk "selector" [toField m, toField n, toField p, toField $ toJSON (map fromName_ cs)]
		toRow' (N.Constructor m n p) = mk "ctor" [toField m, toField n, toField p]
		toRow' (N.Type m n) = mk "type" [toField m, toField n]
		toRow' (N.NewType m n) = mk "newtype" [toField m, toField n]
		toRow' (N.Data m n) = mk "data" [toField m, toField n]
		toRow' (N.Class m n) = mk "class" [toField m, toField n]
		toRow' (N.TypeFam m n assoc) = mk "type-family" [toField m, toField n, SQLNull, SQLNull, toField assoc]
		toRow' (N.DataFam m n assoc) = mk "data-family" [toField m, toField n, SQLNull, SQLNull, toField assoc]
		toRow' (N.PatternConstructor m n pty) = mk "pat-ctor" [toField m, toField n, SQLNull, SQLNull, SQLNull, toField pty]
		toRow' (N.PatternSelector m n pty pctor) = mk "pat-selector" [toField m, toField n, SQLNull, SQLNull, SQLNull, toField pty, toField pctor]

		mk :: T.Text -> [SQLData] -> [SQLData]
		mk what = (toField what :)
		padNulls n fs = fs ++ replicate (n - length fs) SQLNull

instance FromField Severity where
	fromField fld = do
		s <- fromField @String fld
		msum [
			guard (s == "error") >> return Error,
			guard (s == "warning") >> return Warning,
			guard (s == "hint") >> return Hint,
			fail ("Unknown severity: {}" ~~ s)]

instance ToField Severity where
	toField Error = toField @String "error"
	toField Warning = toField @String "warning"
	toField Hint = toField @String "hint"

instance FromRow OutputMessage where
	fromRow = OutputMessage <$> field <*> field

instance ToRow OutputMessage where
	toRow (OutputMessage msg suggest) = [toField msg, toField suggest]

instance FromRow a => FromRow (Note a) where
	fromRow = Note <$> (FileModule <$> field <*> pure Nothing) <*> fromRow <*> field <*> fromRow

instance ToRow a => ToRow (Note a) where
	toRow (Note mloc rgn sev n) = concat [
		[toField $ mloc ^? moduleFile],
		toRow rgn,
		[toField sev],
		toRow n]
