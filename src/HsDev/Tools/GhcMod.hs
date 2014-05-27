{-# LANGUAGE OverloadedStrings #-}

module HsDev.Tools.GhcMod (
	list,
	browse, browseInspection,
	info,
	TypedRegion(..),
	typeOf,
	ErrorMessage(..),
	check
	) where

import Control.Applicative
import Control.Arrow
import Control.DeepSeq
import Control.Exception
import Control.Monad.Error
import Data.Aeson
import Data.Char
import Data.Maybe
import Data.List (sort, nub)
import qualified Data.Map as M
import Exception (ghandle)
import GHC (Ghc, runGhc, getSessionDynFlags, defaultCleanupHandler)
import GHC.Paths (libdir)
import Text.Read (readMaybe)
import System.Directory (getCurrentDirectory)

import qualified Language.Haskell.GhcMod as GhcMod

import HsDev.Cabal
import HsDev.Project
import HsDev.Symbols
import HsDev.Symbols.Location
import HsDev.Tools.Base
import HsDev.Util ((.::), liftException)

list :: [String] -> Cabal -> ErrorT String IO [ModuleLocation]
list opts cabal = liftException $ do
	cradle <- cradle cabal Nothing
	ms <- (map splitPackage . lines) <$> GhcMod.listModules
		(GhcMod.defaultOptions { GhcMod.ghcOpts = opts })
		cradle
	return [CabalModule cabal (readMaybe p) m | (m, p) <- ms]
	where
		splitPackage :: String -> (String, String)
		splitPackage = second (drop 1) . break isSpace

browse :: [String] -> Cabal -> String -> Maybe ModulePackage -> ErrorT String IO InspectedModule
browse opts cabal mname mpackage = inspect mloc (return $ browseInspection opts) $ liftException $ do
	cradle <- cradle cabal Nothing
	ts <- lines <$> GhcMod.browseModule
		(GhcMod.defaultOptions {
			GhcMod.detailed = True,
			GhcMod.ghcOpts = packageOpt mpackage ++ opts })
		cradle
		mpkgname
	return $ Module {
		moduleName = mname,
		moduleDocs = Nothing,
		moduleLocation = mloc,
		moduleExports = [],
		moduleImports = [],
		moduleDeclarations = decls ts }
	where
		mpkgname = maybe mname (\p -> packageName p ++ ":" ++ mname) mpackage
		mloc = CabalModule cabal mpackage mname
		decls rs = M.fromList $ map (declarationName &&& id) $ mapMaybe parseDecl rs
		parseFunction s = do
			groups <- match "(\\w+)\\s+::\\s+(.*)" s
			return $ Declaration (groups `at` 1) Nothing Nothing (Function (Just $ groups `at` 2) [])
		parseType s = do
			groups <- match "(class|type|data|newtype)\\s+(\\w+)(\\s+(\\w+(\\s+\\w+)*))?" s
			let
				args = maybe [] words $ groups 3
			return $ Declaration (groups `at` 2) Nothing Nothing (declarationTypeCtor (groups `at` 1) $ TypeInfo Nothing args Nothing)
		parseDecl s = parseFunction s `mplus` parseType s

browseInspection :: [String] -> Inspection
browseInspection = InspectionAt 0

info :: [String] -> Cabal -> FilePath -> Maybe Project -> String -> String -> ErrorT String IO Declaration
info opts cabal file mproj mname sname = do
	rs <- liftException $ do
		cradle <- cradle cabal mproj
		GhcMod.infoExpr (GhcMod.defaultOptions { GhcMod.ghcOpts = cabalOpt cabal ++ opts }) cradle file sname
	toDecl rs
	where
		toDecl s = maybe (throwError $ "Can't parse info: '" ++ s ++ "'") return $ parseData s `mplus` parseFunction s
		parseFunction s = do
			groups <- match (sname ++ "\\s+::\\s+(.*?)(\\s+--(.*))?$") s
			return $ Declaration sname Nothing Nothing (Function (Just $ groups `at` 1) [])
		parseData s = do
			groups <- match "(newtype|type|data)\\s+((.*)=>\\s+)?(\\S+)\\s+((\\w+\\s+)*)=(\\s*(.*)\\s+-- Defined)?" s
			let
				args = maybe [] words $ groups 5
				ctx = fmap trim $ groups 3
				def = groups 8
			return $ Declaration sname Nothing Nothing (declarationTypeCtor (groups `at` 1) $ TypeInfo ctx args def)
		trim = p . p where
			p = reverse . dropWhile isSpace

data TypedRegion = TypedRegion {
	typedRegion :: Region,
	typedExpr :: String,
	typedType :: String }
		deriving (Eq, Ord, Read, Show)

instance NFData TypedRegion where
	rnf (TypedRegion r e t) = rnf r `seq` rnf e `seq` rnf t

instance ToJSON TypedRegion where
	toJSON (TypedRegion r e t) = object [
		"region" .= r,
		"expr" .= e,
		"type" .= t]

instance FromJSON TypedRegion where
	parseJSON = withObject "typed region" $ \v -> TypedRegion <$>
		v .:: "region" <*>
		v .:: "expr" <*>
		v .:: "type"

typeOf :: [String] -> Cabal -> FilePath -> Maybe Project -> String -> Int -> Int -> ErrorT String IO [TypedRegion]
typeOf opts cabal file mproj mname line col = liftException $ do
	fileCts <- readFile file
	cradle <- cradle cabal mproj
	ts <- lines <$> GhcMod.typeExpr
		(GhcMod.defaultOptions { GhcMod.ghcOpts = cabalOpt cabal ++ opts })
		cradle
		file
		line
		col
	return $ mapMaybe (toRegionType fileCts) ts
	where
		toRegionType :: String -> String -> Maybe TypedRegion
		toRegionType fstr s = do
			(r, tp) <- parseRead s $ (,) <$> parseRegion <*> readParse
			return $ TypedRegion r (regionStr r fstr) tp
		parseRegion :: ReadM Region
		parseRegion = Region <$> parsePosition <*> parsePosition
		parsePosition :: ReadM Position
		parsePosition = Position <$> readParse <*> readParse

data ErrorMessage = ErrorMessage {
	errorLocation :: Location,
	errorWarning :: Bool,
	errorMessage :: String }
		deriving (Eq, Show)

instance NFData ErrorMessage where
	rnf (ErrorMessage l w m) = rnf l `seq` rnf w `seq` rnf m

instance ToJSON ErrorMessage where
	toJSON (ErrorMessage l w m) = object [
		"location" .= l,
		"warning" .= w,
		"message" .= m]

instance FromJSON ErrorMessage where
	parseJSON = withObject "error message" $ \v -> ErrorMessage <$>
		v .:: "location" <*>
		v .:: "warning" <*>
		v .:: "message"

parseErrorMessage :: String -> Maybe ErrorMessage
parseErrorMessage s = do
	groups <- match "^(.+):(\\d+):(\\d+):(\\s*Warning:)?\\s*(.*)$" s
	return $ ErrorMessage {
		errorLocation = Location {
			locationModule = FileModule (groups `at` 1) Nothing,
			locationPosition = Position <$> readMaybe (groups `at` 2) <*> readMaybe (groups `at` 3) },
		errorWarning = isJust (groups 4),
		errorMessage = groups `at` 5 }

check :: [String] -> Cabal -> [FilePath] -> Maybe Project -> ErrorT String IO [ErrorMessage]
check opts cabal files mproj = liftException $ do
	cradle <- cradle cabal mproj
	msgs <- lines <$> GhcMod.checkSyntax
		(GhcMod.defaultOptions { GhcMod.ghcOpts = cabalOpt cabal ++ opts })
		cradle
		files
	return $ mapMaybe parseErrorMessage msgs

cradle :: Cabal -> Maybe Project -> IO GhcMod.Cradle
cradle cabal Nothing = do
	dir <- getCurrentDirectory
	return $ GhcMod.Cradle dir dir Nothing []
cradle cabal (Just proj) = do
	dir <- getCurrentDirectory
	return $ GhcMod.Cradle
		dir
		(projectPath proj)
		(Just $ projectCabal proj)
		[]
