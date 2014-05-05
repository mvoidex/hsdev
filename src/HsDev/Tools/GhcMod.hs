{-# LANGUAGE OverloadedStrings #-}

module HsDev.Tools.GhcMod (
	list,
	browse, browseInspection,
	info,
	TypedRegion(..),
	typeOf
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
import qualified Language.Haskell.GhcMod.Ghc as GhcMod

import HsDev.Cabal
import HsDev.Project
import HsDev.Symbols
import HsDev.Symbols.Location
import HsDev.Tools.Base
import HsDev.Util ((.::))

list :: [String] -> Cabal -> ErrorT String IO [ModuleLocation]
list opts cabal = do
	cradle <- liftIO $ cradle cabal Nothing
	ms <- tryGhc $ (map splitPackage . lines) <$> GhcMod.modules (GhcMod.defaultOptions { GhcMod.ghcOpts = opts })
	return [CabalModule cabal (readMaybe p) m | (m, p) <- ms]
	where
		splitPackage :: String -> (String, String)
		splitPackage = second (drop 1) . break isSpace

browse :: [String] -> Cabal -> String -> Maybe ModulePackage -> ErrorT String IO InspectedModule
browse opts cabal mname mpackage = inspect mloc (return $ browseInspection opts) $ do
	cradle <- liftIO $ cradle cabal Nothing
	ts <- tryGhc $ lines <$> GhcMod.browse (GhcMod.defaultOptions { GhcMod.detailed = True, GhcMod.ghcOpts = packageOpt mpackage ++ opts }) mpkgname
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
	cradle <- liftIO $ cradle cabal mproj
	rs <- tryGhc $ GhcMod.info (GhcMod.defaultOptions { GhcMod.ghcOpts = cabalOpt cabal ++ opts }) file sname
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
typeOf opts cabal file mproj mname line col = do
	fileCts <- liftIO $ readFile file
	cradle <- liftIO $ cradle cabal mproj
	ts <- fmap lines $ tryGhc $ GhcMod.types (GhcMod.defaultOptions { GhcMod.ghcOpts = cabalOpt cabal ++ opts }) file line col
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

tryGhc :: Ghc a -> ErrorT String IO a
tryGhc act = ErrorT $ ghandle rethrow $ liftM Right $ runGhc (Just libdir) $ do
	dflags <- getSessionDynFlags
	defaultCleanupHandler dflags act
	where
		rethrow :: SomeException -> IO (Either String a)
		rethrow = return . Left . show

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
	where
		deps = fromMaybe [] $ do
			desc <- projectDescription proj
			return $ nub $ sort $ concatMap infoDepends $ concat [
					map buildInfo (maybeToList (projectLibrary desc)),
					map buildInfo (projectExecutables desc),
					map buildInfo (projectTests desc)]
