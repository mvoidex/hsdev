module HsDev.Tools.GhcMod (
	list,
	browse, browseInspection,
	info
	) where

import Control.Arrow
import Control.Exception
import Control.Monad.Error
import Data.Char
import Data.Maybe
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

list :: [String] -> Cabal -> ErrorT String IO [ModuleLocation]
list opts cabal = do
	cradle <- liftIO $ cradle cabal Nothing
	ms <- tryGhc $ GhcMod.listMods (GhcMod.defaultOptions { GhcMod.ghcOpts = opts }) cradle
	return [CabalModule cabal (readMaybe p) m | (m, p) <- ms]

browse :: [String] -> Cabal -> String -> Maybe String -> ErrorT String IO InspectedModule
browse opts cabal mname mpackage = inspect mloc (return $ browseInspection opts) $ do
	cradle <- liftIO $ cradle cabal Nothing
	ts <- tryGhc $ GhcMod.browse (GhcMod.defaultOptions { GhcMod.detailed = True, GhcMod.ghcOpts = packageOpt mpackage++ opts, GhcMod.packageId = mpackage }) cradle mname
	return $ Module {
		moduleName = mname,
		moduleDocs = Nothing,
		moduleLocation = mloc,
		moduleExports = [],
		moduleImports = M.empty,
		moduleDeclarations = decls ts }
	where
		mloc = CabalModule cabal (mpackage >>= readMaybe) mname
		decls rs = M.fromList $ map (declarationName &&& id) $ mapMaybe parseDecl rs
		parseFunction s = do
			groups <- match "(\\w+)\\s+::\\s+(.*)" s
			return $ Declaration (groups `at` 1) Nothing Nothing (Function $ Just $ groups `at` 2)
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
	rs <- tryGhc $ GhcMod.info (GhcMod.defaultOptions { GhcMod.ghcOpts = cabalOpt cabal ++ opts }) cradle file mname sname
	toDecl rs
	where
		toDecl s = maybe (throwError $ "Can't parse info: '" ++ s ++ "'") return $ parseData s `mplus` parseFunction s
		parseFunction s = do
			groups <- match (sname ++ "\\s+::\\s+(.*?)(\\s+--(.*))?$") s
			return $ Declaration sname Nothing Nothing (Function $ Just $ groups `at` 1)
		parseData s = do
			groups <- match "(newtype|type|data)\\s+((.*)=>\\s+)?(\\S+)\\s+((\\w+\\s+)*)=(\\s*(.*)\\s+-- Defined)?" s
			let
				args = maybe [] words $ groups 5
				ctx = fmap trim $ groups 3
				def = groups 8
			return $ Declaration sname Nothing Nothing (declarationTypeCtor (groups `at` 1) $ TypeInfo ctx args def)
		trim = p . p where
			p = reverse . dropWhile isSpace

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
	return $ GhcMod.Cradle dir Nothing Nothing (cabalOpt cabal)
cradle cabal (Just proj) = return $ GhcMod.Cradle
	(projectPath proj)
	(Just $ projectPath proj)
	(Just $ projectCabal proj)
	(cabalOpt cabal)
