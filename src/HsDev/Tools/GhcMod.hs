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

import qualified Language.Haskell.GhcMod as GhcMod

import HsDev.Symbols
import HsDev.Tools.Base

list :: [String] -> ErrorT String IO [String]
list opts = do
	cradle <- getCradle Cabal
	tryGhc $ GhcMod.listMods (GhcMod.defaultOptions { GhcMod.ghcOpts = opts }) cradle

browse :: [String] -> String -> ErrorT String IO InspectedModule
browse opts mname = inspect (CabalModule Cabal Nothing mname) (browseInspection opts mname) $ do
	cradle <- getCradle Cabal
	ts <- tryGhc $ GhcMod.browse (GhcMod.defaultOptions { GhcMod.detailed = True, GhcMod.ghcOpts = opts }) cradle mname
	return $ Module {
		moduleName = mname,
		moduleDocs = Nothing,
		moduleLocation = CabalModule Cabal Nothing mname,
		moduleExports = [],
		moduleImports = M.empty,
		moduleDeclarations = decls ts }
	where
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

browseInspection :: [String] -> String -> ErrorT String IO Inspection
browseInspection opts _ = return $ InspectionAt 0 opts

info :: [String] -> FilePath -> String -> String -> Cabal -> ErrorT String IO Declaration
info opts file mname sname cabal = do
	cradle <- getCradle cabal
	rs <- tryGhc $ GhcMod.info (GhcMod.defaultOptions { GhcMod.ghcOpts = opts }) cradle file mname sname
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

getCradle :: MonadIO m => Cabal -> m GhcMod.Cradle
getCradle _ = liftIO GhcMod.findCradle -- FIXME

tryGhc :: Ghc a -> ErrorT String IO a
tryGhc act = ErrorT $ ghandle rethrow $ liftM Right $ runGhc (Just libdir) $ do
	dflags <- getSessionDynFlags
	defaultCleanupHandler dflags act
	where
		rethrow :: SomeException -> IO (Either String a)
		rethrow = return . Left . show
