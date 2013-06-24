module HsDev.Tools.GhcMod (
	list,
	browse,
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
list opts = tryGhc $ GhcMod.listMods (GhcMod.defaultOptions { GhcMod.ghcOpts = opts })

browse :: [String] -> String -> ErrorT String IO (Symbol Module)
browse opts moduleName = do
	rs <- tryGhc $ GhcMod.browse (GhcMod.defaultOptions { GhcMod.detailed = True, GhcMod.ghcOpts = opts }) moduleName
	return $ setModuleReferences Symbol {
		symbolName = moduleName,
		symbolModule = Nothing,
		symbolDocs = Nothing,
		symbolLocation = Nothing,
		symbolTags = [],
		symbol = Module {
			moduleExports = [],
			moduleImports = M.empty,
			moduleDeclarations = decls rs,
			moduleCabal = Just Cabal } }
	where
		decls rs = M.fromList $ map (symbolName &&& id) $ mapMaybe parseDecl rs
		parseFunction s = do
			groups <- match "(\\w+)\\s+::\\s+(.*)" s
			return $ mkSymbol (groups `at` 1) (Function $ Just $ groups `at` 2)
		parseType s = do
			groups <- match "(class|type|data|newtype)\\s+(\\w+)(\\s+(\\w+(\\s+\\w+)*))?" s
			let
				args = maybe [] words $ groups 3
			return $ mkSymbol (groups `at` 2) (ctor (groups `at` 1) $ TypeInfo Nothing args Nothing)
		parseDecl s = parseFunction s `mplus` parseType s

info :: [String] -> FilePath -> String -> String -> Cabal -> ErrorT String IO (Symbol Declaration)
info opts file moduleName symName cabal = do
	cradle <- getCradle cabal
	rs <- tryGhc $ GhcMod.info (GhcMod.defaultOptions { GhcMod.ghcOpts = opts }) cradle file moduleName symName
	toDecl rs
	where
		toDecl s = maybe (throwError $ "Can't parse info: '" ++ s ++ "'") return $ parseData s `mplus` parseFunction s
		parseFunction s = do
			groups <- match (symName ++ "\\s+::\\s+(.*?)(\\s+--(.*))?$") s
			return $ mkSymbol symName (Function $ Just $ groups `at` 1)
		parseData s = do
			groups <- match "(newtype|type|data)\\s+((.*)=>\\s+)?(\\S+)\\s+((\\w+\\s+)*)=(\\s*(.*)\\s+-- Defined)?" s
			let
				args = maybe [] words $ groups 5
				ctx = fmap trim $ groups 3
				def = groups 8
			return $ mkSymbol symName $ ctor (groups `at` 1) $ TypeInfo ctx args def
		trim = p . p where
			p = reverse . dropWhile isSpace

ctor :: String -> TypeInfo -> Declaration
ctor "type" = Type
ctor "newtype" = NewType
ctor "data" = Data
ctor "class" = Class
ctor n = error $ "HsDev.Tools.GhcMod.ctor: unknown keyword '" ++ n ++ "'"

getCradle :: MonadIO m => Cabal -> m GhcMod.Cradle
getCradle cabal = liftIO $ do
	(strVer, _) <- GhcMod.getGHCVersion
	GhcMod.findCradle sandbox strVer
	where
		sandbox = case cabal of
			Cabal -> Nothing
			CabalDev p -> Just p

tryGhc :: Ghc a -> ErrorT String IO a
tryGhc act = ErrorT $ ghandle rethrow $ liftM Right $ runGhc (Just libdir) $ do
	dflags <- getSessionDynFlags
	defaultCleanupHandler dflags act
	where
		rethrow :: SomeException -> IO (Either String a)
		rethrow = return . Left . show
