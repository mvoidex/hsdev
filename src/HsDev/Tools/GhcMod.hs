module HsDev.Tools.GhcMod (
	ghcmod,
	package_db,
	include_path,
	ghc_opts,
	Config(..),
	config,
	GhcMod,
	withConfig,
	ghcmod_,
	list,
	browse,
	info,
	inferType
	) where

import Control.Arrow
import Control.Monad.Error
import Control.Monad.Reader
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as M
import System.Directory
import System.FilePath
import System.Process
import Text.RegexPR

import HsDev.Symbols
import HsDev.Tools.Base

ghcmod :: [String] -> ErrorT String IO String
ghcmod args = do
	r <- liftIO $ runWait "ghc-mod" args ""
	either throwError return r

package_db :: Cabal -> ErrorT String IO [String]
package_db Cabal = return []
package_db (CabalDev p) = do
	cts <- liftIO $ getDirectoryContents p
	maybe notFound mkArg $ find (isJust . matchRegexPR "packages-(.*)\\.conf") cts
	where
		notFound = throwError $ "Cabal-dev sandbox is not found in " ++ p
		mkArg name = return ["-package-db " ++ p </> name]

include_path :: FilePath -> [String]
include_path path = ["-i " ++ path]

ghc_opts :: [String] -> [String]
ghc_opts = concatMap (\opt -> ["-g", opt])

data Config = Config {
	configCabal :: Cabal,
	configInclude :: Maybe FilePath,
	configOpts :: [String] }

config :: Config
config = Config Cabal Nothing []

type GhcMod a = ReaderT Config (ErrorT String IO) a

withConfig :: Config -> GhcMod a -> ErrorT String IO a
withConfig cfg action = runReaderT action cfg

ghcmod_ :: [String] -> GhcMod String
ghcmod_ args = do
	db' <- asks configCabal >>= lift . package_db
	i' <- asks (maybe [] include_path . configInclude)
	opts' <- asks (ghc_opts . configOpts)
	lift $ ghcmod (args ++ db' ++ i' ++ opts')

list :: GhcMod [String]
list = fmap lines $ ghcmod_ ["list"]

browse :: String -> GhcMod (Symbol Module)
browse moduleName = fmap toModule $ ghcmod_ ["browse", "-d", moduleName] where
	toModule str = setModuleReferences $ Symbol {
		symbolName = moduleName,
		symbolModule = Nothing,
		symbolDocs = Nothing,
		symbolLocation = Nothing,
		symbolTags = [],
		symbol = Module {
			moduleExports = [],
			moduleImports = M.empty,
			moduleDeclarations = decls,
			moduleCabal = Just Cabal } }
		where
			decls = M.fromList $ map (symbolName &&& id) $ mapMaybe parseDecl $ lines str
			parseFunction s = do
				groups <- match "(\\w+)\\s+::\\s+(.*)" s
				return $ mkSymbol (groups `at` 1) (Function $ groups `at` 2)
			parseType s = do
				groups <- match "(class|type|data|newtype)\\s+(\\w+)(\\s+(\\w+(\\s+\\w+)*))?" s
				let
					args = maybe [] words $ groups 3
				return $ mkSymbol (groups `at` 2) (ctor (groups `at` 1) $ TypeInfo Nothing args Nothing)
			parseDecl s = parseFunction s `mplus` parseType s

info :: FilePath -> String -> String -> Cabal -> GhcMod (Symbol Declaration)
info file moduleName symbolName cabal = ghcmod_ ["info", file, moduleName, symbolName] >>= toDecl where
	toDecl s = maybe (throwError $ "Can't parse info: '" ++ s ++ "'") return $ parseData s `mplus` parseFunction s
	parseFunction s = do
		groups <- match (symbolName ++ "\\s+::\\s+(.*?)(\\s+--(.*))?$") s
		return $ mkSymbol symbolName (Function $ groups `at` 1)
	parseData s = do
		groups <- match "(newtype|type|data)\\s+((.*)=>\\s+)?(\\S+)\\s+((\\w+\\s+)*)=(\\s*(.*)\\s+-- Defined)?" s
		let
			args = maybe [] words $ groups 5
			ctx = fmap trim $ groups 3
			def = groups 8
		return $ mkSymbol symbolName $ ctor (groups `at` 1) $ TypeInfo ctx args def
	trim = p . p where
		p = reverse . dropWhile isSpace

inferType :: FilePath -> String -> Int -> Int -> Cabal -> GhcMod String
inferType file moduleName line column cabal = ghcmod_ ["type", file, moduleName, show line, show column]

ctor :: String -> TypeInfo -> Declaration
ctor "type" = Type
ctor "newtype" = NewType
ctor "data" = Data
ctor "class" = Class
ctor n = error $ "HsDev.Tools.GhcMod.ctor: unknown keyword '" ++ n ++ "'"
