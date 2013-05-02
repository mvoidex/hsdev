module HsDev.Tools.GhcMod (
	ghcmod
	) where

import Control.Arrow
import Control.Monad.Error
import qualified Data.Map as M
import System.Process

import HsDev.Symbols
import HsDev.Tools.Base

ghcmod :: [String] -> ErrorT String IO String
ghcmod args = do
	r <- liftIO $ runWait "ghc-mod" args ""
	either throwError return r

list :: ErrorT String IO [String]
list = fmap lines $ ghcmod ["list"]

browse :: String -> ErrorT String IO (Symbol Module)
browse moduleName = fmap toModule $ ghcmod ["browse", "-d", moduleName] where
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
			decls = M.fromList $ map (symbolName &&& id) $ fmap toDecl $ lines str
			toDecl s = undefined
			functionRegex = "(\\w+)\\s+::\\s+(.*)"
			typeRegex = "(class|type|data|newtype)\\s+(\\w+)(\\s+(\\w+(\\s+\\w+)*))?"
