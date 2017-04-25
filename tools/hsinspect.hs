{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Main (
	main
	) where

import Control.Lens (view)
import Control.Monad (liftM, (>=>))
import Control.Monad.IO.Class
import Data.List (intercalate)
import qualified Data.Text.IO as T
import Data.String (fromString)
import System.Directory (canonicalizePath)
import System.FilePath (takeExtension)

import HsDev.Error
import HsDev.Inspect (inspectContents, inspectDocs, getDefines, inspectFile)
import HsDev.PackageDb
import HsDev.Project (readProject)
import HsDev.Scan (scanModify)
import HsDev.Scan.Browse (listModules, browseModules)
import HsDev.Symbols.Location (installedModuleName)
import HsDev.Tools.Ghc.Types (inferTypes)
import HsDev.Tools.Ghc.Worker
import System.Directory.Paths

import Tool

data Opts = Opts (Maybe String) [String]

opts :: Parser Opts
opts = Opts <$>
	optional (strArgument (metavar "what" <> help "depending of what <what> is, inspect installed module, source file (.hs), cabal file (.cabal) or contents, passes as input if no <what> specified")) <*>
	many (strOption (metavar "GHC_OPT" <> long "ghc" <> short 'g' <> help "options to pass to GHC"))

main :: IO ()
main = toolMain "hsinspect" "haskell inspect" opts (runToolClient . inspect') where
	inspect' :: Opts -> ClientM IO Value
	inspect' (Opts Nothing ghcs) = do
		cts <- liftIO T.getContents
		defs <- liftIO getDefines
		liftM toJSON $ liftIO $ hsdevLift $ inspectContents "stdin" defs ghcs cts
	inspect' (Opts (Just fname@(takeExtension -> ".hs")) ghcs) = do
		fname' <- liftIO $ canonicalizePath fname
		defs <- liftIO getDefines
		im <- liftIO $ inspectFile defs ghcs (fromFilePath fname') Nothing Nothing
		ghc <- ghcWorker
		let
			scanAdditional =
				scanModify' (\opts' -> liftIO . inWorker ghc . inspectDocs opts') >=>
				scanModify' (\opts' m -> liftIO (inWorker ghc (ghcSession opts' >> inferTypes opts' m Nothing)))
		toJSON <$> scanAdditional im
	inspect' (Opts (Just fcabal@(takeExtension -> ".cabal")) _) = do
		fcabal' <- liftIO $ canonicalizePath fcabal
		toJSON <$> liftIO (readProject fcabal')
	inspect' (Opts (Just mname) ghcs) = do
		ghc <- ghcWorker
		mlocs <- liftIO $ inWorker ghc $ listModules ghcs userDb
		let
			mlocs' = filter ((== fromString mname) . view installedModuleName) mlocs
		case mlocs' of
			[mloc] -> do
				[im] <- liftIO $ inWorker ghc $ browseModules ghcs userDb [mloc]
				return $ toJSON im
			[] -> hsdevError $ InspectError $ "module '" ++ mname ++ "' not found"
			_ -> hsdevError $ InspectError $ "ambiguous modules: " ++ intercalate ", " (map show mlocs')
	scanModify' f im = scanModify f im <|> return im
