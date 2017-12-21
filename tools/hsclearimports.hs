module Main (
	main
	) where

import Control.Lens (view)
import Control.Exception (finally)
import Control.Monad.Except
import System.Directory

import HsDev.Tools.ClearImports (clearImports)
import HsDev.Symbols (locateSourceDir)
import HsDev.Project (entity)
import System.Directory.Paths

import Tool

data Opts = Opts {
	optsFile :: String,
	optsGHC :: [String],
	optsHideImportList :: Bool,
	optsMaxImportList :: Maybe Int }

opts :: Parser Opts
opts = Opts <$>
	strArgument (metavar "file" <> help "file to clear imports in") <*>
	many (strOption (long "ghc" <> short 'g' <> metavar "GHC_OPT" <> help "options for GHC")) <*>
	switch (long "hide-import-list" <> help "hide import list") <*>
	optional (option auto (long "max-import-list" <> metavar "N" <> help "hide long import lists"))

main :: IO ()
main = toolMain "hsclearimports" "clears imports in haskell source" opts $ \opts' -> do
	file <- canonicalize (optsFile opts')
	mroot <- liftM (fmap $ view (entity . path)) $ locateSourceDir file
	cur <- getCurrentDirectory
	flip finally (setCurrentDirectory cur) $ do
		maybe (return ()) setCurrentDirectory mroot
		void $ runExceptT $ catchError
			(clearImports (optsGHC opts') file >>= mapM_ (liftIO . putStrLn . format opts'))
			(\e -> liftIO (putStrLn $ "Error: " ++ e))
	where
		format :: Opts -> (String, String) -> String
		format as (imp, lst)
			| optsHideImportList as = imp
			| maybe False (length lst >) (optsMaxImportList as) = imp
			| otherwise = imp ++ " (" ++ lst ++ ")"
