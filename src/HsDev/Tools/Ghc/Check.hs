{-# LANGUAGE PatternGuards #-}

module HsDev.Tools.Ghc.Check (
	checkFiles, check, checkFile, checkSource,

	Ghc,
	module HsDev.Tools.Types,
	module HsDev.Symbols.Types,
	PackageDb(..), PackageDbStack(..), Project(..),

	recalcNotesTabs,

	module Control.Monad.Except
	) where

import Control.Lens (preview, view, each, _Just, (^..))
import Control.Monad.Except
import Data.Maybe (fromMaybe)
import System.FilePath (makeRelative)
import System.Directory (doesDirectoryExist)

import GHC hiding (Warning, Module, moduleName)

import Control.Concurrent.FiniteChan
import System.Directory.Paths
import HsDev.PackageDb
import HsDev.Scan.Browse (browsePackages)
import HsDev.Symbols (moduleOpts)
import HsDev.Symbols.Location
import HsDev.Symbols.Types
import HsDev.Tools.Base
import HsDev.Tools.Ghc.Worker
import HsDev.Tools.Types
import HsDev.Util (readFileUtf8, ordNub)

-- | Check files and collect warnings and errors
checkFiles :: [String] -> PackageDbStack -> [FilePath] -> Maybe Project -> Ghc [Note OutputMessage]
checkFiles opts pdbs files _ = do
	ch <- liftIO newChan
	withFlags $ do
		modifyFlags (\fs -> fs { log_action = logToChan ch })
		_ <- setCmdOpts ("-Wall" : (packageDbStackOpts pdbs ++ opts))
		clearTargets
		mapM (`makeTarget` Nothing) files >>= loadTargets
	notes <- liftIO $ stopChan ch
	liftIO $ recalcNotesTabs notes

-- | Check module source
check :: [String] -> PackageDbStack -> Module -> Maybe String -> ExceptT String Ghc [Note OutputMessage]
check opts pdbs m msrc = case view moduleLocation m of
	FileModule file proj -> do
		ch <- liftIO newChan
		pkgs <- mapExceptT liftIO $ browsePackages opts pdbs
		let
			dir = fromMaybe
				(sourceModuleRoot (view moduleName m) file) $
				preview (_Just . projectPath) proj
		dirExist <- liftIO $ doesDirectoryExist dir
		lift $ withFlags $ (if dirExist then withCurrentDirectory dir else id) $ do
			_ <- setCmdOpts $ concat [
				["-Wall"],
				packageDbStackOpts pdbs,
				moduleOpts pkgs m,
				opts]
			modifyFlags (\fs -> fs { log_action = logToChan ch })
			clearTargets
			target <- makeTarget (makeRelative dir file) msrc
			loadTargets [target]
		notes <- liftIO $ stopChan ch
		liftIO $ recalcNotesTabs notes
	_ -> throwError "Module is not source"

-- | Check module and collect warnings and errors
checkFile :: [String] -> PackageDbStack -> Module -> ExceptT String Ghc [Note OutputMessage]
checkFile opts pdbs m = check opts pdbs m Nothing

-- | Check module and collect warnings and errors
checkSource :: [String] -> PackageDbStack -> Module -> String -> ExceptT String Ghc [Note OutputMessage]
checkSource opts pdbs m src = check opts pdbs m (Just src)

-- Recalc tabs for notes
recalcNotesTabs :: [Note OutputMessage] -> IO [Note OutputMessage]
recalcNotesTabs notes = do
	cts <- mapM readFileUtf8 files
	let
		recalc' n = fromMaybe n $ do
			fname <- preview (noteSource . moduleFile) n
			cts' <- lookup fname (zip files cts)
			return $ recalcTabs cts' 8 n
	return $ map recalc' notes
	where
		files = ordNub $ notes ^.. each . noteSource . moduleFile
