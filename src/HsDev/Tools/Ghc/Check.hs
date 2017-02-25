{-# LANGUAGE OverloadedStrings #-}

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
import System.Log.Simple (MonadLog(..), scope, sendLog, Level(..))

import GHC hiding (Warning, Module, moduleName)

import Control.Concurrent.FiniteChan
import HsDev.Error
import HsDev.PackageDb
import HsDev.Symbols.Location
import HsDev.Symbols.Types
import HsDev.Project (fileTarget)
import HsDev.Tools.Base
import HsDev.Tools.Ghc.Worker
import HsDev.Tools.Ghc.Compat
import HsDev.Tools.Types
import HsDev.Util (readFileUtf8, ordNub)

-- | Check files and collect warnings and errors
checkFiles :: (MonadLog m, GhcMonad m) => [String] -> [FilePath] -> Maybe Project -> m [Note OutputMessage]
checkFiles opts files _ = scope "check-files" $ do
	ch <- liftIO newChan
	withFlags $ do
		modifyFlags $ setLogAction $ logToChan ch
		addCmdOpts opts
		clearTargets
		mapM (`makeTarget` Nothing) files >>= loadTargets
	notes <- liftIO $ stopChan ch
	liftIO $ recalcNotesTabs notes

-- | Check module source
check :: (MonadLog m, GhcMonad m) => [String] -> Module -> Maybe String -> m [Note OutputMessage]
check opts m msrc = scope "check" $ case view (moduleId . moduleLocation) m of
	FileModule file proj -> do
		ch <- liftIO newChan
		let
			dir = fromMaybe
				(sourceModuleRoot (view (moduleId . moduleName) m) file) $
				preview (_Just . projectPath) proj
		dirExist <- liftIO $ doesDirectoryExist dir
		withFlags $ (if dirExist then withCurrentDirectory dir else id) $ do
			addCmdOpts opts
			modifyFlags $ setLogAction $ logToChan ch
			clearTargets
			target <- makeTarget (makeRelative dir file) msrc
			loadTargets [target]
		notes <- liftIO $ stopChan ch
		liftIO $ recalcNotesTabs notes
	_ -> scope "check" $ hsdevError $ ModuleNotSource (view (moduleId . moduleLocation) m)

-- | Check module and collect warnings and errors
checkFile :: (MonadLog m, GhcMonad m) => [String] -> Module -> m [Note OutputMessage]
checkFile opts m = check opts m Nothing

-- | Check module and collect warnings and errors
checkSource :: (MonadLog m, GhcMonad m) => [String] -> Module -> String -> m [Note OutputMessage]
checkSource opts m src = check opts m (Just src)

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
