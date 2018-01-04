{-# LANGUAGE OverloadedStrings #-}

module HsDev.Tools.Ghc.Check (
	check,

	Ghc,
	module HsDev.Tools.Types,
	module HsDev.Symbols.Types,
	PackageDb(..), PackageDbStack(..), Project(..),

	recalcNotesTabs,

	module Control.Monad.Except
	) where

import Control.Lens (preview, view, each, _Just, (^..), (^.))
import Control.Monad.Except
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.Log.Simple (MonadLog(..), scope)

import GHC hiding (Warning, Module, moduleName)

import Control.Concurrent.FiniteChan
import HsDev.Error
import HsDev.PackageDb
import HsDev.Symbols.Location
import HsDev.Symbols.Types
import HsDev.Tools.Base
import HsDev.Tools.Ghc.Worker
import HsDev.Tools.Ghc.Compat as C
import HsDev.Tools.Types
import HsDev.Util (readFileUtf8, ordNub)
import System.Directory.Paths

-- | Check module source
check :: (MonadLog m, GhcMonad m) => [String] -> Module -> Maybe Text -> m [Note OutputMessage]
check opts m msrc = scope "check" $ case view (moduleId . moduleLocation) m of
	FileModule file proj -> do
		ch <- liftIO newChan
		let
			dir = fromMaybe
				(sourceModuleRoot (view (moduleId . moduleName) m) file) $
				preview (_Just . projectPath) proj
		ex <- liftIO $ dirExists dir
		withFlags $ (if ex then withCurrentDirectory (dir ^. path) else id) $ do
			addCmdOpts opts
			modifyFlags $ C.setLogAction $ logToChan ch
			target <- makeTarget (relPathTo dir file) msrc
			loadTargets [target]
		notes <- liftIO $ stopChan ch
		liftIO $ recalcNotesTabs notes
	_ -> scope "check" $ hsdevError $ ModuleNotSource (view (moduleId . moduleLocation) m)

-- Recalc tabs for notes
recalcNotesTabs :: [Note OutputMessage] -> IO [Note OutputMessage]
recalcNotesTabs notes = do
	cts <- mapM (readFileUtf8 . view path) files
	let
		recalc' n = fromMaybe n $ do
			fname <- preview (noteSource . moduleFile) n
			cts' <- lookup fname (zip files cts)
			return $ recalcTabs cts' 8 n
	return $ map recalc' notes
	where
		files = ordNub $ notes ^.. each . noteSource . moduleFile
