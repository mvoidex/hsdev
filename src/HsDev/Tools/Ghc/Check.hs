{-# LANGUAGE OverloadedStrings #-}

module HsDev.Tools.Ghc.Check (
	check,

	Ghc,
	module HsDev.Tools.Types,
	module HsDev.Symbols.Types,
	PackageDb(..), PackageDbStack(..), Project(..),

	module Control.Monad.Except
	) where

import Control.Lens (view, (^.))
import Control.Monad.Except
import qualified Data.Map as M
import Data.Text (Text)
import System.Log.Simple (MonadLog(..), scope, sendLog, Level(Trace))

import GHC hiding (Warning, Module)

import HsDev.Error
import HsDev.PackageDb
import HsDev.Symbols.Location
import HsDev.Symbols.Types
import HsDev.Tools.Base
import HsDev.Tools.Ghc.Worker
import HsDev.Tools.Types
import HsDev.Tools.Tabs
import System.Directory.Paths

-- | Check module source
check :: (MonadLog m, GhcMonad m) => Module -> Maybe Text -> m [Note OutputMessage]
check m msrc = scope "check" $ case view (moduleId . moduleLocation) m of
	FileModule file _ -> do
		let
			dir = sourceRoot_ (m ^. moduleId)
			-- FIXME: There can be dependent modules with modified file contents
			-- Their contents should be set here too
			srcs = maybe mempty (M.singleton file) msrc
		ex <- liftIO $ dirExists dir
		sendLog Trace "loading targets"
		notes <- withFlags $ (if ex then withCurrentDirectory (dir ^. path) else id) $ collectMessages_ $ do
			target <- makeTarget (relPathTo dir file) msrc
			loadTargets [target]
		sendLog Trace "targets checked"
		liftIO $ recalcNotesTabs srcs notes
	_ -> scope "check" $ hsdevError $ ModuleNotSource (view (moduleId . moduleLocation) m)
