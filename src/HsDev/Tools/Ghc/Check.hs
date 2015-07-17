{-# LANGUAGE PatternGuards #-}

module HsDev.Tools.Ghc.Check (
	check,

	Ghc,
	module HsDev.Tools.Types
	) where

import Control.Lens (preview)
import Control.Concurrent.FiniteChan
import HsDev.Tools.Ghc.Worker

import GHC hiding (Warning)
import Outputable
import FastString (unpackFS)
import qualified ErrUtils as E

import HsDev.Project (Project(..))
import HsDev.Symbols.Location
import HsDev.Tools.Base
import HsDev.Tools.Types
import HsDev.Util

-- | Check files and collect warnings and errors
check :: [String] -> Cabal -> [FilePath] -> Maybe Project -> Ghc [Note OutputMessage]
check opts cabal files _ = do
	cts <- liftIO $ mapM readFileUtf8 files
	ch <- liftIO newChan
	let
		logAction :: DynFlags -> E.Severity -> SrcSpan -> PprStyle -> SDoc -> IO ()
		logAction fs sev src _ msg
			| Just sev' <- checkSev sev = do
				putChan ch $ maybe id recalcTabs fileCts $ Note {
					_noteSource = srcMod,
					_noteRegion = case src of
						RealSrcSpan src' ->
							Position (srcSpanStartLine src') (srcSpanStartCol src')
							`region`
							Position (srcSpanEndLine src') (srcSpanEndCol src')
						_ -> Position 0 0 `region` Position 0 0,
					_noteLevel = sev',
					_note = OutputMessage {
						_message = showSDoc fs msg,
						_messageSuggestion = Nothing } }
			| otherwise = return ()
			where
				checkSev SevWarning = Just Warning
				checkSev SevError = Just Error
				checkSev SevFatal = Just Error
				checkSev _ = Nothing
				srcMod = case src of
					RealSrcSpan src' -> FileModule (unpackFS $ srcSpanFile src') Nothing
					_ -> ModuleSource Nothing
				fileCts = do
					fileName <- preview moduleFile srcMod
					lookup fileName $ zip files cts
	withFlags $ do
		modifyFlags (\fs -> fs { log_action = logAction })
		_ <- addCmdOpts ("-Wall" : (cabalOpt cabal ++ opts))
		clearTargets
		mapM (flip makeTarget Nothing) files >>= loadTargets
	liftIO $ stopChan ch
