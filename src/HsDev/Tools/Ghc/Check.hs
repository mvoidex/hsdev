{-# LANGUAGE PatternGuards #-}

module HsDev.Tools.Ghc.Check (
	checkFiles, check, checkFile, checkSource,

	Ghc,
	module HsDev.Tools.Types,
	module HsDev.Symbols.Types,
	Cabal(..), Project(..),

	recalcNotesTabs,

	module Control.Monad.Except
	) where

import Control.Lens (preview, view, each, _Just, (^..))
import Control.Monad.Except
import Control.Concurrent.FiniteChan
import Data.Maybe (fromMaybe)
import HsDev.Tools.Ghc.Worker
import System.FilePath (makeRelative)
import System.Directory (doesDirectoryExist)

import GHC hiding (Warning, Module, moduleName)
import Outputable
import FastString (unpackFS)
import qualified ErrUtils as E

import System.Directory.Paths
import HsDev.Symbols (moduleOpts)
import HsDev.Symbols.Location
import HsDev.Symbols.Types
import HsDev.Tools.Base
import HsDev.Tools.Types
import HsDev.Util (readFileUtf8, ordNub)

-- | Check files and collect warnings and errors
checkFiles :: [String] -> SandboxStack -> [FilePath] -> Maybe Project -> Ghc [Note OutputMessage]
checkFiles opts sboxes files _ = do
	ch <- liftIO newChan
	withFlags $ do
		modifyFlags (\fs -> fs { log_action = logAction ch })
		_ <- addCmdOpts ("-Wall" : (sandboxStackOpt sboxes ++ opts))
		clearTargets
		mapM (`makeTarget` Nothing) files >>= loadTargets
	notes <- liftIO $ stopChan ch
	liftIO $ recalcNotesTabs notes

-- | Check module source
check :: [String] -> SandboxStack -> Module -> Maybe String -> ExceptT String Ghc [Note OutputMessage]
check opts sboxes m msrc = case view moduleLocation m of
	FileModule file proj -> do
		ch <- liftIO newChan
		pkgs <- lift listPackages
		let
			dir = fromMaybe
				(sourceModuleRoot (view moduleName m) file) $
				preview (_Just . projectPath) proj
		dirExist <- liftIO $ doesDirectoryExist dir
		lift $ withFlags $ (if dirExist then withCurrentDirectory dir else id) $ do
			modifyFlags (\fs -> fs { log_action = logAction ch })
			_ <- addCmdOpts $ concat [
				["-Wall"],
				sandboxStackOpt sboxes,
				moduleOpts pkgs m,
				opts]
			clearTargets
			target <- makeTarget (makeRelative dir file) msrc
			loadTargets [target]
		notes <- liftIO $ stopChan ch
		liftIO $ recalcNotesTabs notes
	_ -> throwError "Module is not source"

-- | Check module and collect warnings and errors
checkFile :: [String] -> SandboxStack -> Module -> ExceptT String Ghc [Note OutputMessage]
checkFile opts sboxes m = check opts sboxes m Nothing

-- | Check module and collect warnings and errors
checkSource :: [String] -> SandboxStack -> Module -> String -> ExceptT String Ghc [Note OutputMessage]
checkSource opts sboxes m src = check opts sboxes m (Just src)

-- | Log  ghc warnings and errors as to chan
-- You may have to apply recalcTabs on result notes
logAction :: Chan (Note OutputMessage) -> DynFlags -> E.Severity -> SrcSpan -> PprStyle -> SDoc -> IO ()
logAction ch fs sev src _ msg
	| Just sev' <- checkSev sev = do
		src' <- canonicalize srcMod
		putChan ch $ Note {
			_noteSource = src',
			_noteRegion = spanRegion src,
			_noteLevel = Just sev',
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
			RealSrcSpan s' -> FileModule (unpackFS $ srcSpanFile s') Nothing
			_ -> ModuleSource Nothing

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
