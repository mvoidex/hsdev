{-# LANGUAGE PatternGuards #-}

module HsDev.Tools.Ghc.Check (
	checkFiles, check,

	Ghc,
	module HsDev.Tools.Types,
	module HsDev.Symbols.Types,
	Cabal(..), Project(..),

	module Control.Monad.Except
	) where

import Control.Lens (preview, view, each, _Just, (^..))
import Control.Monad.Except
import Control.Concurrent.FiniteChan
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Version (showVersion)
import HsDev.Tools.Ghc.Worker
import System.FilePath (makeRelative)
import Text.Read (readMaybe)

import GHC hiding (Warning, Module, moduleName)
import Outputable
import qualified Packages as GHC
import FastString (unpackFS)
import qualified ErrUtils as E

import HsDev.Symbols (Canonicalize(..), sourceModuleRoot)
import HsDev.Symbols.Location
import HsDev.Symbols.Types
import HsDev.Tools.Base
import HsDev.Tools.Types
import HsDev.Util (readFileUtf8, ordNub)

-- | Check files and collect warnings and errors
checkFiles :: [String] -> Cabal -> [FilePath] -> Maybe Project -> Ghc [Note OutputMessage]
checkFiles opts cabal files _ = do
	ch <- liftIO newChan
	withFlags $ do
		modifyFlags (\fs -> fs { log_action = logAction ch })
		_ <- addCmdOpts ("-Wall" : (cabalOpt cabal ++ opts))
		clearTargets
		mapM (flip makeTarget Nothing) files >>= loadTargets
	notes <- liftIO $ stopChan ch
	liftIO $ recalcNotesTabs notes

-- | Check module and collect warnings and errors
check :: [String] -> Cabal -> Module -> ExceptT String Ghc [Note OutputMessage]
check opts cabal m = case view moduleLocation m of
	FileModule file proj -> do
		ch <- liftIO newChan
		pkgs <- lift $ liftM (map $ view packageName) listPackages
		let
			dir = fromMaybe
				(sourceModuleRoot (view moduleName m) file) $
				preview (_Just . projectPath) proj
			infos' = maybe [] (`fileTargets` file) proj
			srcDirs = concatMap (view infoSourceDirs) infos'
			exts = concatMap (view infoExtensions) infos'
			deps = concatMap (view infoDepends) infos'
			hidePackages
				| null infos' = []
				| otherwise = ["-hide-all-packages"]
		lift $ withFlags $ withCurrentDirectory dir $ do
			modifyFlags (\fs -> fs { log_action = logAction ch })
			_ <- addCmdOpts $ concat [
				["-Wall"],
				cabalOpt cabal,
				["-i" ++ s | s <- srcDirs],
				extensionsOpts exts,
				hidePackages,
				["-package " ++ p | p <- deps, p `elem` pkgs],
				opts]
			clearTargets
			target <- makeTarget (makeRelative dir file) Nothing
			loadTargets [target]
		notes <- liftIO $ stopChan ch
		liftIO $ recalcNotesTabs notes
	_ -> throwError "Module is not source"

-- | Log  ghc warnings and errors as to chan
-- You may have to apply recalcTabs on result notes
logAction :: Chan (Note OutputMessage) -> DynFlags -> E.Severity -> SrcSpan -> PprStyle -> SDoc -> IO ()
logAction ch fs sev src _ msg
	| Just sev' <- checkSev sev = do
		src' <- canonicalize srcMod
		putChan ch $ Note {
			_noteSource = src',
			_noteRegion = case src of
				RealSrcSpan s' ->
					Position (srcSpanStartLine s') (srcSpanStartCol s')
					`region`
					Position (srcSpanEndLine s') (srcSpanEndCol s')
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
			RealSrcSpan s' -> FileModule (unpackFS $ srcSpanFile s') Nothing
			_ -> ModuleSource Nothing

-- | Get list of installed packages
listPackages :: Ghc [ModulePackage]
listPackages = getSessionDynFlags >>= return . mapMaybe readPackage . fromMaybe [] . pkgDatabase

readPackage :: GHC.PackageConfig -> Maybe ModulePackage
readPackage pc = readMaybe $ GHC.packageNameString pc ++ "-" ++ showVersion (GHC.packageVersion pc)

-- Recalc tabs for notes
recalcNotesTabs :: [Note OutputMessage] -> IO [Note OutputMessage]
recalcNotesTabs notes = do
	cts <- mapM readFileUtf8 files
	let
		recalc' n = fromMaybe n $ do
			fname <- preview (noteSource . moduleFile) n
			cts' <- lookup fname (zip files cts)
			return $ recalcTabs cts' n
	return $ map recalc' notes
	where
		files = ordNub $ notes ^.. each . noteSource . moduleFile
