module HsDev.Tools.HLint (
	hlint
	) where

import Control.Monad.Except
import Language.Haskell.HLint3 (autoSettings, parseModuleEx, applyHints, Idea(..), parseErrorMessage)
import Language.Haskell.Exts.SrcLoc
import qualified Language.Haskell.HLint3 as HL (Severity(..))

import HsDev.Symbols (Canonicalize(..))
import HsDev.Symbols.Location
import HsDev.Tools.Base
import HsDev.Util (readFileUtf8)

hlint :: FilePath -> ExceptT String IO [Note OutputMessage]
hlint file = do
	file' <- liftIO $ canonicalize file
	cts <- liftIO $ readFileUtf8 file'
	(flags, classify, hint) <- liftIO autoSettings
	p <- liftIO $ parseModuleEx flags file' (Just cts)
	m <- either (throwError . parseErrorMessage) return p
	return $ map (recalcTabs cts . fromIdea) $ applyHints classify hint [m]

fromIdea :: Idea -> Note OutputMessage
fromIdea idea = Note {
	_noteSource = FileModule (srcSpanFilename src) Nothing,
	_noteRegion = Region (Position (srcSpanStartLine src) (srcSpanStartColumn src)) (Position (srcSpanEndLine src) (srcSpanEndColumn src)),
	_noteLevel = case ideaSeverity idea of
		HL.Ignore -> Hint
		HL.Warning -> Warning
		HL.Error -> Error,
	_note = OutputMessage {
		_message = ideaHint idea,
		_messageSuggestion = ideaTo idea } }
	where
		src = ideaSpan idea
