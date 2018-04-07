{-# LANGUAGE CPP #-}

module HsDev.Tools.HLint (
	hlint,
	hlintSupported,

	module Control.Monad.Except
	) where

import Control.Monad.Except
import Data.Text (Text)

import HsDev.Tools.Base

#ifndef NOHLINT
import Control.Arrow
import Control.Lens (over, view, _Just)
import Data.Char
import Data.List
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Ord
import Data.String (fromString)
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.HLint3 (argsSettings, parseModuleEx, applyHints, Idea(..), parseErrorMessage, ParseFlags(..), CppFlags(..))
import qualified Language.Haskell.HLint3 as HL (Severity(..))

import System.Directory.Paths
import HsDev.Symbols.Location
import HsDev.Util (readFileUtf8)
#endif

hlint :: [String] -> FilePath -> Maybe Text -> ExceptT String IO [Note OutputMessage]
#ifndef NOHLINT
hlint opts file msrc = do
	file' <- liftIO $ canonicalize file
	cts <- maybe (liftIO $ readFileUtf8 file') return msrc
	(flags, classify, hint) <- liftIO $ argsSettings opts
	p <- liftIO $ parseModuleEx (flags { cppFlags = CppSimple }) file' (Just $ T.unpack cts)
	m <- either (throwError . parseErrorMessage) return p
	return $ map (recalcTabs cts 8 . indentIdea cts . fromIdea) $
		filter (not . ignoreIdea) $
		applyHints classify hint [m]
#else
hlint _ _ _ = throwError "Compiled with no hlint support"
#endif

#ifndef NOHLINT
ignoreIdea :: Idea -> Bool
ignoreIdea idea = ideaSeverity idea == HL.Ignore

fromIdea :: Idea -> Note OutputMessage
fromIdea idea = Note {
	_noteSource = FileModule (fromFilePath $ srcSpanFilename src) Nothing,
	_noteRegion = Region (Position (srcSpanStartLine src) (srcSpanStartColumn src)) (Position (srcSpanEndLine src) (srcSpanEndColumn src)),
	_noteLevel = Just $ case ideaSeverity idea of
		HL.Warning -> Warning
		HL.Error -> Error
		_ -> Hint,
	_note = OutputMessage {
		_message = fromString $ ideaHint idea,
		_messageSuggestion = fmap fromString $ ideaTo idea } }
	where
		src = ideaSpan idea

indentIdea :: Text -> Note OutputMessage -> Note OutputMessage
indentIdea cts idea = case analyzeIndent cts of
	Nothing -> idea
	Just i -> over (note . messageSuggestion . _Just) (indent' i) idea
	where
		indent' i' = T.intercalate (fromString "\n") . indentTail . map (uncurry T.append . first ((`T.replicate` i') . (`div` 2) . T.length) . T.span isSpace) . T.split (== '\n')
		indentTail [] = []
		indentTail (h : hs) = h : map (firstIndent `T.append`) hs
		firstIndent = T.takeWhile isSpace firstLine
		firstLine = regionStr (Position firstLineNum 1 `region` Position (succ firstLineNum) 1) cts
		firstLineNum = view (noteRegion . regionFrom . positionLine) idea

-- | Indent in source
data Indent = Spaces Int | Tabs deriving (Eq, Ord)

instance Show Indent where
	show (Spaces n) = replicate n ' '
	show Tabs = "\t"

-- | Analyze source indentation to convert suggestion to same indentation
-- Returns one indent
analyzeIndent :: Text -> Maybe Text
analyzeIndent =
	fmap (fromString . show) . selectIndent . map fst . dropUnusual .
	sortBy (comparing $ negate . snd) .
	map (head &&& length) .
	group . sort .
	mapMaybe (guessIndent . T.takeWhile isSpace) . T.lines
	where
		selectIndent :: [Indent] -> Maybe Indent
		selectIndent [] = Nothing
		selectIndent (Tabs : _) = Just Tabs
		selectIndent indents = Just $ Spaces $ foldr1 gcd $ mapMaybe spaces indents where
			spaces :: Indent -> Maybe Int
			spaces Tabs = Nothing
			spaces (Spaces n) = Just n
		dropUnusual :: [(Indent, Int)] -> [(Indent, Int)]
		dropUnusual [] = []
		dropUnusual is@((_, freq):_) = takeWhile ((> freq `div` 5) . snd) is

-- | Guess indent of one line
guessIndent :: Text -> Maybe Indent
guessIndent s
	| T.all (== ' ') s = Just $ Spaces $ T.length s
	| T.all (== '\t') s = Just Tabs
	| otherwise = Nothing
#endif

hlintSupported :: Bool
#ifndef NOHLINT
hlintSupported = True
#else
hlintSupported = False
#endif
