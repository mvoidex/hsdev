module HsDev.Tools.HLint (
	hlint, hlintFile, hlintSource,

	module Control.Monad.Except
	) where

import Control.Arrow
import Control.Lens (over, view, _Just)
import Control.Monad.Except
import Data.Char
import Data.List
import Data.Maybe (mapMaybe)
import Data.Ord
import Language.Haskell.HLint3 (autoSettings, parseModuleEx, applyHints, Idea(..), parseErrorMessage, ParseFlags(..), CppFlags(..))
import Language.Haskell.Exts.SrcLoc
import qualified Language.Haskell.HLint3 as HL (Severity(..))

import System.Directory.Paths (canonicalize)
import HsDev.Symbols.Location
import HsDev.Tools.Base
import HsDev.Util (readFileUtf8, split)

hlint :: FilePath -> Maybe String -> ExceptT String IO [Note OutputMessage]
hlint file msrc = do
	file' <- liftIO $ canonicalize file
	cts <- maybe (liftIO $ readFileUtf8 file') return msrc
	(flags, classify, hint) <- liftIO autoSettings
	p <- liftIO $ parseModuleEx (flags { cppFlags = CppSimple }) file' (Just cts)
	m <- either (throwError . parseErrorMessage) return p
	return $ map (recalcTabs cts 8 . indentIdea cts . fromIdea) $ applyHints classify hint [m]

hlintFile :: FilePath -> ExceptT String IO [Note OutputMessage]
hlintFile f = hlint f Nothing

hlintSource :: FilePath -> String -> ExceptT String IO [Note OutputMessage]
hlintSource f = hlint f . Just

fromIdea :: Idea -> Note OutputMessage
fromIdea idea = Note {
	_noteSource = FileModule (srcSpanFilename src) Nothing,
	_noteRegion = Region (Position (srcSpanStartLine src) (srcSpanStartColumn src)) (Position (srcSpanEndLine src) (srcSpanEndColumn src)),
	_noteLevel = Just $ case ideaSeverity idea of
		HL.Warning -> Warning
		HL.Error -> Error
		_ -> Hint,
	_note = OutputMessage {
		_message = ideaHint idea,
		_messageSuggestion = ideaTo idea } }
	where
		src = ideaSpan idea

indentIdea :: String -> Note OutputMessage -> Note OutputMessage
indentIdea cts idea = case analyzeIndent cts of
	Nothing -> idea
	Just i -> over (note . messageSuggestion . _Just) (indent' i) idea
	where
		indent' i' = intercalate "\n" . indentTail . map (uncurry (++) . first (concat . (`replicate` i') . (`div` 2) . length) . span isSpace) . split (== '\n')
		indentTail [] = []
		indentTail (h : hs) = h : map (firstIndent ++) hs
		firstIndent = takeWhile isSpace firstLine
		firstLine = regionStr (Position firstLineNum 1 `region` Position (succ firstLineNum) 1) cts
		firstLineNum = view (noteRegion . regionFrom . positionLine) idea

-- | Indent in source
data Indent = Spaces Int | Tabs deriving (Eq, Ord)

instance Show Indent where
	show (Spaces n) = replicate n ' '
	show Tabs = "\t"

-- | Analyze source indentation to convert suggestion to same indentation
-- Returns one indent
analyzeIndent :: String -> Maybe String
analyzeIndent =
	fmap show . selectIndent . map fst . dropUnusual .
	sortBy (comparing $ negate . snd) .
	map (head &&& length) .
	group . sort .
	mapMaybe (guessIndent . takeWhile isSpace) . lines
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
guessIndent :: String -> Maybe Indent
guessIndent s
	| all (== ' ') s = Just $ Spaces $ length s
	| all (== '\t') s = Just Tabs
	| otherwise = Nothing
