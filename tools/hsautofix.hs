module Main (
	main
	) where

import Control.Lens (view, preview)
import Control.Arrow ((***))
import Control.Monad (liftM)
import Control.Monad.State (gets)
import Control.Monad.Except (throwError)
import Data.Aeson
import Data.List (partition, sort)
import Data.Maybe (mapMaybe)
import System.Directory (canonicalizePath)
import Text.Read (readMaybe)

import HsDev.Symbols (Canonicalize(..), moduleFile)
import HsDev.Tools.Base
import HsDev.Tools.AutoFix
import HsDev.Tools.GhcMod (parseOutputMessages)
import HsDev.Util (toUtf8, liftE, readFileUtf8, writeFileUtf8, ordNub)

import Tool

data FixCmd = ShowCmd Bool | FixCmd [Int] Bool

fixP :: Parser FixCmd
fixP = subparser $ mconcat [
	cmd "show" "show what can be auto-fixed" (ShowCmd <$> switch (long "json" <> help "output messages in JSON format")),
	cmd "fix" "fix selected errors" (FixCmd <$>
		many (argument auto (metavar "N" <> help "indices of corrections to apply")) <*>
		switch (long "pure" <> help "don't modify files, just return updated corrections"))]

main :: IO ()
main = toolMain "hsautofix" "automatically fix some errors" fixP (printExceptT . printResult . go) where
	go (ShowCmd json) = do
		input <- liftE getContents
		msgs <- if json
			then maybe (throwError "Can't parse messages") return $ decode (toUtf8 input)
			else return $ parseOutputMessages input
		mapM (liftE . canonicalize) $ corrections msgs
	go (FixCmd ns pure') = do
		input <- liftE getContents
		corrs <- maybe (throwError "Can't parse messages") return $ decode (toUtf8 input)
		let
			check i = i `elem` ns || null ns
			(fixCorrs, upCorrs) = (map snd *** map snd) $
				partition (check . fst) $ zip [1..] corrs
		files <- liftE $ mapM canonicalizePath $ ordNub $ sort $ mapMaybe (preview $ noteSource . moduleFile) corrs
		let
			doFix :: FilePath -> String -> ([Note Correction], String)
			doFix file cts = edit cts upCorrs' $ do
				autoFix fixCorrs'
				gets (view regions)
				where
					findCorrs :: FilePath -> [Note Correction] -> [Note Correction]
					findCorrs f = filter ((== Just f) . preview (noteSource . moduleFile))
					fixCorrs' = map (view note) $ findCorrs file fixCorrs
					upCorrs' = findCorrs file upCorrs
			runFix file
				| pure' = return $ fst $ doFix file ""
				| otherwise = do
					(corrs', cts') <- liftM (doFix file) $ liftE $ readFileUtf8 file
					liftE $ writeFileUtf8 file cts'
					return corrs'
		liftM concat $ mapM runFix files
