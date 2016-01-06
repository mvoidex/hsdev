module Main (
	main
	) where

import Data.Aeson (encode)
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as L (putStrLn)

import HsDev.Tools.Hayoo

import Tool

data HayooOpts = HayooOpts {
	optsQuery :: String,
	optsPage :: Int,
	optsPages :: Int }

hayooOpts :: Parser HayooOpts
hayooOpts = HayooOpts <$>
	strArgument (help "hayoo query") <*>
	(option auto (long "page" <> short 'p' <> help "page number (0 by default") <|> pure 0) <*>
	(option auto (long "pages" <> short 'n' <> help "pages count (1 by default") <|> pure 1)

main :: IO ()
main = toolMain "hshayoo" "hayoo search" hayooOpts $ \(HayooOpts q page pages) -> printExceptT $ printResult $
	liftM concat $ forM [page .. page + pred pages] $ \i ->
		liftM (mapMaybe hayooAsDeclaration . resultResult) $ hayoo q (Just i)
