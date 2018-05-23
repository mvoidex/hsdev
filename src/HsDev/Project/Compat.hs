{-# LANGUAGE CPP #-}

module HsDev.Project.Compat (
	showVer, componentName, testSuiteEnabled,
	flattenCondTree,
	parsePackageDesc
	) where

import Data.Maybe (maybeToList)
import Data.Text (Text, pack)
import qualified Distribution.PackageDescription as PD
import Distribution.Version (Version)
import Distribution.Text (display)

#if MIN_VERSION_Cabal(2,2,0)
import Distribution.PackageDescription.Parsec
import Distribution.Parsec.Common (showPError)
import qualified Data.ByteString.Char8 as C8 (pack)
#else
import Distribution.PackageDescription.Parse
#endif

#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Types.CondTree
#else 
import Distribution.PackageDescription (CondTree(..))
#endif

#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Types.UnqualComponentName
#else
import Data.Version (showVersion)
#endif

showVer :: Version -> String
#if MIN_VERSION_Cabal(2,0,0)
showVer = display
#else
showVer = showVersion
#endif

#if MIN_VERSION_Cabal(2,0,0)
componentName :: UnqualComponentName -> Text
componentName = pack . unUnqualComponentName
#else
componentName :: String -> Text
componentName = pack
#endif

testSuiteEnabled :: PD.TestSuite -> Bool
#if MIN_VERSION_Cabal(2,0,0)
testSuiteEnabled _ = True
#else
testSuiteEnabled = PD.testEnabled
#endif

flattenCondTree :: Monoid a => (c -> a -> a) -> CondTree v c a -> a
flattenCondTree f (PD.CondNode x cs cmps) = f cs x `mappend` mconcat (concatMap flattenBranch cmps) where
#if MIN_VERSION_Cabal(2,0,0)
	flattenBranch (CondBranch _ t mb) = go t mb
#else
	flattenBranch (_, t, mb) = go t mb
#endif
	go t mb = flattenCondTree f t : map (flattenCondTree f) (maybeToList mb)

parsePackageDesc :: String -> Either String PD.GenericPackageDescription
#if MIN_VERSION_Cabal(2,2,0)
parsePackageDesc s = case snd . runParseResult . parseGenericPackageDescription . C8.pack $ s of
	Left (_, errs) -> Left $ unlines $ map (showPError "cabal") errs
	Right r -> Right r
#elif MIN_VERSION_Cabal(2,0,0)
parsePackageDesc s = case parseGenericPackageDescription s of
	ParseOk _ r -> Right r
	ParseFailed e -> Left $ show e
#else
parsePackageDesc s = case parsePackageDescription s of
	ParseOk _ r -> Right r
	ParseFailed e -> Left $ show e
#endif
