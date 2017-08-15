{-# LANGUAGE CPP #-}

module HsDev.Project.Compat (
	showVer, componentName, testSuiteEnabled,
	flattenCondTree,
	parsePackageDesc
	) where

import Data.Maybe (maybeToList)
import Data.Text (Text, pack)
import qualified Distribution.PackageDescription as PD
import Distribution.PackageDescription.Parse
import Distribution.Version (Version)
import Distribution.Text (display)
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

parsePackageDesc :: String -> ParseResult PD.GenericPackageDescription
#if MIN_VERSION_Cabal(2,0,0)
parsePackageDesc = parseGenericPackageDescription
#else
parsePackageDesc = parsePackageDescription
#endif
