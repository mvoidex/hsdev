{-# LANGUAGE OverloadedStrings, CPP #-}

module HsDev.Tools.Cabal (
	CabalPackage(..),
	cabalList
	) where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Data.Aeson
import Data.Char (isSpace)
import Data.Maybe
import Distribution.License
import Distribution.Text
import Distribution.Version

import HsDev.Tools.Base
import HsDev.Util

data CabalPackage = CabalPackage {
	cabalPackageName :: String,
	cabalPackageSynopsis :: Maybe String,
	cabalPackageDefaultVersion :: Maybe Version,
	cabalPackageInstalledVersions :: [Version],
	cabalPackageHomepage :: Maybe String,
	cabalPackageLicense :: Maybe License }
		deriving (Eq, Read, Show)

instance ToJSON CabalPackage where
	toJSON cp = object [
		"name" .= cabalPackageName cp,
		"synopsis" .= cabalPackageSynopsis cp,
		"default-version" .= fmap display (cabalPackageDefaultVersion cp),
		"installed-versions" .= map display (cabalPackageInstalledVersions cp),
		"homepage" .= cabalPackageHomepage cp,
		"license" .= fmap display (cabalPackageLicense cp)]

instance FromJSON CabalPackage where
	parseJSON = withObject "cabal-package" $ \v -> CabalPackage <$>
		(v .:: "name") <*>
		(v .:: "synopsis") <*>
		((join . fmap simpleParse) <$> (v .:: "default-version")) <*>
		((mapMaybe simpleParse) <$> (v .:: "installed-versions")) <*>
		(v .:: "homepage") <*>
		((join . fmap simpleParse) <$> (v .:: "license"))

cabalList :: Maybe String -> ToolM [CabalPackage]
cabalList query = do
#if mingw32_HOST_OS
	rs <- liftM (split (all isSpace) . lines) $ runTool_ "powershell" [
		"-Command",
		unwords (["&", "{", "chcp 65001 | out-null;", "cabal list"] ++ maybe [] return query ++ ["}"])]
#else
	rs <- liftM (split (all isSpace) . lines) $ runTool_ "cabal" (["list"] ++ maybe [] return query)
#endif
	return $ map toPackage $ mapMaybe parseFields rs
	where
		toPackage :: (String, [(String, String)]) -> CabalPackage
		toPackage (name, fs) = CabalPackage {
			cabalPackageName = name,
			cabalPackageSynopsis = lookup "Synopsis" fs,
			cabalPackageDefaultVersion = (lookup "Default available version" fs >>= simpleParse),
			cabalPackageInstalledVersions = fromMaybe [] (lookup "Installed versions" fs >>= mapM (simpleParse . trim) . split (== ',')),
			cabalPackageHomepage = lookup "Homepage" fs,
			cabalPackageLicense = lookup "License" fs >>= simpleParse }

		parseFields :: [String] -> Maybe (String, [(String, String)])
		parseFields [] = Nothing
		parseFields (('*':name):fs) = Just (trim name, mapMaybe parseField' fs) where
			parseField' :: String -> Maybe (String, String)
			parseField' str = case parseField str of
				(fname, Just fval) -> Just (fname, fval)
				_ -> Nothing
		parseFields _ = Nothing

		-- foo: bar → (foo, bar)
		parseField :: String -> (String, Maybe String)
		parseField = (trim *** (parseValue . trim . drop 1)) . break (== ':')
		-- [ ... ] → Nothing, ... → Just ...
		parseValue :: String -> Maybe String
		parseValue ('[':_) = Nothing
		parseValue v = Just v
