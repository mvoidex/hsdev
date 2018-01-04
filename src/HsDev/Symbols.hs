{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsDev.Symbols (
	-- * Utility
	locateProject, searchProject,
	locateSourceDir,
	standaloneInfo,
	moduleOpts,

	-- * Tags
	setTag, hasTag, removeTag, dropTags,

	-- * Reexportss
	module HsDev.Symbols.Types,
	module HsDev.Symbols.Class,
	module HsDev.Symbols.Documented,
	module HsDev.Symbols.HaskellNames
	) where

import Control.Applicative
import Control.Lens
import Control.Monad.Trans.Maybe
import Control.Monad.Except
import Data.List
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Directory
import System.FilePath

import HsDev.Symbols.Types
import HsDev.Symbols.Class
import HsDev.Symbols.Documented (Documented(..))
import HsDev.Symbols.HaskellNames
import HsDev.Util (searchPath, uniqueBy, directoryContents)
import System.Directory.Paths

-- | Find project file is related to
locateProject :: FilePath -> IO (Maybe Project)
locateProject file = do
	file' <- canonicalizePath file
	isDir <- doesDirectoryExist file'
	if isDir then locateHere file' else locateParent (takeDirectory file')
	where
		locateHere p = do
			cts <- filter (not . null . takeBaseName) <$> directoryContents p
			return $ fmap (project . (p </>)) $ find ((== ".cabal") . takeExtension) cts
		locateParent dir = do
			cts <- filter (not . null . takeBaseName) <$> directoryContents dir
			case find ((== ".cabal") . takeExtension) cts of
				Nothing -> if isDrive dir then return Nothing else locateParent (takeDirectory dir)
				Just cabalf -> return $ Just $ project (dir </> cabalf)

-- | Search project up
searchProject :: FilePath -> IO (Maybe Project)
searchProject file = runMaybeT $ searchPath file (MaybeT . locateProject) <|> mzero

-- | Locate source dir of file
locateSourceDir :: FilePath -> IO (Maybe (Extensions Path))
locateSourceDir f = runMaybeT $ do
	file <- liftIO $ canonicalizePath f
	p <- MaybeT $ locateProject file
	proj <- lift $ loadProject p
	MaybeT $ return $ findSourceDir proj (fromFilePath file)

-- | Make `Info` for standalone `Module`
standaloneInfo :: [PackageConfig] -> Module -> Info
standaloneInfo pkgs m = mempty { _infoDepends = pkgDeps ^.. each . package . packageName } where
	pkgDeps = catMaybes [M.lookup mdep pkgMap >>= listToMaybe | mdep <- "Prelude" : imps]
	pkgMap = M.unionsWith mergePkgs [M.singleton m' [p] | p <- pkgs, m' <- view packageModules p]
	mergePkgs ls rs = if null es then hs else es where
		(es, hs) = partition (view packageExposed) $ uniqueBy (view package) (ls ++ rs)
	imps = delete (view (moduleId . moduleName) m) (m ^. moduleImports)

-- | Options for GHC of module and project
moduleOpts :: [PackageConfig] -> Module -> [String]
moduleOpts pkgs m = case view (moduleId . moduleLocation) m of
	FileModule file proj -> concat [
		hidePackages,
		targetOpts absInfo]
		where
			infos' = maybe [standaloneInfo pkgs m] (`fileTargets` file) proj
			info' = over infoDepends (filter validDep) (mconcat $ selfInfo : infos')
			absInfo = maybe id (absolutise . view projectPath) proj info'
			selfInfo
				| proj ^? _Just . projectName `elem` map Just (infos' ^.. each . infoDepends . each) = fromMaybe mempty $
					proj ^? _Just . projectDescription . _Just . projectLibrary . _Just . libraryBuildInfo
				| otherwise = mempty
			-- filter out unavailable packages such as unix under windows
			validDep d = d `elem` pkgs'
			pkgs' = pkgs ^.. each . package . packageName
			hidePackages
				| null (info' ^. infoDepends) = []
				| otherwise = ["-hide-all-packages"]
	_ -> []

-- | Set tag to `Inspected`
setTag :: Ord t => t -> Inspected i t a -> Inspected i t a
setTag tag' = over inspectionTags (S.insert tag')

-- | Check whether `Inspected` has tag
hasTag :: Ord t => t -> Inspected i t a -> Bool
hasTag tag' = has (inspectionTags . ix tag')

-- | Drop tag from `Inspected`
removeTag :: Ord t => t -> Inspected i t a -> Inspected i t a
removeTag tag' = over inspectionTags (S.delete tag')

-- | Drop all tags
dropTags :: Inspected i t a -> Inspected i t a
dropTags = set inspectionTags S.empty
