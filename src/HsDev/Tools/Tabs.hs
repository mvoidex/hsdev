module HsDev.Tools.Tabs (
	recalcNotesTabs
	) where

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import System.Directory.Paths
import HsDev.Symbols.Location
import HsDev.Tools.Types
import HsDev.Util

-- | Some tools counts tab as 8 symbols and return such file positions; convert them (consider tab = one symbol)
recalcNotesTabs :: Map Path Text -> [Note a] -> IO [Note a]
recalcNotesTabs srcs notes = do
	ctsMap <- fmap M.fromList $ mapM loadFileContents files
	let
		recalc' n = fromMaybe n $ do
			fname <- preview (noteSource . moduleFile) n
			cts' <- M.lookup fname ctsMap
			return $ recalcTabs cts' 8 n
	return $ map recalc' notes
	where
		files = ordNub $ notes ^.. each . noteSource . moduleFile
		loadFileContents f = do
			cts <- maybe (readFileUtf8 $ view path f) return $ M.lookup f srcs
			return (f, cts)
