{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Data.Deps (
	Deps(..), depsMap,
	mapDeps,
	dep, deps,
	inverse, flatten
	) where

import Control.Lens
import Control.Monad.State
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

-- | Dependency map
data Deps a = Deps {
	_depsMap :: Map a [a] }

depsMap :: Lens (Deps a) (Deps b) (Map a [a]) (Map b [b])
depsMap = lens _depsMap (const Deps)

instance Ord a => Monoid (Deps a) where
	mempty = Deps mempty
	mappend (Deps l) (Deps r) = Deps $ M.unionWith nubConcat l r

type instance Index (Deps a) = a
type instance IxValue (Deps a) = [a]

instance Ord a => Ixed (Deps a) where
	ix k = depsMap . ix k

instance Ord a => At (Deps a) where
	at k = depsMap . at k

mapDeps :: Ord b => (a -> b) -> Deps a -> Deps b
mapDeps f = Deps . M.mapKeys f . M.map (map f) . _depsMap

-- | Make single dependency
dep :: a -> a -> Deps a
dep x y = deps x [y]

-- | Make dependency for one target, note that order of dependencies is matter
deps :: a -> [a] -> Deps a
deps x ys = Deps $ M.singleton x ys

-- | Inverse dependencies, i.e. make map where keys are dependencies and elements are targets depends on it
inverse :: Ord a => Deps a -> Deps a
inverse = mconcat . map (uncurry dep) . concatMap inverse' . M.toList . _depsMap where
	inverse' :: (a, [a]) -> [(a, a)]
	inverse' (m, ds) = zip ds (repeat m)

-- | Flatten dependencies so that there will be no indirect dependencies
flatten :: Ord a => Deps a -> Deps a
flatten (Deps ds) = flip execState mempty . mapM_ flatten' . M.keys $ ds where
	-- flatten' :: a -> State (Deps a) [a]
	flatten' n = do
		d <- gets (M.lookup n . _depsMap)
		case d of
			Just d' -> return d'
			Nothing -> do
				let
					deps' = fromMaybe [] $ M.lookup n ds
				d'' <- (nub . concat . (++ [deps'])) <$> mapM flatten' deps'
				modify $ mappend (deps n d'')
				return d''

nubConcat :: Ord a => [a] -> [a] -> [a]
nubConcat xs ys = nub $ xs ++ ys
