{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Data.Deps (
	Deps(..), depsMap,
	mapDeps,
	dep, deps,
	inverse,
	DepsError(..), flatten,
	selfDepend,
	linearize
	) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Except
import Data.List (nub, intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Semigroup
import Data.Maybe (fromMaybe)

-- | Dependency map
newtype Deps a = Deps {
	_depsMap :: Map a [a] }

depsMap :: Lens (Deps a) (Deps b) (Map a [a]) (Map b [b])
depsMap = lens _depsMap (const Deps)

instance Ord a => Semigroup (Deps a) where
	Deps l <> Deps r = Deps $ M.unionWith nubConcat l r

instance Ord a => Monoid (Deps a) where
	mempty = Deps mempty
	mappend (Deps l) (Deps r) = Deps $ M.unionWith nubConcat l r

instance Show a => Show (Deps a) where
	show (Deps ds) = unlines [show d ++ " -> " ++ intercalate ", " (map show s) | (d, s) <- M.toList ds]

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

newtype DepsError a =
	CyclicDeps [a]
	-- ^ Dependency cycle, list is cycle, where last item depends on first
		deriving (Eq, Ord, Read)

instance Show a => Show (DepsError a) where
	show (CyclicDeps c) = "dependencies forms a cycle: " ++ concat [show d ++ " -> " | d <- c] ++ "..."

-- | Flatten dependencies so that there will be no indirect dependencies
flatten :: Ord a => Deps a -> Either (DepsError a) (Deps a)
flatten s@(Deps ds) = fmap snd . flip execStateT mempty . mapM_ (flatten' s) . M.keys $ ds where
	flatten' :: Ord a => Deps a -> a -> StateT ([a], Deps a) (Either (DepsError a)) [a]
	flatten' s' n = do
		path <- gets (view _1)
		when (preview (reversed . each) path == Just n) $ throwError (CyclicDeps $ reverse path)
		d <- gets (preview $ _2 . ix n)
		case d of
			Just d' -> return d'
			Nothing -> pushPath n $ do
				d'' <- (nub . concat . (++ [deps'])) <$> mapM (flatten' s') deps'
				modify (over _2 $ mappend (deps n d''))
				return d''
				where
					deps' = fromMaybe [] $ preview (ix n) s'
	pushPath :: MonadState ([a], Deps a) m => a -> m b -> m b
	pushPath p act = do
		modify (over _1 (p:))
		r <- act
		modify (over _1 tail)
		return r

selfDepend :: Deps a -> Deps a
selfDepend = Deps . M.mapWithKey (\s d -> d ++ [s]) . _depsMap

-- | Linearize dependencies so that all items can be processed in this order,
-- i.e. for each item all its dependencies goes before
linearize :: Ord a => Deps a -> Either (DepsError a) [a]
linearize = fmap (nub . concat . toListOf (depsMap . each) . selfDepend) . flatten

nubConcat :: Ord a => [a] -> [a] -> [a]
nubConcat xs ys = nub $ xs ++ ys
