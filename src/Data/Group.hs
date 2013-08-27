{-# LANGUAGE TypeSynonymInstances #-}

module Data.Group (
	Group(..),
	groupSum
	) where

import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.HashMap as HM
import Data.Set (Set)
import qualified Data.Set as S

-- | Group is monoid with invertibility
-- But for our purposes we prefer two functions: `add` and `sub`.
class Eq a => Group a where
	add :: a -> a -> a
	sub :: a -> a -> a
	zero :: a

instance Eq a => Group [a] where
	add = (++)
	sub = (\\)
	zero = []

instance Ord a => Group (Set a) where
	add = S.union
	sub = S.difference
	zero = S.empty

instance (Ord k, Group a) => Group (Map k a) where
	add = M.unionWith add
	sub = M.differenceWith sub' where
		sub' x y = if z == zero then Nothing else Just z where
			z = sub x y
	zero = M.empty

instance (Ord k, Group a) => Group (HM.Map k a) where
	add = HM.unionWith add
	sub = HM.differenceWith sub' where
		sub' x y = if z == zero then Nothing else Just z where
			z = sub x y
	zero = HM.empty

-- | Sums list
groupSum :: Group a => [a] -> a
groupSum = foldr add zero
