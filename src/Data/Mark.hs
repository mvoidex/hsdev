{-# LANGUAGE ViewPatterns, OverloadedStrings, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, RankNTypes, MultiParamTypeClasses, FunctionalDependencies #-}

module Data.Mark (
	Point(..), point, (.-.), (.+.), till, Size, linesSize, stringSize,
	Region(..), regionLines, emptyRegion,
	line,
	region, regionSize, at,
	-- * Mappings
	Map(..), apply, back,
	cut, insert,
	cutRegion, insertRegion,
	-- * Edited data
	Contents, Edit(..),
	EditM(..), editRegion, mapRegion, runEdit, edit, editEval,
	Prefix(..), prefix, Suffix(..), suffix, concatCts, splitCts,
	-- * Editable class
	Editable(..), measure,
	-- * Actions
	EditAction(..),
	Replace(..), run
	) where

import Prelude hiding (splitAt, length, lines, unlines)

import Control.Arrow ((&&&))
import Control.Applicative
import Control.Lens (view)
import Control.Lens.Iso
import Control.Monad.State
import Data.Aeson
import qualified Data.List as List (splitAt, length, break, intercalate)
import Data.Text (Text)
import qualified Data.Text as T (splitAt, length, split, intercalate)
import Data.Monoid

import HsDev.Util ((.::))

-- | Point at text: line and column
data Point = Point {
	pointLine :: Int,
	pointColumn :: Int }
		deriving (Eq, Ord, Read, Show)

instance ToJSON Point where
	toJSON (Point l c) = object ["line" .= l, "column" .= c]

instance FromJSON Point where
	parseJSON = withObject "point" $ \v -> Point <$> v .:: "line" <*> v .:: "column"

-- | Distance between points is measured in lines and columns.
-- And it is defined, that distance between point at l:c and point (l + 1):0 is one line no matter c is
-- because we need to go to new line to reach destination point
-- Columns are taken into account only if points are on the same line
-- @pt .-. base@ is distance from @base@ to @pt@
-- Distance can't be less then zero lines and columns
(.-.) :: Point -> Point -> Point
(Point l c) .-. (Point bl bc)
	| bl < l = Point (l - bl) c
	| bl == l = Point 0 (max 0 (c - bc))
	| otherwise = Point 0 0

-- | Opposite to ".-.", @(pt .-. base) .+. base = pt@
(.+.) :: Point -> Point -> Point
(Point l c) .+. (Point bl bc)
	| l == 0 = Point bl (c + bc)
	| otherwise = Point (l + bl) c

point :: Int -> Int -> Point
point l c
	| l < 0 = error "Line can't be less then zero"
	| c < 0 = error "Column can't be less then zero"
	| otherwise = Point l c

-- | Region from "Point" to "Point"
data Region = Region {
	regionFrom :: Point,
	regionTo :: Point }
		deriving (Eq, Ord, Read, Show)

type Size = Point

instance Monoid Size where
	mempty = Point 0 0
	l `mappend` r = r .+. l

-- | Region from one @Point@ to another
till :: Point -> Point -> Region
l `till` r = Region (min l r) (max l r)

-- | Distance in @n@ lines
linesSize :: Int -> Point
linesSize n = Point n 0

-- | Distance in @n@ chars within one line
stringSize :: Int -> Point
stringSize n = Point 0 n

instance ToJSON Region where
	toJSON (Region f t) = object ["from" .= f, "to" .= t]

instance FromJSON Region where
	parseJSON = withObject "region" $ \v -> Region <$> v .:: "from" <*> v .:: "to"

-- | "Region" height in lines. Any "Region" at least of one line height
regionLines :: Region -> Int
regionLines r = succ $ pointLine (regionTo r) - pointLine (regionFrom r)

-- | Is "Region" empty
emptyRegion :: Region -> Bool
emptyRegion r = regionTo r == regionFrom r

-- | n'th line region, starts at the beginning of line and ends on the next line
line :: Int -> Region
line l = region (Point l 0) (Point (succ l) 0)

-- | Make region
region :: Point -> Point -> Region
region f t = Region (min f t) (max f t)

-- | Make region from starting point and its size
regionSize :: Point -> Size -> Region
regionSize pt sz = region pt (sz .+. pt)

-- | Get contents at specified region
at :: Editable a => Contents a -> Region -> Contents a
at cts r =
	onHead (snd . splitAt (pointColumn $ regionFrom r)) .
	onLast (fst . splitAt (pointColumn $ regionTo r)) .
	take (regionLines r) .
	drop (pointLine (regionFrom r)) $
	cts
	where
		onHead :: (a -> a) -> [a] -> [a]
		onHead _ [] = []
		onHead f (x:xs) = f x : xs
		onLast :: (a -> a) -> [a] -> [a]
		onLast _ [] = []
		onLast f l@(last -> x) = init l ++ [f x]

-- | Main idea is that there are only two basic actions , that chances regions: inserting and cutting
-- When something is cutted out or inserted in, region positions must be updated
-- All editings can be represented as many cuts and inserts, so we can combine them to get function
-- which maps source regions to regions on updated data
-- Because insert is dual to cut (and therefore composes iso), we can also get function to map regions back
-- Combining this functions while edit, we get function, that maps regions from source data to edited one
-- To get back function, we must also combine opposite actions, or we can represent actions as isomorphisms
-- Same idea goes for modifying contents, represent each action as isomorphism and combine them together
newtype Map = Map { mapIso :: Iso' Region Region }

instance Monoid Map where
	mempty = Map $ iso id id
	(Map l) `mappend` (Map r) = Map (r . l)

-- | Apply mapping
apply :: Map -> Region -> Region
apply = view . mapIso

-- | Back mapping
back :: Map -> Map
back (Map f) = Map (from f)

-- | Cut region mapping
cut :: Region -> Map
cut rgn = Map $ iso (cutRegion rgn) (insertRegion rgn)

-- | Opposite to "cut"
insert :: Region -> Map
insert = back . cut

-- | Update second region position as if it was data cutted at first region
cutRegion :: Region -> Region -> Region
cutRegion (Region is ie) (Region s e) = Region
	(if is < s then (s .-. ie) .+. is else s)
	(if is < e then (e .-. ie) .+. is else e)

-- | Update second region position as if it was data inserted at first region
insertRegion :: Region -> Region -> Region
insertRegion (Region is ie) (Region s e) = Region
	(if is <= s then (s .-. is) .+. ie else s)
	(if is < e then (e .-. is) .+. ie else e)

-- | Contents is list of lines
type Contents a = [a]

-- | Edit data
data Edit a = Edit {
	editCts :: Contents a -> Contents a, -- ^ Edit contents splitted by lines
	editMap :: Map } -- ^ Map region from source contents to edited

instance Monoid (Edit a) where
	mempty = Edit id mempty
	(Edit fl ml) `mappend` (Edit fr mr) = Edit (fr . fl) (ml `mappend` mr)

-- | Edit monad is state on "Edit", it also collects region mappings
newtype EditM s a = EditM { runEditM :: State (Edit s) a }
	deriving (Functor, Applicative, Monad, MonadState (Edit s))

-- | Basic edit action in monad
-- It takes region, region edit function and contents updater
-- and passes mapped region to these functions to get new state
editRegion :: Region -> (Region -> Edit a) -> EditM a ()
editRegion rgn edit' = do
	rgn' <- mapRegion rgn
	modify (`mappend` (edit' rgn'))

-- | Get mapped region
mapRegion :: Region -> EditM a Region
mapRegion rgn = gets (($ rgn) . apply . editMap)

-- | Run edit monad
runEdit :: Editable s => EditM s a -> (a, Edit s)
runEdit act = runState (runEditM act) mempty

-- | Edit contents
edit :: Editable s => s -> EditM s a -> s
edit cts = snd . editEval cts

-- | Eval edit
editEval :: Editable s => s -> EditM s a -> (a, s)
editEval cts act = (v, unlines . editCts st . lines $ cts) where
	(v, st) = runEdit act

-- | Prefix of contents cutted at some point
data Prefix a = Prefix {
	prefixLines :: [a],
	prefixLine :: a }
		deriving (Eq, Ord, Read, Show)

instance Functor Prefix where
	fmap f (Prefix ls l) = Prefix (fmap f ls) (f l)

-- | Make prefix from full contents
prefix :: Contents a -> Prefix a
prefix cts = Prefix (init cts) (last cts)

-- | Suffix of contents
data Suffix a = Suffix {
	suffixLine :: a,
	suffixLines :: [a] }
		deriving (Eq, Ord, Read, Show)

instance Functor Suffix where
	fmap f (Suffix l ls) = Suffix (f l) (fmap f ls)

suffix :: Contents a -> Suffix a
suffix cts = Suffix (head cts) (tail cts)

-- | Concat prefix and suffix. First line of suffix is appended to last line of prefix
concatCts :: Monoid a => Prefix a -> Suffix a -> Contents a
concatCts (Prefix ps p) (Suffix s ss) = ps ++ [p `mappend` s] ++ ss

-- | Split contents at point. First argument is function to split one line at position.
splitCts :: Editable a => Point -> Contents a -> (Prefix a, Suffix a)
splitCts (Point l c) cts = (Prefix (take l cts) p, Suffix s (drop (succ l) cts)) where
	(p, s) = splitAt c (cts !! l)

class Monoid a => Editable a where
	splitAt :: Int -> a -> (a, a)
	length :: a -> Int
	lines :: a -> [a]
	unlines :: [a] -> a

instance Editable String where
	splitAt = List.splitAt
	length = List.length
	lines s = case List.break (== '\n') s of
		(pre, "") -> [pre]
		(pre, _:post) -> pre : lines post
	unlines = List.intercalate "\n"

instance Editable Text where
	splitAt = T.splitAt
	length = T.length
	lines = T.split (== '\n')
	unlines = T.intercalate "\n"

-- | Contents size
measure :: Editable s => Contents s -> Size
measure [] = error "Invalid argument"
measure cts = Point (pred $ List.length cts) (length $ last cts)

class EditAction e where
	erase :: Editable s => Region -> e s ()
	write :: Editable s => Point -> s -> e s ()
	replace :: Editable s => Region -> s -> e s ()

instance EditAction EditM where
	erase rgn = editRegion rgn (\r -> Edit (erase' r) (cut r)) where
		erase' :: Editable a => Region -> Contents a -> Contents a
		erase' rgn' cts = fst (splitCts (regionFrom rgn') cts) `concatCts` snd (splitCts (regionTo rgn') cts)

	write pt cts = editRegion (pt `regionSize` measure cts') (\r -> Edit (write' r) (insert r)) where
		cts' = lines cts
		write' rgn' origin = prefix (before' `concatCts` suffix cts') `concatCts` after' where
			(before', after') = splitCts (regionFrom rgn') origin

	replace rgn cts = erase rgn >> write (regionFrom rgn) cts

-- | Serializable replace action
data Replace s a = Replace {
	replaceRegion :: Region,
	replaceWith :: s }
		deriving (Eq, Read, Show)

instance (Editable s, ToJSON s) => ToJSON (Replace s a) where
	toJSON (Replace e c) = object ["region" .= e, "contents" .= c]

instance (Editable s, FromJSON s) => FromJSON (Replace s a) where
	parseJSON = withObject "edit" $ \v -> Replace <$> v .:: "region" <*> v .:: "contents"

instance EditAction Replace where
	erase rgn = Replace rgn mempty
	write pt cts = Replace (region pt pt) cts
	replace rgn cts = Replace rgn cts

-- | Run replace actions to get monadic action
run :: Editable s => [Replace s ()] -> EditM s ()
run = mapM_ (uncurry replace . (replaceRegion &&& replaceWith))
