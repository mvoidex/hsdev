{-# LANGUAGE ViewPatterns, OverloadedStrings, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, RankNTypes, MultiParamTypeClasses #-}

module Data.Mark (
	Point(..), point, (.-.), (.+.), till, Size, linesSize, stringSize,
	Range(..), rangeLines, emptyRange,
	line,
	range, rangeSize, expandLines, at,
	-- * Mappings
	Map(..), apply, back,
	cut, insert,
	cutRange, insertRange,
	-- * Edited data
	Contents, Edit(..),
	EditM(..), editRange, mapRange, runEdit, edit, editEval,
	Prefix(..), prefix, Suffix(..), suffix, concatCts, splitCts,
	-- * Editable class
	Editable(..), measure,
	-- * Actions
	EditAction(..),
	Replace(..), run
	) where

import Prelude hiding (splitAt, length, lines, unlines)

import Control.Arrow ((&&&))
import Control.Lens (view)
import Control.Lens.Iso
import Control.Monad.State
import Data.Aeson
import qualified Data.List as List (splitAt, length, break, intercalate)
import Data.Text (Text)
import qualified Data.Text as T (splitAt, length, split, intercalate)

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

-- | Range from "Point" to "Point"
data Range = Range {
	rangeFrom :: Point,
	rangeTo :: Point }
		deriving (Eq, Ord, Read, Show)

type Size = Point

instance Monoid Size where
	mempty = Point 0 0
	l `mappend` r = r .+. l

-- | Range from one @Point@ to another
till :: Point -> Point -> Range
l `till` r = Range (min l r) (max l r)

-- | Distance in @n@ lines
linesSize :: Int -> Point
linesSize n = Point n 0

-- | Distance in @n@ chars within one line
stringSize :: Int -> Point
stringSize n = Point 0 n

instance ToJSON Range where
	toJSON (Range f t) = object ["from" .= f, "to" .= t]

instance FromJSON Range where
	parseJSON = withObject "range" $ \v -> Range <$> v .:: "from" <*> v .:: "to"

-- | "Range" height in lines. Any "Range" at least of one line height
rangeLines :: Range -> Int
rangeLines r = succ $ pointLine (rangeTo r) - pointLine (rangeFrom r)

-- | Is "Range" empty
emptyRange :: Range -> Bool
emptyRange r = rangeTo r == rangeFrom r

-- | n'th line range, starts at the beginning of line and ends on the next line
line :: Int -> Range
line l = range (Point l 0) (Point (succ l) 0)

-- | Make range
range :: Point -> Point -> Range
range f t = Range (min f t) (max f t)

-- | Make range from starting point and its size
rangeSize :: Point -> Size -> Range
rangeSize pt sz = range pt (sz .+. pt)

-- | Expand range to contain full lines
expandLines :: Range -> Range
expandLines (Range (Point sline _) (Point eline _)) = Range (Point sline 0) (Point (succ eline) 0)

-- | Get contents at specified range
at :: Editable a => Contents a -> Range -> Contents a
at cts r =
	onHead (snd . splitAt (pointColumn $ rangeFrom r)) .
	onLast (fst . splitAt (pointColumn $ rangeTo r)) .
	take (rangeLines r) .
	drop (pointLine (rangeFrom r)) $
	cts
	where
		onHead :: (a -> a) -> [a] -> [a]
		onHead _ [] = []
		onHead f (x:xs) = f x : xs
		onLast :: (a -> a) -> [a] -> [a]
		onLast _ [] = []
		onLast f l@(last -> x) = init l ++ [f x]

-- | Main idea is that there are only two basic actions, that chances ranges: inserting and cutting
-- When something is cutted out or inserted in, range positions must be updated
-- All editings can be represented as many cuts and inserts, so we can combine them to get function
-- which maps source ranges to ranges on updated data
-- Because insert is dual to cut (and therefore composes iso), we can also get function to map ranges back
-- Combining this functions while edit, we get function, that maps ranges from source data to edited one
-- To get back function, we must also combine opposite actions, or we can represent actions as isomorphisms
-- Same idea goes for modifying contents, represent each action as isomorphism and combine them together
newtype Map = Map { mapIso :: Iso' Range Range }

instance Monoid Map where
	mempty = Map $ iso id id
	(Map l) `mappend` (Map r) = Map (r . l)

-- | Apply mapping
apply :: Map -> Range -> Range
apply = view . mapIso

-- | Back mapping
back :: Map -> Map
back (Map f) = Map (from f)

-- | Cut range mapping
cut :: Range -> Map
cut rgn = Map $ iso (cutRange rgn) (insertRange rgn)

-- | Opposite to "cut"
insert :: Range -> Map
insert = back . cut

-- | Update second range position as if it was data cutted at first range
cutRange :: Range -> Range -> Range
cutRange (Range is ie) (Range s e) = Range
	(if is < s then (s .-. ie) .+. is else s)
	(if is < e then (e .-. ie) .+. is else e)

-- | Update second range position as if it was data inserted at first range
insertRange :: Range -> Range -> Range
insertRange (Range is ie) (Range s e) = Range
	(if is <= s then (s .-. is) .+. ie else s)
	(if is < e then (e .-. is) .+. ie else e)

-- | Contents is list of lines
type Contents a = [a]

-- | Edit data
data Edit a = Edit {
	editCts :: Contents a -> Contents a, -- ^ Edit contents splitted by lines
	editMap :: Map } -- ^ Map range from source contents to edited

instance Monoid (Edit a) where
	mempty = Edit id mempty
	(Edit fl ml) `mappend` (Edit fr mr) = Edit (fr . fl) (ml `mappend` mr)

-- | Edit monad is state on "Edit", it also collects range mappings
newtype EditM s a = EditM { runEditM :: State (Edit s) a }
	deriving (Functor, Applicative, Monad, MonadState (Edit s))

-- | Basic edit action in monad
-- It takes range, range edit function and contents updater
-- and passes mapped range to these functions to get new state
editRange :: Range -> (Range -> Edit a) -> EditM a ()
editRange rgn edit' = do
	rgn' <- mapRange rgn
	modify (`mappend` (edit' rgn'))

-- | Get mapped range
mapRange :: Range -> EditM a Range
mapRange rgn = gets (($ rgn) . apply . editMap)

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
	erase :: Editable s => Range -> e s ()
	write :: Editable s => Point -> s -> e s ()
	replace :: Editable s => Range -> s -> e s ()

instance EditAction EditM where
	erase rgn = editRange rgn (\r -> Edit (erase' r) (cut r)) where
		erase' :: Editable a => Range -> Contents a -> Contents a
		erase' rgn' cts = fst (splitCts (rangeFrom rgn') cts) `concatCts` snd (splitCts (rangeTo rgn') cts)

	write pt cts = editRange (pt `rangeSize` measure cts') (\r -> Edit (write' r) (insert r)) where
		cts' = lines cts
		write' rgn' origin = prefix (before' `concatCts` suffix cts') `concatCts` after' where
			(before', after') = splitCts (rangeFrom rgn') origin

	replace rgn cts = erase rgn >> write (rangeFrom rgn) cts

-- | Serializable replace action
data Replace s a = Replace {
	replaceRange :: Range,
	replaceWith :: s }
		deriving (Eq, Read, Show)

instance (Editable s, ToJSON s) => ToJSON (Replace s a) where
	toJSON (Replace e c) = object ["range" .= e, "contents" .= c]

instance (Editable s, FromJSON s) => FromJSON (Replace s a) where
	parseJSON = withObject "edit" $ \v -> Replace <$> v .:: "range" <*> v .:: "contents"

instance EditAction Replace where
	erase rgn = Replace rgn mempty
	write pt cts = Replace (range pt pt) cts
	replace rgn cts = Replace rgn cts

-- | Run replace actions to get monadic action
run :: Editable s => [Replace s ()] -> EditM s ()
run = mapM_ (uncurry replace . (replaceRange &&& replaceWith))
