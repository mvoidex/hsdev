{-# LANGUAGE ViewPatterns, OverloadedStrings, GeneralizedNewtypeDeriving, TypeSynonymInstances #-}

module Data.Mark (
	Point(..), (.-.), (.+.), Size, linesSize, stringSize,
	Region(..), regionLines, emptyRegion,
	line,
	region, regionSize, at,
	insert, cut,

	Mark(..), withMarkRegion, Marks, onMarks,
	Edited(..), edited, EditM(..), edit, mapRegion, runEdit, evalEdit,
	Contents(..), Prefix(..), prefix, Suffix(..), suffix,
	concatCts, splitCts,
	erase, write, replace,
	text, untext
	) where

import Control.Arrow (second)
import Control.Applicative
import Control.Monad.State
import Data.Aeson
import Data.List (unfoldr, intercalate)
import Data.Monoid

import HsDev.Util ((.::))

data Point = Point {
	pointLine :: Int,
	pointColumn :: Int }
		deriving (Eq, Ord, Read, Show)

instance ToJSON Point where
	toJSON (Point l c) = object ["line" .= l, "column" .= c]

instance FromJSON Point where
	parseJSON = withObject "point" $ \v -> Point <$> v .:: "line" <*> v .:: "column"

(.-.) :: Point -> Point -> Point
(Point l c) .-. (Point bl bc)
	| bl < l = Point (l - bl) c
	| bl == l = Point 0 (max 0 (c - bc))
	| otherwise = Point 0 0

(.+.) :: Point -> Point -> Point
(Point l c) .+. (Point bl bc)
	| l == 0 = Point bl (c + bc)
	| otherwise = Point (l + bl) c

data Region = Region {
	regionFrom :: Point,
	regionTo :: Point }
		deriving (Eq, Ord, Read, Show)

type Size = Point

instance Monoid Size where
	mempty = Point 0 0
	l `mappend` r = r .+. l

linesSize :: Int -> Point
linesSize n = Point n 0

stringSize :: Int -> Point
stringSize n = Point 0 n

instance ToJSON Region where
	toJSON (Region f t) = object ["from" .= f, "to" .= t]

instance FromJSON Region where
	parseJSON = withObject "region" $ \v -> Region <$> v .:: "from" <*> v .:: "to"

regionLines :: Region -> Int
regionLines r = succ $ pointLine (regionTo r) - pointLine (regionFrom r)

emptyRegion :: Region -> Bool
emptyRegion r = regionTo r == regionFrom r

line :: Int -> Region
line l = region (Point l 0) (Point (succ l) 0)

region :: Point -> Point -> Region
region from to = Region (min from to) (max from to)

regionSize :: Point -> Size -> Region
regionSize pt sz = region pt (sz .+. pt)

at :: Contents a -> Region -> Contents a
at (Contents cts) r =
	Contents .
	onHead (drop $ pointColumn $ regionFrom r) .
	onLast (take $ pointColumn $ regionTo r) .
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

-- | Update second region position as if it was data inserted at first region
insert :: Region -> Region -> Region
insert (Region is ie) (Region s e) = Region
	(if is <= s then (s .-. is) .+. ie else s)
	(if is < e then (e .-. is) .+. ie else e)

-- | Update second region position as if it was data cutted at first region
cut :: Region -> Region -> Region
cut (Region is ie) (Region s e) = Region
	(if is < s then (s .-. ie) .+. is else s)
	(if is < e then (e .-. ie) .+. is else e)

-- | Mark region with data
data Mark a = Mark {
	mark :: a,
	markRegion :: Region }
		deriving (Eq, Ord, Read, Show)

instance Functor Mark where
	fmap f (Mark v r) = Mark (f v) r

instance ToJSON a => ToJSON (Mark a) where
	toJSON (Mark m r) = object ["mark" .= m, "region" .= r]

instance FromJSON a => FromJSON (Mark a) where
	parseJSON = withObject "mark" $ \v -> Mark <$> v .:: "mark" <*> v .:: "region"

withMarkRegion :: (Region -> Region) -> Mark a -> Mark a
withMarkRegion f (Mark v r) = Mark v (f r)

type Marks a = [Mark a]

-- | Lift region function like `insert` or `cut`
onMarks :: (Region -> Region) -> Marks a -> Marks a
onMarks f = map (withMarkRegion f)

data Edited a = Edited {
	editedContents :: Contents a,
	editedRegions :: Region -> Region }

instance Functor Edited where
	fmap f (Edited cts r) = Edited (fmap f cts) r

edited :: Contents a -> Edited a
edited cts = Edited cts id

newtype EditM s a = EditM { runEditM :: State (Edited s) a }
	deriving (Functor, Applicative, Monad, MonadState (Edited s))

edit :: Region -> (Region -> Region -> Region) -> (Region -> Contents a -> Contents a) -> EditM a ()
edit rgn fr fc = do
	rgn' <- mapRegion rgn
	modify (\(Edited cts r) -> Edited (fc rgn' cts) (fr rgn' . r))

mapRegion :: Region -> EditM a Region
mapRegion rgn = gets (($ rgn) . editedRegions)

runEdit :: Contents s -> EditM s a -> (a, Contents s)
runEdit cts act = second editedContents $ runState (runEditM act) (edited cts)

evalEdit :: Contents s -> EditM s a -> Contents s
evalEdit cts act = snd $ runEdit cts act

data Contents a = Contents {
	contents :: [[a]] }
		deriving (Eq, Ord, Read, Show)

instance Functor Contents where
	fmap f (Contents cts) = Contents $ fmap (fmap f) cts

data Prefix a = Prefix {
	prefixLines :: [[a]],
	prefixLine :: [a] }
		deriving (Eq, Ord, Read, Show)

instance Functor Prefix where
	fmap f (Prefix ls l) = Prefix (fmap (fmap f) ls) (fmap f l)

prefix :: Contents a -> Prefix a
prefix (Contents cts) = Prefix (init cts) (last cts)

data Suffix a = Suffix {
	suffixLine :: [a],
	suffixLines :: [[a]] }
		deriving (Eq, Ord, Read, Show)

instance Functor Suffix where
	fmap f (Suffix l ls) = Suffix (fmap f l) (fmap (fmap f) ls)

suffix :: Contents a -> Suffix a
suffix (Contents cts) = Suffix (head cts) (tail cts)

concatCts :: Prefix a -> Suffix a -> Contents a
concatCts (Prefix ps p) (Suffix s ss) = Contents (ps ++ [p ++ s] ++ ss)

splitCts :: Point -> Contents a -> (Prefix a, Suffix a)
splitCts (Point l c) (Contents cts) = (Prefix (take l cts) (take c (cts !! l)), Suffix (drop c (cts !! l)) (drop (succ l) cts))

-- | Erase data
erase :: Region -> EditM s ()
erase rgn = edit rgn cut erase' where
	erase' :: Region -> Contents b -> Contents b
	erase' rgn' cts = fst (splitCts (regionFrom rgn') cts) `concatCts` snd (splitCts (regionTo rgn') cts)

-- | Paste data at position
write :: Point -> Contents s -> EditM s ()
write _ (Contents []) = error "Invalid argument"
write pt cts = edit (region pt pt') insert write' where
	pt' :: Point
	pt' = case contents cts of
		[] -> error "Impossible"
		[line'] -> pt { pointColumn = pointColumn pt + length line' }
		lines'@(last -> line') -> Point { pointLine = pointLine pt + pred (length lines'), pointColumn = length line' }
	write' rgn' origin = prefix (before' `concatCts` suffix cts) `concatCts` after' where
		(before', after') = splitCts (regionFrom rgn') origin

-- | Replace data with
replace :: Region -> Contents s -> EditM s ()
replace rgn cts = erase rgn >> write (regionFrom rgn) cts

text :: String -> Contents Char
text = Contents . lines' where
	lines' :: String -> [String]
	lines' = unfoldr breakLine . Just where
		breakLine :: Maybe String -> Maybe (String, Maybe String)
		breakLine (Just str) = case break (== '\n') str of
			(pre, "") -> Just (pre, Nothing)
			(pre, _:post) -> Just (pre, Just post)
		breakLine Nothing = Nothing

untext :: Contents Char -> String
untext = intercalate "\n" . contents
