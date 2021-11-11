{-# LANGUAGE InstanceSigs #-}

data Range a = Range {
    lower_bound :: a,
    upper_bound :: a
} deriving Show

-- Creates a new range
newRange :: Ord a => a -> a -> Range a
newRange low up | low <= up = Range { lower_bound = low, upper_bound = up }
                | otherwise = Range { lower_bound = up, upper_bound = low }

-- Checks if a given value is inside a range
isInRange :: Ord a => a -> Range a -> Bool
isInRange value range = value >= lower_bound range && value <= upper_bound range

-- a absorbs b if a contains b
-- [1..10] absorbs [2..5]
absorbs :: Ord a => Range a -> Range a -> Bool
absorbs a b = lower_bound a <= lower_bound b && upper_bound a >= upper_bound b

-- Checks if 2 ranges intersect eachother
-- Usually should be used after a check for absortion
intersects :: Ord a => Range a -> Range a -> Bool
intersects a b = (upper_bound a >= lower_bound b && upper_bound a <= upper_bound b) ||
                 (upper_bound b >= lower_bound a && upper_bound b <= upper_bound a)

-- This assumes the list is ordered
-- Inserts a range with a value inside a list of tuples range value
insertRangeWithValue :: (Ord a, Enum a) => [(Range a, b)] -> Range a -> b -> [(Range a, b)]
insertRangeWithValue [] range value = [(range, value)]
insertRangeWithValue ((r, v) : rs) range value | upper_bound range < lower_bound r = (range, value) : (r, v) : rs
                                               | range `absorbs` r = insertRangeWithValue rs range value
                                               | lower_bound range > upper_bound r = (r,v) : insertRangeWithValue rs range value
                                               | range `intersects` r = insert' ((r, v) : rs) range value
    where
        insert' :: (Ord a, Enum a) => [(Range a, b)] -> Range a -> b -> [(Range a, b)]
        insert' ((r, v) : rs) range value | r `absorbs` range = ((newRange (lower_bound r)(pred $ lower_bound range)), v)
                                                              : (range, value)
                                                              : ((newRange (succ $ upper_bound range) (upper_bound r)), v) : rs
                                          | lower_bound range >= lower_bound r
                                                              = ((newRange (lower_bound r) (pred $ lower_bound range)), v)
                                                              : insertRangeWithValue rs range value
                                          | lower_bound range <= lower_bound r = (range, value)
                                                                               : (newRange (succ $ upper_bound range) (upper_bound r), v) : rs 

data IntervalMap k v = IntervalMap {
    mapped_ranges :: [(Range k, v)],
    def :: v
} deriving Show

singleton :: v -> IntervalMap k v
singleton v = IntervalMap {mapped_ranges = [], def = v}

(!) :: Ord k => IntervalMap k v -> k -> v
(!) imap key = lookup' (mapped_ranges imap) (def imap) key
    where
        lookup' :: Ord k => [(Range k, v)] -> v -> k -> v
        lookup' [] def key = def
        lookup' ((range, value):rs) def key | key `isInRange` range = value
                                            | otherwise = lookup' rs def key

insert :: (Ord k, Enum k) => k -> k -> v -> IntervalMap k v -> IntervalMap k v
insert low up value imap = let range = newRange low up
                               old_map = mapped_ranges imap
                               def_val = def imap
                            in IntervalMap {mapped_ranges = insertRangeWithValue old_map range value, def = def_val}

-- Testing #1
a = singleton 'a' :: IntervalMap Int Char
b = insert 10 20 'b' a
c = insert 9 21 'c' b
d = insert 5 15 'd' c
e = insert 14 22 'e' d
f = insert 10 19 'f' e

instance Functor (IntervalMap k) where
    fmap :: (a -> b) -> IntervalMap k a -> IntervalMap k b
    fmap f imap = let old_mapped = mapped_ranges imap
                      old_def = def imap
                   in IntervalMap { mapped_ranges = map (\(a,b) -> (a, f b)) old_mapped,
                                    def = f old_def }
-- Testing #2
g = fmap fromEnum f
h = "Hello" <$ g

instance (Ord k, Enum k) => Applicative (IntervalMap k) where
    pure :: v -> IntervalMap k v
    pure = singleton

    (<*>) :: IntervalMap k (a -> b) -> IntervalMap k a -> IntervalMap k b
    (<*>) imap1 imap2 = IntervalMap { mapped_ranges = applyWithRanges (mapped_ranges imap1) (mapped_ranges imap2) (def imap1) (def imap2),
                                      def = ((def imap1) $ def imap2) }

-- Function used for the application
-- Arguments:
-- [(Range k, a -> b)] <- application range
-- [(Range k, a)] <- applied range
-- (a -> b) <- default application (in case application not in range)
-- a <- default value (in case value not in range)
applyWithRanges :: (Ord k, Enum k) => [(Range k, a -> b)] -> [(Range k, a)] -> (a -> b) -> a -> [(Range k, b)]
applyWithRanges [] [] _ _ = []
applyWithRanges [] ((rB, vB) : rBs) func defV = (rB, func vB) : applyWithRanges [] rBs func defV
applyWithRanges ((rA, fA) : rAs) [] func defV = (rA, fA defV) : applyWithRanges rAs [] func defV
applyWithRanges ((rA, fA) : rAs) ((rB, vB) : rBs) func defV
                       | upper_bound rB < lower_bound rA = (rB, func vB) : applyWithRanges ((rA, fA) : rAs) rBs func defV
                       | lower_bound rB > upper_bound rA = (rA, fA defV) : applyWithRanges rAs ((rB, vB) : rBs) func defV
                       | rA `absorbs` rB = (newRange (lower_bound rA) (pred $ lower_bound rB), fA defV)
                                         : (rB, fA vB)
                                         : applyWithRanges ((newRange (succ $ upper_bound rB) (upper_bound rA), fA) : rAs) rBs func defV
                       | rB `absorbs` rA = (newRange (lower_bound rB) (pred $ lower_bound rA), func vB)
                                         : (rB, fA vB)
                                         : applyWithRanges rAs ((newRange (succ $ upper_bound rA) (upper_bound rB), vB) : rBs) func defV
                       | rA `intersects` rB = applyIntersected ((rA, fA) : rAs) ((rB, vB) : rBs) func defV

-- case for applying when there is an intersection of ranges
-- (assumes no absorbtion)
applyIntersected :: (Ord k, Enum k) => [(Range k, a -> b)] -> [(Range k, a)] -> (a -> b) -> a -> [(Range k, b)]
applyIntersected ((rA, fA) : rAs) ((rB, vB) : rBs) func defV
          | lower_bound rA < lower_bound rB = (newRange (lower_bound rA) (pred $ lower_bound rB), fA defV)
                                            : (newRange (lower_bound rB) (upper_bound rA), fA vB)
                                            : applyWithRanges rAs ((newRange (succ $ upper_bound rA) (upper_bound rB), vB):rBs) func defV
          | lower_bound rB < lower_bound rA = (newRange (lower_bound rB) (pred $ lower_bound rA), func vB)
                                            : (newRange (lower_bound rA) (upper_bound rB), fA vB)
                                            : applyWithRanges ((newRange (succ $ upper_bound rB) (upper_bound rA), fA):rAs) rBs func defV

-- Testing
i = insert 5 10 110 $ insert 10 15 90 $ singleton 100 :: IntervalMap Int Int
j = insert 5 10 (-) $ insert 10 15 (*) $ singleton (+) :: IntervalMap Int (Int -> Int -> Int)
k = insert 3 18 2 $ singleton 10 :: IntervalMap Int Int
l = j <*> i <*> k
