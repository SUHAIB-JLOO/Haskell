data IntervalMap k v = IntervalMap v [(k, v)] deriving (Show)

singleton :: v -> IntervalMap k v
singleton v = IntervalMap v  []

(!) :: Ord k => IntervalMap k v -> k -> v
IntervalMap c [] ! _ = c
IntervalMap c ys ! key | key < (fst $ head ys) = c
                          | key > (fst $ last ys) = c
                          | otherwise = snd $ last $ takeWhile (\(k,_) -> k <= key) ys


insert :: Ord k => k -> k -> v -> IntervalMap k v -> IntervalMap k v
insert start end value (IntervalMap c []) = IntervalMap c [(start, value), (end, c)]
insert start end value (IntervalMap c ys) | start > (fst $ head ys) = IntervalMap c $ [(fst $ head ys, snd $ head ys), (start, value), (end, c)] ++ ys
                                          | otherwise = IntervalMap c $ [(start, value), (end, c)] ++ ys


instance Functor (IntervalMap k) where
fmap f (IntervalMap c ys) = IntervalMap (f c) ([(start, (f value)) | (start, value) <- ys])
value <$ (IntervalMap c ys) = IntervalMap value ([(start, value) | (start, _) <- ys])


instance Ord k => Applicative (IntervalMap k) where
pure c = IntervalMap c []
(IntervalMap c xs) <*> (IntervalMap r ys) = IntervalMap (c r) ([(start, (c r)) | (start, value) <- ys])



a = singleton 'a' :: IntervalMap Int Char
b = insert 10 20 'b' a
c = insert 9 21 'c' b
d = insert 5 15 'd' c
e = insert 14 22 'e' d 
f = insert 10 19 'f' e



i = insert 5 10 110 $ insert 10 15 90 $ singleton 100 :: IntervalMap Int Int
j = insert 5 10 (-) $ insert 10 15 (*) $ singleton (+) :: IntervalMap Int (Int -> Int -> Int)
k = insert 3 18 2 $ singleton 10 :: IntervalMap Int Int



-- > [a ! x| x <- [1..25]] "aaaaaaaaaaaaaaaaaaaaaaaaa" 
-- > [b ! x| x <- [1..25]] "aaaaaaaaabbbbbbbbbbaaaaaa" 
-- > [c ! x| x <- [1..25]] "aaaaaaaaccccccccccccaaaaa" 
-- > [d ! x| x <- [1..25]] "aaaaddddddddddccccccaaaaa" 
-- > [e ! x| x <- [1..25]] "aaaadddddddddeeeeeeeeaaaa" 
-- > [f ! x| x <- [1..25]] "aaaadddddfffffffffeeeaaaa"

-- > [(x,g ! x)| x <- [1..25]] [(1,97),(2,97),(3,97),(4,97),(5,100),(6,100),(7,100),(8,100),(9,100),(10,10 2),(11,102),(12,102),(13,102),(14,102),(15,102),(16,102),(17,102),(18,102), (19,101),(20,101),(21,101),(22,97),(23,97),(24,97),(25,97)]
-- > [(x,h ! x)| x <- [1..25]] [(1,"Hello"),(2,"Hello"),(3,"Hello"),(4,"Hello"),(5,"Hello"),(6,"Hello"),(7 ,"Hello"),(8,"Hello"),(9,"Hello"),(10,"Hello"),(11,"Hello"),(12,"Hello"),(1 3,"Hello"),(14,"Hello"),(15,"Hello"),(16,"Hello"),(17,"Hello"),(18,"Hello") ,(19,"Hello"),(20,"Hello"),(21,"Hello"),(22,"Hello"),(23,"Hello"),(24,"Hell o"),(25,"Hello")]

-- > [(x,l ! x)| x <- [1..20]] [(1,110),(2,110),(3,102),(4,102),(5,108),(6,108),(7,108),(8,108),(9,108),(1 0,180),(11,180),(12,180),(13,180),(14,180),(15,102),(16,102),(17,102),(18,1 10),(19,110),(20,110)]
