import Data.Char
-- Exercises
-- 1
mapAndFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapAndFilter f p = map f . filter p

-- 2
-- a)
all' :: (a -> Bool) -> [a] -> Bool
all' p = and . (map p)

-- b)
any' :: (a -> Bool) -> [a] -> Bool
any' p = or . (map p)

-- c)
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p (x:xs) | p x = [x] ++ takeWhile' p xs 
                    | otherwise = []

-- d)
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = x:xs

-- 3
mapf :: (a -> b) -> [a] -> [b]
mapf f = foldr (\x xs -> f x : xs) []

filt :: (a -> Bool) -> a -> [a]
filt p x | p x = [x] 
         | otherwise = []

filterp :: (a -> Bool) -> [a] -> [a]
filterp p = foldr (\x xs -> (filt p x) ++ xs) []

-- Even simpler (From book)
filterp' :: (a -> Bool) -> [a] -> [a]
filterp' p = foldr (\x xs -> if p x then x:xs else xs) []

-- 4
dec2int :: [Int] -> Int
dec2int ns = sum [n * 10^p | (n, p) <- zip (reverse ns) [0..]]

dec2int' :: [Int] -> Int
dec2int' = foldl (\x y -> 10*x + y) 0

-- 6
type Bit = Int

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold (== []) (take 8) (drop 8)

map' :: Eq a => (a -> b) -> [a] -> [b]
map' f = unfold (== []) (f . head) (drop 1)

-- iterate' :: (a -> b) -> a -> [b]
-- iterate' f = unfold ((== 8) . length) (f) (f)
