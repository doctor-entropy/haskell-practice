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
