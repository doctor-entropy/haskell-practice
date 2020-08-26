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

-- 5
curry' :: ((a, b) -> c) -> a -> (b -> c)
curry' f = \x -> \y -> f (x, y)

-- uncurry' :: a -> b -> c -> ((a -> b) -> c)
-- uncurry' f a b = f (a, b)

-- 6
type Bit = Int

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold (== []) (take 8) (drop 8)

map' :: Eq a => (a -> b) -> [a] -> [b]
map' f = unfold (== []) (f . head) (drop 1)

iterate' :: Eq a => (a -> a) -> a -> [a]
iterate' f x = unfold (const False) id f x

-- 7 and 8
-- In ParityBitBinaryStringTransmitter.hs file

-- 9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g []          = []
altMap f g (n1:[])     = (f n1) : []
altMap f g (n1:n2:ns)  = (f n1) : (g n2) : (altMap f g ns)

-- 10
luhnDouble :: Int -> Int
luhnDouble n | double > 9 = double - 9
             | otherwise = double
             where double = 2 * n

luhn :: [Int] -> Bool
-- luhn n1 n2 n3 n4 = if luhnResult == 0 then True else False
--                    where luhnResult = ((luhnDouble n1) + n2 + (luhnDouble n3) + n4) `mod` 10
luhn ns | sum (altMap luhnDouble id ns) `mod` 10 == 0 = True
        | otherwise = False
