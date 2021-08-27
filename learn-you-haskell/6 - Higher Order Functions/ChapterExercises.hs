-- elem implementation using foldl
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else False) False ys

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldr (\x acc -> acc ++ [x]) []

product' :: (Num a) => [a] -> a
product' = foldl1 (*)

filter' :: (Eq a) => (a -> Bool) -> [a] -> [a]
filter' f = foldl (\acc x -> if f x then acc ++ [x] else acc) []

head' :: [a] -> a
head' = foldl1 (\x _ -> x)

last' :: [a] -> a
last' = foldr1 (\_ x -> x)

-- Function composition definition
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- f . g = \x -> f (g x)

-- Finding the sum of all odd squares that are smaller than 10,000
sumOddSquares :: Integer
sumOddSquares = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

sumOddSquares' :: Integer
sumOddSquares' = 
    let oddSquares = filter odd . map (^2) $ [1..]
        takeUntil10000 = takeWhile (<10000)
    in sum . takeUntil10000 $ oddSquares
