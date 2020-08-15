add :: Int -> (Int -> Int)
add = \x -> (\y -> x + y)

twice :: (a -> a) -> a -> a
twice f x = f (f x)

map :: (a -> b) -> [a] -> [b]
map f xs = [f x | x <- xs]

mapRecursion :: (a -> b) -> [a] -> [b]
mapRecursion _ [] = []
mapRecursion f (x:xs) = f x : mapRecursion f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = [x | x <- xs, f x]

filterGuards :: (a -> Bool) -> [a] -> [a]
filterGuards _ [] = []
filterGuards f (x:xs) | f x = x : filterGuards f xs
                      | otherwise = filterGuards f xs

sum' :: Num a => [a] -> a
sum' = foldr (+) 0

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f v [] = v
foldl' f v (x:xs) = foldl' f (f v x) xs
