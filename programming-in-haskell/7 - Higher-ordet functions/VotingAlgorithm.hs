import Data.List

count :: Eq a => a -> [a] -> Int
count x xs = sum [1 | x' <- xs, x' == x]

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = x : unique (filter (/= x) xs)

result :: Ord a => [a] -> [(Int, a)]
result xs = sort [(count u xs, u) | u <- unique xs]

winner :: Ord a => [a] -> a
winner  = snd . last . result
