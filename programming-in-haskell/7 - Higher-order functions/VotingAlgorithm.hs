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

-- Alternative Vote
ballots :: [[String]]
ballots = [["RED", "GREEN"],
            ["BLUE"],
            ["GREEN", "RED", "BLUE"],
            ["BLUE", "GREEN", "RED"],
            ["GREEN"]]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
                    [w]    -> w
                    (l:cs) -> winner' (elim l bs)
