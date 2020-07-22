-- 1 Sum of Squares
sumOfSquares :: Int -> Int
sumOfSquares n = sum [i^2 | i <- [1..n]]

-- 2
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

-- 3
squareGrid :: Int -> [(Int, Int)]
squareGrid n = [g | g <- grid n n, fst g /= snd g]

-- 4
replicate' :: Int -> a -> [a]
replicate' n a = [a | _ <- [1..n]]

-- 5 Pythagorean triples
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- enum,
                       y <- enum,
                       z <- enum,
                       x^2 + y^2 == z^2]
                    where enum = [1..n]

-- 6 Perfect numbers
factors :: Int -> [Int]
factors n = [i | i <- [1..(quot n 2)], n `mod` i == 0]

perfect :: Int -> [Int]
perfect n = [i | i <- [1..n], sum (factors i) == i]

-- 7
-- concat [[(x, 3), (x,4)] | x <- [1,2]]

-- 8
find :: Eq a => a -> [(a, b)] -> [b]
find k ts = [v | (k', v) <- ts, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions k xs = find k (zip xs [0..])

-- 9 Scalar Product
scalarProduct :: Num a => [a] -> [a] -> a
scalarProduct xs ys = sum [x * y | (x, y) <- zip xs ys]
