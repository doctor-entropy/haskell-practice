euclid :: Int -> Int -> Int
euclid n m | n > m = euclids (n - m) m
           | n < m = euclids n (m - n)
           | otherwise = n
