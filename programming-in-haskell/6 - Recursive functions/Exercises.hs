-- 3
(^^^) :: Int -> Int -> Int
n ^^^ 1 = n
n ^^^ m = n * (n ^^^ (m - 1))
