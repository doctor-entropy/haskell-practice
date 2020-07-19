luhnDouble :: Int -> Int
luhnDouble n | double > 9 = double - 9
             | otherwise = double
             where double = 2 * n

luhn :: Int -> Int -> Int -> Int -> Bool
luhn n1 n2 n3 n4 = if luhnResult == 0 then True else False
                   where luhnResult = ((luhnDouble n1) + n2 + (luhnDouble n3) + n4) `mod` 10
