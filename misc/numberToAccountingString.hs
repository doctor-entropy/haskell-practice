numberToAccountingString :: (Ord a, Num a, Show a) => a -> String
numberToAccountingString num | num < 0 = "(" ++ show (abs num) ++ ")"
                             | otherwise = show num
