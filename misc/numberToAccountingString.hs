numberToAccountingString :: (Ord a, Show a) => a -> String
numberToAccountingString num | num < 0 = show num
                             | otherwise = show num