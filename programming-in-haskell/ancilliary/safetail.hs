safeTail :: Eq a => [a] -> [a]
safeTail xs | xs == []  = []
            | otherwise = tail xs 
