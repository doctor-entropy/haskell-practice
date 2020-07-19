maxList :: (Ord a) => [a] -> a
maxList [] = error "Cannot find Max of empty list"
maxList [x] = x
maxList (x:xs) = max x (maxList xs)