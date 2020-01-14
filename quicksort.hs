quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallersort = quicksort [a | a <- xs, a <= x]
        biggersort = quicksort [a | a <- xs, a > x]
    in smallersort ++ [x] ++ biggersort