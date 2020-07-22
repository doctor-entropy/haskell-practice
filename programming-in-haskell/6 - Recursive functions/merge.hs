-- Exercise problems
-- 7
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) ys = smaller ++ [x] ++ merge xs remainder
                  where smaller = [a | a <- ys, a <= x]
                        remainder = drop (length smaller) ys

-- 8
halver :: [a] -> ([a], [a])
halver xs = (take half xs, drop half xs)
            where half = quot (length xs) 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (fst halved) (snd halved)
                  where halved = halver xs
