merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) ys = smaller ++ [x] ++ merge xs remainder
                  where smaller = [a | a <- ys, a <= x]
                        remainder = drop (length smaller) ys
