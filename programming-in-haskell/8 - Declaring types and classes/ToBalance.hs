-- Exercises
-- 4

data Tree a = Leaf a | Node (Tree a) (Tree a)
                deriving Show

halve :: [a] -> ([a], [a])
halve xs = (take half xs, drop half xs)
            where half = quot (length xs) 2

balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs = Node (balance (fst half)) (balance (snd half))
            where half = halve xs