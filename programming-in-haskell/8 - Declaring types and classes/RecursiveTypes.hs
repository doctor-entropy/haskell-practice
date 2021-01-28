-- Chapter example of Recursive type section 8.4
data Tree a = Leaf a | Node (Tree a) a (Tree a)
                deriving Show

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
         (Node (Leaf 6) 7 (Leaf 9))

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

occursEff :: Ord a => a -> Tree a -> Bool
occursEff x (Leaf y) = x == y
occursEff x (Node l y r) | x == y = True
                         | x < y = occursEff x l
                         | otherwise = occursEff x r
