-- Exercise problem 4
data Tree a = Leaf a | Node (Tree a) (Tree a)
                    deriving Show

t1 :: Tree Int
t1 = Node (Node (Leaf 1) (Leaf 4))
         (Node (Leaf 6) (Leaf 9))

t2 :: Tree Int
t2 = Node (Node (Leaf 1) (Leaf 4))
         (Node (Leaf 6) (Node (Leaf 9) (Leaf 11)))

t3 :: Tree Int
t3 = Node (Node (Leaf 1) (Leaf 4))
         (Node (Leaf 6) (Node (Leaf 9) (Node (Leaf 14) (Leaf 13))))

t4 :: Tree Int
t4 = Node (Node (Leaf 4) (Node (Leaf 6) (Leaf 7)))
          (Leaf 3)

countLeaves :: Tree a -> Int
countLeaves (Leaf _) = 1
countLeaves (Node x y) = countLeaves x + countLeaves y

isBalanced :: Tree a -> Bool
isBalanced (Leaf _) = True
isBalanced (Node x y) = abs (countLeaves x - countLeaves y) <= 1
