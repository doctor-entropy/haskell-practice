-- Exercise problems

-- 1
data Nat = Zero | Succ Nat
            deriving Show

add' :: Nat -> Nat -> Nat
add' Zero n = n
add' (Succ m) n = Succ (add' m n)

mult :: Nat -> Nat -> Nat
mult (Succ Zero) n = n
mult (Succ m) n = add' n (mult m n)  

-- 2
data Tree a = Leaf a | Node (Tree a) a (Tree a)
                     deriving Show

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = (compare x y) == EQ
occurs x (Node l y r) | (compare x y) == EQ = True
                      | (compare x y) == LT = occurs x l
                      | (compare x y) == GT = occurs x r

-- Solution
occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y) = x == y
occurs' x (Node l y r) = case compare x y of
                            LT -> occurs' x l
                            EQ -> True
                            GT -> occurs' x r

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
         (Node (Leaf 6) 7 (Leaf 9))
