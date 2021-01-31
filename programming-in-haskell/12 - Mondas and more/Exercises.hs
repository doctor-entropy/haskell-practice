{-# LANGUAGE InstanceSigs #-}

-- 1
data Tree a = Leaf | Node (Tree a) a (Tree a)
                    deriving Show

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap g Leaf = Leaf
  fmap g (Node l a r) = Node (fmap g l) (g a) (fmap g r)

tree :: Tree Int
tree = Node (Node (Leaf) 2 (Leaf)) 1 (Node (Leaf) 3 (Leaf))

-- 2
-- instance Functor ((->) a) where
--   fmap :: (b -> c) -> (a -> b) -> (a -> c)
--   fmap = (.)

-- 3
-- instance Functor ((-> a) a) => Applicative ((->) a) where
--   pure :: (a -> b) -> (a -> b)
--   pure = const

--   (<*>) :: (a -> b -> c) -> (a -> b) -> (b -> c)
--   g <*> h = \x -> g x (h x)

-- 4

