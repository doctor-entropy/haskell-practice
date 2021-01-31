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

-- Trying again (Qs. 2 and 3)
-- instance Functor ((->) a) where
--   fmap :: (b -> c) -> (a -> b) -> (a -> c)
--   fmap = (.)

-- instance Functor ((-> a) a) => Applicative ((-> a) a) where
--   pure :: (a -> b) -> (a -> b)
--   pure = const

--   (<*>) :: (a -> b -> c) -> (a -> b) -> (b -> c)
--   f <*> g = (\x -> f x (g x))

-- 4
newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
  fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap g (Z xs) = Z (fmap g xs)

instance Applicative ZipList where
  pure :: a -> ZipList a
  pure x = Z (repeat x)

  (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  (Z gs) <*> (Z xs) = Z [ g x | (g, x) <- zip gs xs]

-- 6
-- instance Applicative ((-> a) a) => Monad ((-> a) a) where
--   (>>=) :: (a -> b) -> ((a -> b) -> ( b-> c)) -> (b -> c)
--   f >>= g = g . f
