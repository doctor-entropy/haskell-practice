-- 1

data Tree a = Leaf | Node (Tree a) a (Tree a)
                        deriving Show

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap g Leaf         = Leaf
    fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

-- 2

-- instance Functor ((->) a) where
    -- fmap :: (a -> b) -> (-> c) a -> (-> c) b
    -- fmap :: (a -> b) -> (c -> a) -> (c -> b)
    -- fmap = (.) -- Normal function composition

-- 3
-- instance Applicative ((->) a) where
--     pure x  = (\_ -> x)
--     f <*> g = (\x -> f x (g x))

-- 4
newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
    -- fmap :: (a -> b) -> ZipList a -> ZipList b
    fmap g (Z xs) = Z (fmap g xs)

instance Applicative ZipList where
    -- pure :: a -> ZipList a
    pure x = Z $ repeat x

    -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
    (Z gs) <*> (Z xs) = Z (gs <*> xs)

-- 6
-- instance Monad ((->) r) where
--     -- (>>=) :: ((->) r) a -> (a -> ((->) r) b) -> ((->) r) b
--     -- (>>=) :: (r -> a) -> (a -> r -> b) -> (r -> b)
--     f >>= mg = (\x -> let a = f x in (mg a x))

-- 7
data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
                        deriving Show

instance Functor Expr where
    -- fmap :: (a -> b) -> Expr a -> Expr b
    fmap g (Val x)     = Val x
    fmap g (Var x)     = Var (g x)
    fmap g (Add x1 x2) = Add (fmap g x1) (fmap g x2)

instance Applicative Expr where
    -- pure :: a -> Expr a
    pure = Var

    -- <*> :: Expr (a -> b) -> Expr a -> Expr b
    Val a <*> _             = Val a
    _     <*> Val a         = Val a
    (Var f) <*> (Var a)     = Var (f a)
    (Var f) <*> (Add e1 e2) = Add (fmap f e1) (fmap f e2)
    (Add f g) <*> e         = Add (f <*> e) (g <*> e)
    
instance Monad Expr where
    -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
    (Val n) >>= _ = Val n
    (Var x) >>= f = f x
    (Add x y) >>= f = Add (x >>= f) (y >>= f)

squareVar :: Char -> Expr String
squareVar c = Var ([c] ++ "^2")

-- Small example
-- $ t = Add (Var 'x') (Val 3)
-- $ t >>= squareVar
-- Add (Var "x^2") (Val 3)

-- 8

-- The state monad

type State = Int
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
    -- fmap :: (a -> b) -> ST a -> ST b
    fmap g st = do 
        x <- st
        return (g x)

instance Applicative ST where
    pure x = S (\s -> (x, s))

    stf <*> stx = do
        f <- stf
        x <- stx
        return (f x)

instance Monad ST where
    st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')

