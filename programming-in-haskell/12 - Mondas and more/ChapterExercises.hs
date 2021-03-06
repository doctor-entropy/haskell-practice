-- Practice
-- Loading this will not work as this clashes with the Prelude definitions

-- ********
-- FUNCTORS
-- ********
class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor [] where
    fmap :: (a -> b) -> [a] -> [b]
    fmap = map

instance Functor Maybe where
    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap _ Nothing = Nothing
    fmap g (Just x) = Just (g x)

data Tree a = Leaf a | Node (Tree a) (Tree a)
                      deriving Show

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap g (Leaf x) = Leaf (g x)
    fmap g (Node l r) = Node (g l) (g r)

instance Functor IO where
    fmap :: (a -> b) -> IO a -> IO b
    fmap g mx = do {x <- mx; return (g x)}

-- Functor laws
fmap id = id
fmap (g . h) = fmap g . fmap h

-- ************
-- APPLICATIVES
-- ************
class Functor f => Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Maybe where
    pure  :: a -> Maybe a
    pure = Just

    (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    Nothing <*> _ = Nothing
    (Just g) <*> mx = fmap g mx

instance Applicative [] where
    pure :: a -> [a]
    pure x = [x]

    (<*>) :: [a -> b] -> [a] -> [b]
    gs <*> xs = [g x | g <- gs, x <- xs]

instance Applicative IO where
    pure :: a -> IO a
    pure = return

    (<*>) :: IO (a -> b) -> IO a -> IO b
    mg <*> xs = do {g <- gs; x <- xs; return (g x)}

-- ******
-- MONADS
-- ******

(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
mx >>= f = case mx of
              Nothing -> Nothing
              Just x -> f x

class Applicative m = Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

  return = pure

instance Monad Maybe where
  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Nothing >>= _ = Nothing
  (Just x) >>= f = f x

instance Monad [] where
  (>>=) :: [a] -> (a -> [b]) -> [b]
  xs >>= f = [ y | x <- xs; y <- f x]

-- instance Monad IO where
--   (>>=) :: IO a -> (a -> IO b) -> IO b
--   mx >>= f = do {x <- mx; return f x}

-- ***********
-- STATE MONAD
-- ***********

newtype ST s = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
  fmap :: (a -> b) -> ST a -> ST b
  fmap g st = S (\s -> let (x, s') = app st s in (g x, s'))

instance Applicative ST where
  pure :: a -> ST a
  pure x = S (\s -> (x, s))

  (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = S (\s ->
          let (f, s') = app stf s
              (x, s'') = app stx s' in (f x, s''))

instance Monad ST where
  (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s -> let (x, s') = app st s in app (f x) s')

-- Relabelling trees

data Tree a = Leaf a | Node (Tree a) (Tree a)
                deriving Show

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n   = (Leaf n, n+1)
rlabel (Node l r) n = (Node l' r', n'')
                      where
                        (l', n') = rlabel l n
                        (r', n'') = rlabel r n'

fresh :: ST Int
fresh = S (\n -> (n, n + 1))

alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _) = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r

mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _)   = do n <- fresh
                       return (Leaf n)
mlabel (Node l r) = do l' <- mlabel l
                       r' <- mlabel r
                       return (Node l' r')