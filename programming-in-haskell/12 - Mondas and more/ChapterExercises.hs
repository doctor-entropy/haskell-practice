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
