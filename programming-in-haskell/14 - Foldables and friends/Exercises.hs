import Data.Monoid

-- 1
instance (Monoid a, Monoid b) => Monoid (a, b) where
    -- mempty :: (a,b)
    mempty = (mempty, mempty)

    -- mappend :: (a, b) -> (a, b) -> (a, b)
    (a1, b1) `mappend` (a2, b2) = (a1 `mappend` a2, b1 `mappend` b2)

-- 2
instance Monoid b => Monoid (a -> b) where
    -- mempty :: (a -> b)
    mempty = (\_ -> mempty)

    -- mappend :: (a -> b) -> (a -> b) -> (a -> b)
    f `mappend` g = (\x -> f x `mappend` g x)

-- 3
instance Foldable Maybe where
    -- fold :: Monoid a => Maybe a -> a
    fold Nothing  = mempty
    fold (Just x) = x

    -- foldMap :: Monoid b => (a -> b) -> Maybe a -> Maybe b
    foldMap f Nothing  = Nothing
    foldMap f (Just x) = Just (f x)

    -- foldr :: (a -> b -> b) -> b -> Maybe a -> b
    foldr f v Nothing  = v
    foldr f v (Just x) = f x v

    -- foldl :: (a -> b -> a) -> a -> Maybe b -> a
    foldl f v Nothing  = v
    foldl f v (Just x) = f v x

instance Traversable Maybe where
    -- traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
    traverse f Nothing  = pure Nothing
    traverse f (Just x) = pure Just <*> f x

-- 4
data Tree a = Leaf | Node (Tree a) a (Tree a)

instance Foldable Tree where
    -- fold :: Monoid a => Tree a -> a
    fold Leaf = mempty
    fold (Node l a b) = a `mappend` (fold l) `mappend` (fold r)

    -- foldMap :: Monoid a => (a -> b) -> Tree a -> Tree b
    foldMap _ Leaf = Leaf
    foldMap f (Node l a r) = f a `mappend` (foldMap f l) `mappend` (foldMap f r)

    -- foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr f v Leaf = f mempty v
    foldr f v (Node l a r) = foldr f (foldr f v1 r) l
                                where v1 = f a v

    -- foldl :: (a -> b -> a) -> a -> Tree b -> a
    foldl f v Leaf = f mempty v
    foldl f v (Node l a r) = foldl f (foldl f v1 l) r
                                where v1 = f v a

instance Traversable Tree where
    -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse g Leaf = pure Leaf
    traverse g (Node l a r) = pure Node <*> traverse g l <*> g a <*> traverse g r

-- 5
filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF p = foldMap (\x -> if p x then [x] else [])
