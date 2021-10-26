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
    