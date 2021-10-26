class Monoid a where
    mempty  :: a
    mappend :: a -> a -> a

    mconcat :: [a] -> a
    mconcat = foldr mappend mempty

instance Monoid [a] where
    -- mempty :: [a]
    mempty = []

    -- mappend :: [a] -> [a] -> [a]
    mappend = (++)

instance Monoid a => Monoid (Maybe a) where
    -- mempty :: Maybe a
    mempty = Nothing

    -- mappend :: Maybe a -> Maybe a -> Maybe a
    Nothing `mappend` my      = my
    mx      `mappend` Nothing = mx
    Just x  `mappend` Just y  = Just (x `mappend` y)  

newtype Sum a = Sum a
                (deriving Show, Eq, Ord, Read)

getSum :: Sum a -> a
getSum (Sum x) = x

instance Num a => Monoid (Sum a) where
    -- mempty :: Sum a
    mempty = Sum 0

    -- mappend :: Sum a -> Sum a -> Sum a
    Sum x `mappend` Sum y = Sum (x + y)

newtype Product a = Product a
                        deriving (Show, Eq, Ord, Read)

getProduct :: Product a -> a
getProduct (Product x) = x

instance Num a => Monoid (Num a) where
    -- mempty :: Product a
    mempty = Product 1

    -- mappend :: Product a -> Product a -> Product a
    Product x `mappend` Product y = Product (x * y)


-- Foldables
class Foldable t where
    fold :: Monoid a => t a -> a
    foldMap :: Monoid a => (a -> b) -> t a -> t b
    foldr :: Monoid a => (a -> b -> b) -> b -> t a -> b
    foldl :: Monoid a => (a -> b -> a) -> a -> t b -> a

instance Foldable [] where
    -- fold :: Monoid a => t a -> a
    fold []     = mempty
    fold (x:xs) = x `mappend` fold xs

    -- foldMap :: Monoid a => (a -> b) -> t a -> t b
    foldMap g []     = mempty
    foldMap g (x:xs) = g x `mappend` foldMap g xs

    -- foldr :: Monoid a => (a -> b -> b) -> b -> t a -> b
    foldr f v []     = v
    foldr f v (x:xs) = f x (foldr f v xs)

    -- foldl :: Monoid a => (a -> b -> a) -> a -> t b -> a
    foldl f v []     = v
    foldl f v (x:xs) = foldl f (f v x) xs

instance Foldable Tree where
    -- fold :: Monoid a => Tree a -> a
    fold (Leaf x)   = x
    fold (Tree l r) = fold l `mappend` fold r

    -- foldMap :: Monoid a => (a -> b) -> Tree a -> b
    foldMap f (Leaf x)   = f x
    foldMap f (Tree l r) = (foldMap f l) `mappend` (foldMap f r)

    -- foldr :: Monoid a => (a -> b -> b) -> b -> t a -> b
    foldr f v (Leaf x)   = f x v
    foldr f v (Tree l r) = foldr f (foldr f v r) l

    -- foldl :: Monoid a => (a -> b -> a) -> a -> t b -> a
    foldl f v (Leaf x)   = f v x
    foldl f v (Tree l r) = foldl f (foldl f v l) r

-- Other primitives and defaults
null :: t a -> Bool
length :: t a -> Int
elem :: Eq a => a -> t a -> Bool
maximum :: Ord a => t a -> a
minimum :: Ord a => t a -> a
sum :: Num a => t a -> a
product :: Num a => t a -> a

toList :: t a -> [a]

-- Important relationships between fold, foldMap and toList
fold      = foldMap id
foldMap f = foldlr (mappend . f) mempty
toList    = foldMap (\x -> [x])
