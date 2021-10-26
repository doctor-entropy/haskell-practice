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

