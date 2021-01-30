import Data.Char

-- Generic functions

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (x:xs) = do y <- f x
                    ys <- mapM' f xs
                    return (y:ys)

conv :: Char -> Maybe Int
conv x | isDigit x = Just (digitToInt x)
       | otherwise = Nothing

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' p []     = return []
filterM' p (x:xs) = do y  <- p x
                       ys <- filterM' p xs
                       return (if y then (x:ys) else ys)

join :: Monad m => m (m a) -> m a
join mmx = do mx <- mmx
              x <- mx
              return x
