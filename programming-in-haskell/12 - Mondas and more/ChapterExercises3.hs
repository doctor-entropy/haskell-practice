-- Monads practice
data Expr = Val Int | Div Expr Expr

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

eval :: Expr -> Maybe Int
eval (Val n)   = Just n
eval (Div x y) = case eval x of
                    Nothing -> Nothing
                    Just m -> case eval y of
                                Nothing -> Nothing
                                Just n -> safediv m n

-- With bind operator
eval' :: Expr -> Maybe Int
eval' (Val n)   = Just n
eval' (Div x y) = eval x >>= \n ->
                  eval y >>= \m ->
                    safediv n m

-- With do notation
eval'' :: Expr -> Maybe Int
eval'' (Val n)   = Just n
eval'' (Div x y) = do n <- eval x
                      m <- eval y
                      safediv n m

pairs :: [a] -> [b] -> [(a, b)]
pairs xs ys = do x <- xs
                 y <- ys
                 return (x, y)


-- The state monad

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap f st = S (\s -> let (x, s') = app st s in (f x, s'))

instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (\s -> (x, s))

  -- <*> :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = S (\s ->
      let (f, s')  = app stf s 
          (x, s'') = app stx s' in  (f x, s''))

instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = (\s -> )
