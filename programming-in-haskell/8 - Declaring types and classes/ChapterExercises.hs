data Move = North | South | East | West

type Pos = (Int, Int)

move :: Move -> Pos -> Pos
move North (x, y) = (x, y + 1)
move South (x, y) = (x, y - 1)
move East (x, y) = (x + 1, y)
move West (x, y) = (x - 1, y)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m:ms) p = moves ms (move m p)

-- ///////////////////////////////////////

data Shape = Circle Float | Rect Float Float
                deriving Show

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * (r ^ 2)
area (Rect x y) = x * y

-- ////////////////////////////////////////

-- data Maybe a = Nothing | Just a

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just(m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just(head xs)
