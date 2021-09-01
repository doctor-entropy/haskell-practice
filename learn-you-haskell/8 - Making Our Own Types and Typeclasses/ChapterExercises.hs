data Shape = Circle Point Float | Rectangle Point Point
             deriving Show

data Point = Point Float Float
                    deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * (r ^ 2)
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x1 - x2) * (abs $ y1 - y2)


infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)


data Tree a = EmptyTree | Node a (Tree a) (Tree a)
                deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton a = Node a (EmptyTree) (EmptyTree)

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)


treeElems :: (Ord a) => a -> Tree a -> Bool
treeElems x EmptyTree = False
treeElems x (Node a left right)
    | x == a = True
    | x < a = treeElems x left
    | x > a = treeElems x right
