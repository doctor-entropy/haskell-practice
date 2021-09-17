import Data.Char
import Data.List
import System.IO


-- Basic declarations

size :: Int
size = 3

type Grid = [[Player]]

data Player = O | B | X
                deriving (Show, Eq, Ord)

next :: Player -> Player
next B = B  -- Just for completeness, to avoid partial functions
next X = O
next O = X

-- Grid Utilities

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
            where os = length (filter (== O) $ concat g)
                  xs = length (filter (== X) $ concat g)

wins :: Player -> Grid -> Bool
wins p g = any lines (rows ++ cols ++ dias)
            where lines = all (== p)
                  rows = g
                  cols = transpose g
                  dias = diags g

diags :: Grid -> [[Player]]
diags g = [[ g !! n !! n | n <- nth ],
          [ g !! (n' - n) !! n | n <- nth ]]
            where nth = [0..n']
                  n' = size - 1

won :: Grid -> Bool
won g = wins O g || wins X g

-- Displaying Grid

putGrid :: Grid -> IO ()
putGrid =
    putStrLn . unlines . concat . interleave bar . map showRow
        where bar = [replicate ((size*4) - 1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
            where
                beside = foldr1 (zipWith (++))
                bar = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer X = ["   ", " X ", "   "]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]


interleave :: a -> [a] -> [a]
interleave y [] = []
interleave y [x] = [x]
interleave y (x:xs) = x : y : interleave y xs

-- Making a move

valid :: Grid -> Int -> Bool
valid g i = i >=0 && i < size^2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p = 
    if valid g i then [ chop size (xs ++ [p] ++ ys) ] else []
        where (xs, B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

-- Reading a number

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   l <- getLine
                   if l /= [] && all isDigit l then
                       return (read l)
                   else
                       do putStrLn "ERROR: Invalid entry. Please enter a number between 0 - 9."
                          getNat prompt

tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do cls
             goto (1,1)
             putGrid g
             run' g p

run' :: Grid -> Player -> IO ()
run' g p | wins O g = putStrLn "Player O wins.\n"
         | wins X g = putStrLn "Player X wins.\n"
         | full g   = putStrLn "It's a draw.\n"
         | otherwise = 
             do i <- getNat (prompt p)
                case move g i p of
                    [] -> do putStrLn "ERROR: Invalid move."
                             run' g p
                    [g'] -> run g' (next p)                      

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

cls :: IO ()
cls = putStr "\ESC[2J"

-- Game of trees
data Tree a = Node a [Tree a]
                deriving (Show)

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p 
      | won g = []
      | full g = []
      | otherwise = concat [ move g i p | i <- [0..(size^2 - 1)] ]

-- Pruning the tree
prune :: Int -> Tree Grid -> Tree Grid
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int
depth = 9

-- Minimax algorithm
minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
        | wins O g  = Node (g, O) []
        | wins X g  = Node (g, X) []
        | otherwise = Node (g, B) []
minimax (Node g ts)
        | turn g == O = Node (g, minimum ps) ts'
        | turn g == X = Node (g, maximum ps) ts'
                        where
                            ts' = map minimax ts
                            ps = [ p | Node (_, p) _ <- ts' ]

bestmove :: Grid -> Player -> Grid
bestmove g p = head [ g' | Node (g', p') _ <- ts, p' == best ]
                where
                    tree = prune depth (gametree g p)
                    Node (_, best) ts = minimax tree

-- Human vs computer
main :: IO ()
main = do hSetBuffering stdout NoBuffering
          play empty O

play :: Grid -> Player -> IO ()
play g p = do cls
              goto (1, 1)
              putGrid g
              play' g p

play' :: Grid -> Player -> IO ()
play' g p
    | wins O g = putStrLn "Player O wins!\n"
    | wins X g = putStrLn "Player X wins!\n"
    | full g   = putStrLn "It's a draw!\n"
    | p == O   = do i <- getNat (prompt p)
                    case move g i p of
                        [] -> do putStrLn "ERROR: Invalid move"
                                 play' g p
                        [g'] -> play g' (next p)
    | p == X   = do putStr "Player X is thinking... "
                    (play $! (bestmove g p)) (next p)
