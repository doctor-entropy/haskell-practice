module TicTacToe
(
    gametree
,   prune
,   empty
,   depth
,   Grid
,   Player (O, B, X)
,   Tree (Node)
) where

import Data.Char
import Data.List
import System.IO
import System.Random (randomRIO)

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

-- Exercises : Q3
toInt :: Maybe Int -> Int
toInt (Just n) = n
toInt Nothing = 0

mindepths :: Tree (Grid, Player) -> Int
mindepths (Node (_, _) []) = 0
mindepths (Node (_, _) ts) = 1 + minimum (map mindepths ts)

bestTree :: Player -> Tree (Grid, Player) -> Bool
bestTree best (Node (g, p) _) = p == best

fastestroute :: Grid -> Player -> Grid
fastestroute g p = bestgrids !! (toInt minDepthIdx)
                    where
                        bestgrids = [ g' | Node (g', p') _ <- besttrees ]
                        besttrees = filter (bestTree best) ts
                        tree = prune depth (gametree g p)
                        Node (_, best) ts = minimax tree
                        depths = map mindepths besttrees
                        minDepth = foldl1 min depths
                        minDepthIdx = elemIndex minDepth depths

-- Exercises : Q2
bestmoves :: Grid -> Player -> [Grid]
bestmoves g p = [ g' | Node (g', p') _ <- ts, p' == best ]
                where
                    tree = prune depth (gametree g p)
                    Node (_, best) ts = minimax tree

-- Exercises : Q4.1
getChoice :: IO Player
getChoice = do x <- getChar
               putChar '\n'
               if (toUpper x) == 'X' then
                   return X
               else if (toUpper x) == 'O' then
                   return O
               else
                   do putStrLn "ERROR: Invalid entry. Either choose 'O' or 'X'"
                      putStr "Human, please choose your symbol (O goes first): "
                      h <- getChoice
                      return h

-- Human vs computer
main :: IO ()
main = do hSetBuffering stdout NoBuffering
          -- Exercises : Q4.1
          putStr "Human, please choose your symbol (O goes first): "
          h <- getChoice
          playC empty O h

play :: Grid -> Player -> IO ()
play g p = do cls
              goto (1, 1)
              putGrid g
              play' g p

-- Exercises : Q4.1
playC :: Grid -> Player -> Player -> IO ()
playC g p h = do cls
                 goto (1, 1)
                 putGrid g
                 playChoice g p h

-- Exercises : Q2
rand :: [a] -> IO Int
rand xs = do r <- randomRIO (0, (length xs)-1)
             return r

-- Exercises : Q2
playR :: Grid -> Player -> IO ()
playR g p
    | wins O g = putStrLn "Player O wins!\n"
    | wins X g = putStrLn "Player X wins!\n"
    | full g   = putStrLn "It's a draw!\n"
    | p == O   = do i <- getNat (prompt p)
                    case move g i p of
                        [] -> do putStrLn "ERROR: Invalid move"
                                 playR g p
                        [g'] -> play g' (next p)
    | p == X   = do putStr "Player X is thinking... "
                    let bestmoves' = bestmoves g p
                    i <- rand bestmoves'
                    (play $! (bestmoves' !! i)) (next p)

-- Exercises : Q3
playFast :: Grid -> Player -> IO ()
playFast g p
    | wins O g = putStrLn "Player O wins!\n"
    | wins X g = putStrLn "Player X wins!\n"
    | full g   = putStrLn "It's a draw!\n"
    | p == O   = do i <- getNat (prompt p)
                    case move g i p of
                        [] -> do putStrLn "ERROR: Invalid move"
                                 playFast g p
                        [g'] -> play g' (next p)
    | p == X   = do putStr "Player X is thinking... "
                    (play $! (fastestroute g p)) (next p)

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

-- Exercises : Q4.1
playChoice :: Grid -> Player -> Player -> IO ()
playChoice g p h
    | wins O g = putStrLn "Player O wins!\n"
    | wins X g = putStrLn "Player X wins!\n"
    | full g   = putStrLn "It's a draw!\n"
    | p == h   = do i <- getNat (prompt p)
                    case move g i p of
                        [] -> do putStrLn "ERROR: Invalid move"
                                 playChoice g p h
                        [g'] -> playC g' (next p) h
    | otherwise = do putStr $ "Player " ++ show (next h) ++ " is thinking... "
                     (playC $! (bestmove g p)) (next p) h
