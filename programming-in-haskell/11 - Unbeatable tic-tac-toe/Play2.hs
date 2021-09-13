import Data.Char
import Data.List
import System.IO


-- Basic declarations

size :: Int
size = 3

type Grid = [[Player]]

data Player = B | X | O
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
chop _ [] = [[]]
chop n xs = take n xs : chop n (drop n xs)
