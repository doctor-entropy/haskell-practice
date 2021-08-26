import Data.Char

next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

update :: Int -> Int -> Int
update n num = n - num

move :: Board -> Int -> Int -> Board
move board row num = [ if r == row then update n num else n | (r, n) <- zip [1..] board]


-- IO Utilities

putRow :: Int -> Int -> IO ()
putRow r n = do putStr (show r)
                putStr ": "
                putStrLn (concat (replicate n "* "))

putBoard :: Board -> IO ()
putBoard [n1, n2, n3, n4, n5] = do putRow 1 n1
                                   putRow 2 n2
                                   putRow 3 n3
                                   putRow 4 n4
                                   putRow 5 n5

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline
                     if isDigit x then
                         return (digitToInt x)
                     else
                         do putStrLn "ERROR: Invalid digit"
                            getDigit prompt

newline :: IO ()
newline = putChar '\n'


-- Game of Nim

play :: Board -> Int -> IO ()
play board player = 
    do newline
       putBoard board
       if finished board then
           do newline
              putStr "Player "
              putStr (show (next player))
              putStrLn " wins!!"
        else
            do putStr "Player "
               putStrLn (show player)
               row <- getDigit "Enter a row: "
               num <- getDigit "Enter the number of stars to remove: "
               if valid board row num then
                   play (move board row num) (next player)
               else
                   do newline
                      putStrLn "ERROR: Invalid move"
                      play board player

nim :: IO ()
nim = play initial 1