import System.IO
-- 1
putStr' :: String -> IO ()
putStr' xs = sequence_ [ putChar x | x <- xs]

-- 2 && 3
putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

type Board = [Int]
putBoard :: Board -> IO ()
putBoard ns = sequence_ [ putRow r n | (r, n) <- zip [1..] ns]

-- 4

adder' :: Int -> Int -> IO Int
adder' sum n = 
            do if n == 0 then
                return sum
               else
                do xs <- getLine
                   let i = read xs :: Int
                   adder' (sum + i) (n - 1)                     

adder :: IO ()
adder = do putStr "How many numbers? "
           xs <- getLine
           let n = read xs :: Int
           total <- adder' 0 n
           do putStr "The total is "
              putStrLn (show total)

-- 5
adder2' :: IO Int
adder2' = do xs <- getLine
             return (read xs :: Int)

adder2 :: IO ()
adder2 = do putStr "How many numbers? "
            xs <- getLine
            let n = read xs :: Int
            ns <- sequence [ adder2' | _ <- [1..n]]
            putStr (show (sum ns))

-- 6
-- Got partial answers from "https://github.com/wololock/programming-in-haskell-2nd-edition/blob/master/ch10_00_sandbox.hs"
getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin False
           return x

newline :: IO ()
newline = putChar '\n'

erase :: IO ()
erase = do putChar '\b'
           putChar ' '
           putChar '\b'

readline' :: IO String
readline' = do x <- getCh
               if x == '\n' then
                  do newline
                     return []
               else if x == '\DEL' then
                  do erase
                     xs <- readline'
                     return (x:xs)
               else
                  do putChar x
                     xs <- readline'
                     return (x:xs)

readline :: IO String
readline = do xs <- readline'
              return (removeErased xs)

removeErased :: String -> String 
removeErased = foldl (\xs x -> if x == '\DEL' then init xs else xs ++ [x]) ""

