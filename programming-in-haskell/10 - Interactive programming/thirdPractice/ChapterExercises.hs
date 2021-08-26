-- Derived Primitives

getLine' :: IO String
getLine' = do x <- getChar
              if x == '\n' then
                 return []
              else
                 do xs <- getLine'
                    return (x:xs)

putStr' :: String -> IO ()
putStr' []     = return ()
putStr' (x:xs) = do putChar x
                    putStr' xs

putStrLn' :: String -> IO ()
putStrLn' xs = do putStr' xs
                  putChar '\n'

strlen :: IO ()
strlen = do putStr' "Enter a string: "
            xs <- getLine'
            let n = length xs
            putStr' "The length of the string entered is "
            putStr' (show n)
            putStrLn' " characters."

strlenTwo :: String -> IO ()
strlenTwo xs = do let n = length xs
                  putStr' "The length of the string entered is "
                  putStr' (show n)
                  putStrLn' " characters."
