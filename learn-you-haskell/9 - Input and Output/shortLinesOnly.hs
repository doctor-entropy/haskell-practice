-- import Data.Char

-- main = do
--     contents <- getContents
--     putStr $ shortLinesOnly contents

-- shortLinesOnly :: String -> String
-- shortLinesOnly input =
--     let allLines = lines input
--         shortlines = filter (\line -> length line < 10) allLines
--         result = unlines shortlines
--     in result

main = do  
    contents <- getContents  
    putStr (shortLinesOnly contents)  
  
shortLinesOnly :: String -> String  
shortLinesOnly input =   
    let allLines = lines input  
        shortLines = filter (\line -> length line < 10) allLines  
        result = unlines shortLines  
    in  result  
