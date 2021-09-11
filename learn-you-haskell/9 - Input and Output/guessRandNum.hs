import System.Random
import Control.Monad (when)

main = do
    gen <- getStdGen
    askForNumber gen


askForNumber :: StdGen -> IO ()
askForNumber gen = do
    let (randNum, newGen) = randomR (1, 10) gen :: (Int, StdGen)
    putStr "Which number in the range of 1 to 10 am I thinking off? "
    numberString <- getLine
    when (not $ null numberString) $ do
        let userNum = read numberString
        if userNum == randNum then
            putStrLn "You are correct!"
        else
            putStrLn $ "I was thinking off " ++ show randNum
        askForNumber newGen
