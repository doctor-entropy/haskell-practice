import System.Environment
import System.Directory
import System.IO
import Data.List

type Action = [String] -> IO ()

dispatch :: [(String, Action)]
dispatch = [
        ("add", add),
        ("view", view),
        ("remove", remove)
    ]

activate :: Maybe Action -> [String] -> String -> IO ()
activate (Just action) args _ = action args
activate (Nothing) _ command = putStrLn $ "Invalid command " ++ command

main = do
    (command:args) <- getArgs
    let result = lookup command dispatch
    activate result args command

add :: Action
add [filePath, todo] = appendFile filePath (todo ++ "\n")

view :: Action
view [filePath] = do
    contents <- readFile filePath
    let todos = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [1..] (lines contents)
    putStr $ unlines numberedTasks

remove :: Action
remove [filePath, index] = do
    contents <- readFile filePath
    (tempName, tempHandle) <- openTempFile "." "temp"
    let number = (read index) - 1
        todoTasks = lines contents
        updatedTodoTasks = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines updatedTodoTasks
    hClose tempHandle  
    removeFile filePath  
    renameFile tempName filePath
