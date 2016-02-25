module File where

import System.IO
import System.Directory
import Data.List

read_file = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn "These are your TO-DO items:"
    putStr $ unlines numberedTasks
    putStrLn "Select a line:"
    numberString <- getLine
    let number = read numberString
    putStr numberString
    hClose handle
    hClose tempHandle

parse_file :: String -> String
parse_file filename = do
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    return contents
