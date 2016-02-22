import System.Environment
import System.IO

main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    putStrLn ("The program name is: " ++ progName)
    putStrLn "The arguments are:"
    mapM putStrLn args
    putStrLn "Enter a string:"
    s <- get_string
    putStr (s ++ "\n")
    hGetChar stdin
    putStr "hello\n"

get_string :: IO String
get_string = do
    c <- getChar
    if c == '\n'
    then return ""
    else do
        l <- get_string
        return (c:l)

finished :: Int -> Bool
finished i = False

inc_state :: Int -> Int
inc_state i = i

main_loop :: [Int] -> [Int]
main_loop loop_state = do {
    some_state <- loop_state;
    --new_loop_state <- inc_state loop_state;
    --if finished new_loop_state
    --then return new_loop_state
    --else main_loop new_loop_state
    return some_state;
}
