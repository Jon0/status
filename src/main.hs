import System.Environment
import System.IO
import Device
import File
import Route
import Server

print_args :: IO ()
print_args = do
    args <- getArgs
    progName <- getProgName
    putStrLn ("The program name is: " ++ progName)
    putStrLn "The arguments are:"
    mapM putStrLn args
    return ()


clearMountDir :: IO ()
clearMountDir = do
    dirs <- showDirectory mountPointDir
    removeAllEmptyDirectory mountPointDir dirs


main :: IO ()
main = do
    clearMountDir
    accept_loop replyFn

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

incState :: Int -> IO Int
incState i = do return i

mainLoop :: Int -> IO ()
mainLoop loop_state = do {
    new_loop_state <- incState loop_state;
    if finished new_loop_state
    then return ()
    else mainLoop new_loop_state
}

io_test = do
    putStrLn "Enter a string:"
    s <- get_string
    putStr (s ++ "\n")
    hGetChar stdin
    putStr "hello\n"
