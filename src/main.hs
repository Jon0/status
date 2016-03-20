import System.Environment
import System.IO
import Config
import Device
import File
import Route
import Server



clearMountDir :: FilePath -> IO ()
clearMountDir dir = do
    dirs <- showDirectory (mountPointDir dir)
    removeAllEmptyDirectory (mountPointDir dir) dirs


main :: IO ()
main = do
    args <- getArgs
    let cfg = (argsToConfig args) in do
        clearMountDir (contentPath cfg)
        acceptLoop cfg replyFn


print_args :: IO ()
print_args = do
    args <- getArgs
    progName <- getProgName
    putStrLn ("The program name is: " ++ progName)
    putStrLn "The arguments are:"
    mapM putStrLn args
    return ()
