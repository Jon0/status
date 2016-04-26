import System.Environment
import System.IO
import Any
import Config
import Device
import File
import Route
import Server


-- removes empty directories left behind
clearMountDir :: FilePath -> IO ()
clearMountDir dir = do
    dirs <- showDirectory (mountPointDir dir)
    removeAllEmptyDirectory (mountPointDir dir) dirs


startServer :: Config -> IO ()
startServer cfg = do
    clearMountDir (contentPath cfg)
    acceptLoop cfg (replyFn cfg)


-- generate default database
createDatabase :: Config -> IO ()
createDatabase cfg = do
    return ()


main :: IO ()
main = do
    args <- getArgs
    let cfg = (argsToConfig args) in do
        startServer cfg


print_args :: IO ()
print_args = do
    args <- getArgs
    progName <- getProgName
    putStrLn ("The program name is: " ++ progName)
    putStrLn "The arguments are:"
    mapM putStrLn args
    return ()
