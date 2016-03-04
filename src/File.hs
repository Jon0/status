module File where

import System.IO
import System.Directory
import Data.List

--tz contents = do
--    let list = lines contents
--        return $ zipWith () [0..] list


parse_file :: String -> IO String
parse_file filename = do
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    return contents


getHostname :: IO String
getHostname = parse_file "/etc/hostname"


showDirectory :: FilePath -> IO [String]
showDirectory path = do
    content <- getDirectoryContents path
    return $ content
