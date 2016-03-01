module File where

import System.IO
import System.Directory
import Data.List
import Html

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


showDirectory :: String -> IO String
showDirectory s = do return "..."

getMainPage :: IO String
getMainPage = do
    name <- getHostname
    return $ createPage name
