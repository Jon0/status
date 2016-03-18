module File where

import Control.Exception
import Data.List
import System.Directory
import System.Process
import System.IO
import Util


-- command line actions
tryCommand :: String -> IO ()
tryCommand cmd = do
    putStrLn cmd
    ps <- runCommand cmd
    code <- waitForProcess ps
    putStrLn $ show code


listBlock :: [String] -> IO [[String]]
listBlock items = do
    result <- readProcess "lsblk" ["-r", ("-o" ++ (intercalate "," items))] ""
    return $ map allWords (lines result)


-- return hostname
getHostname :: IO String
getHostname = fileContent "/etc/hostname"


showDirectory :: FilePath -> IO [String]
showDirectory path = do
    content <- getDirectoryContents path
    return $ content


-- only remove empty directories
removeEmptyDirectory :: FilePath -> IO ()
removeEmptyDirectory path = do
    tryCommand ("rmdir " ++ path)


removeAllEmptyDirectory :: [FilePath] -> IO ()
removeAllEmptyDirectory [] = do
    return ()
removeAllEmptyDirectory (p:ps) = do
    removeEmptyDirectory p
    removeAllEmptyDirectory ps


fileErrorHandler :: IOException -> IO String
fileErrorHandler e = do
    print e
    return ""

-- does the file get closed?
fileContent :: FilePath -> IO String
fileContent filename =
    handle (fileErrorHandler) $ do
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    return contents


fileContent2 :: FilePath -> IO (Maybe String)
fileContent2 filename = do
    str <- bracket (openFile filename ReadMode)
            (hClose)
            (\hdl -> do contents <- hGetContents hdl; return contents)
    return $ Just str


mountDevice :: FilePath -> FilePath -> IO ()
mountDevice dev path = do
    tryCommand ("mkdir " ++ path)
    tryCommand ("mount " ++ dev ++ " " ++ path)


umountDevice :: FilePath -> IO ()
umountDevice path = do
    tryCommand ("umount " ++ path)
    removeEmptyDirectory path
