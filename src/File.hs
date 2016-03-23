module File where

import Control.Exception
import Data.List
import System.Directory
import System.FilePath
import System.Process
import System.IO
import Util


data FileStat = FileStat {
    fileLocation :: FilePath,
    fileName :: String,
    isDirectory :: Bool
}


createFileStat :: FilePath -> IO FileStat
createFileStat path = do
    isDir <- doesDirectoryExist path
    return $ FileStat loc name isDir where
        loc = takeDirectory path
        name = takeFileName path


directoryContent :: FilePath -> IO [FileStat]
directoryContent p = do
    allFiles <- showDirectory p
    list <- mapM createFileStat allFiles
    return list


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


dotDirFilter :: String -> Bool
dotDirFilter [] = False
dotDirFilter (c:cs) = c /= '.'


dirErrorHandler :: IOException -> IO [String]
dirErrorHandler e = do
    print e
    return []

showDirectory :: FilePath -> IO [FilePath]
showDirectory path =
    handle (dirErrorHandler) $ do
    content <- getDirectoryContents path
    return $ filter dotDirFilter content


absolutePath :: [String] -> FilePath
absolutePath strs = ("/" ++ (intercalate "/" strs))


-- recursivly get all non-directory files
allFileContents :: FilePath -> [FilePath] -> IO [FilePath]
allFileContents _ [] = do
    return []
allFileContents prefix (p:ps) = do
    isDir <- doesDirectoryExist (prefix ++ "/" ++ p)
    otherDirs <- allFileContents prefix ps
    if isDir
    then do
        ct <- showDirectory (prefix ++ "/" ++ p)
        dirCont <- allFileContents prefix (prefixSet (p ++ "/") ct)
        return (dirCont ++ otherDirs)
    else do
        return (p : otherDirs)


allSubFiles :: FilePath -> IO [FilePath]
allSubFiles path = do
    isDir <- doesDirectoryExist path
    if isDir
    then do
        ct <- showDirectory path
        dirCont <- allFileContents path ct
        return dirCont
    else do
        return []


-- only remove empty directories
removeEmptyDirectory :: FilePath -> IO ()
removeEmptyDirectory path = do
    tryCommand ("rmdir " ++ path)


removeAllEmptyDirectory :: FilePath -> [FilePath] -> IO ()
removeAllEmptyDirectory _ [] = do
    return ()
removeAllEmptyDirectory base (p:ps) = do
    removeEmptyDirectory (base ++ "/" ++ p)
    removeAllEmptyDirectory base ps


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


-- path to content files
defaultPrefix :: String -> String
defaultPrefix path = ("/srv/static/" ++ path)
