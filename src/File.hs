module File where

import Control.Exception
import Data.List
import System.Directory
import System.FilePath
import System.FilePath.Posix
import System.Process
import System.IO
import Util


-- mapping from url paths to mount paths
data DirectoryUrl = DirectoryUrl {
    duMount :: FilePath,
    duWebRoot :: FilePath,
    duDirectory :: FilePath
}


closeHandles :: [Handle] -> IO ()
closeHandles [] = do
    return ()
closeHandles (h:hs) = do
    hClose h
    closeHandles hs


noTrailingSlash :: FilePath -> FilePath
noTrailingSlash p =
    if last p == '/'
    then init p
    else p


webLocation :: DirectoryUrl -> FilePath
webLocation d = noTrailingSlash $ (duWebRoot d) ++ (duDirectory d)


fsLocation :: DirectoryUrl -> FilePath
fsLocation d = noTrailingSlash $ (duMount d) ++ (duDirectory d)


showDirectoryUrl :: DirectoryUrl -> String
showDirectoryUrl d = ("(" ++ (duMount d) ++ ", " ++ (duWebRoot d) ++ ", " ++ (duDirectory d) ++ ")")



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
    list <- mapM createFileStat (prefixSet (p ++ "/") allFiles)
    return list


fileMimeType :: FilePath -> Maybe String
fileMimeType path =
    let ext = takeExtension path in
        if ext == ".webm" then Just "video/webm"
        else if ext == ".mp4" then Just "video/mp4"
        else if ext == ".obv" then Just "video/ogg"
        else Just (path ++ " : " ++ ext)



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


-- recursivly get all non-directory files
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


fileErrorHandler :: IOException -> IO (String, [Handle])
fileErrorHandler e = do
    print e
    return ("", [])


contentHandle :: FilePath -> IO (String, [Handle])
contentHandle filename =
    handle (fileErrorHandler) $ do
    handle <- openFile filename ReadMode
    hSetBinaryMode handle True
    hSetBuffering handle NoBuffering
    contents <- hGetContents handle
    return (contents, [handle])


-- use contentHandle instead to close files
fileContent :: FilePath -> IO String
fileContent filename = do
    (c, h) <- contentHandle filename
    return c




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
