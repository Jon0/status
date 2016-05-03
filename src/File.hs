module File where

import qualified Data.ByteString.Lazy as BStr
import Control.Exception
import Crypto.Hash
import Data.List
import System.Directory
import System.Exit
import System.FilePath
import System.FilePath.Posix
import System.Posix
import System.Process
import System.IO
import Util


printError :: IOException -> IO ()
printError e = do
    print e


emptyError :: IOException -> IO [t]
emptyError e = do
    print e
    return []


nothingError :: IOException -> IO (Maybe t)
nothingError e = do
    print e
    return Nothing


contentSize :: FilePath -> IO (Maybe FileOffset)
contentSize filename =
    handle (nothingError) $ do
    stat <- getFileStatus filename
    return $ Just (fileSize stat)


-- replace with unix stat type
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


fullFsPath :: DirectoryUrl -> FileStat -> FilePath
fullFsPath d f = ((fsLocation d) ++ "/" ++ (fileName f))

fullWebPath :: DirectoryUrl -> FileStat -> FilePath
fullWebPath d f = ((webLocation d) ++ "/" ++ (fileName f))



extMimeMaybe :: String -> Maybe String
extMimeMaybe ext =
    if ext == ".txt" then Just "text/plain"
    else if ext == ".svg" then Just "image/svg+xml"
    else if ext == ".mp3" then Just "audio/mpeg"
    else if ext == ".webm" then Just "video/webm"
    else if ext == ".mp4" then Just "video/mp4"
    else if ext == ".obv" then Just "video/ogg"
    else Nothing


filePathMimeMaybe :: FilePath -> Maybe String
filePathMimeMaybe path = extMimeMaybe (takeExtension path)


showFileMime :: FilePath -> String
showFileMime path =
    let ext = takeExtension path in
        case extMimeMaybe ext of
            Nothing -> ext
            Just m -> m


readFileHash :: FilePath -> IO (Digest MD5)
readFileHash path = do
    fileContent <- BStr.readFile path
    let h = (hashlazy fileContent) in do
        putStrLn (path ++ ":\t" ++ (show h))
        return h


readFileSize :: FilePath -> IO Integer
readFileSize path = do
    size <- contentSize path
    case size of
        Just s -> do
            return $ toInteger s
        Nothing -> do
            return 0


fileSizeBase :: Integer -> Integer -> String
fileSizeBase b i =
    if i > 1024
    then fileSizeBase (b + 1) (div i 1024)
    else ((show i) ++ " " ++ ext) where
        ext = case b of
            0 -> "Bytes"
            1 -> "KB"
            2 -> "MB"
            3 -> "GB"
            4 -> "TB"
            otherwise -> "PB"

showFileSize :: Integer -> String
showFileSize s = fileSizeBase 0 s



listBlockChar :: String -> String
listBlockChar ('\\':'x':xs) = [(hexDigitToChar xs)]
listBlockChar _ = ""


-- replace symbols in url
listBlockReplace :: String -> String
listBlockReplace "" = ""
listBlockReplace url = let (a, b) = break (=='\\') url in
    a ++ (listBlockChar (take 4 b)) ++ (listBlockReplace (drop 4 b))


listBlock :: [String] -> IO [[String]]
listBlock items = do
    result <- readProcess "lsblk" ["-r", ("-o" ++ (intercalate "," items))] ""
    return $ map2D listBlockReplace (map allWords (lines result))


-- command line actions
tryCommand :: String -> IO ExitCode
tryCommand cmd = do
    putStrLn cmd
    ps <- runCommand cmd
    code <- waitForProcess ps
    putStrLn $ show code
    return code


-- return hostname
getHostname :: IO String
getHostname = readFile "/etc/hostname"


dotDirFilter :: String -> Bool
dotDirFilter [] = False
dotDirFilter (c:cs) = c /= '.'


showDirectory :: FilePath -> IO [FilePath]
showDirectory path =
    handle (emptyError) $ do
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
removeEmptyDirectory :: FilePath -> IO ExitCode
removeEmptyDirectory path = do
    e <- tryCommand ("rmdir " ++ path)
    return e


removeAllEmptyDirectory :: FilePath -> [FilePath] -> IO ()
removeAllEmptyDirectory _ [] = do
    return ()
removeAllEmptyDirectory base (p:ps) = do
    removeEmptyDirectory (base ++ "/" ++ p)
    removeAllEmptyDirectory base ps


-- no longer used
fileErrorHandler :: IOException -> IO (String, [Handle])
fileErrorHandler e = do
    print e
    return ("", [])

-- no longer used
contentHandle :: FilePath -> IO (String, [Handle])
contentHandle filename =
    handle (fileErrorHandler) $ do
    handle <- openFile filename ReadMode
    hSetBinaryMode handle True
    hSetBuffering handle NoBuffering
    contents <- hGetContents handle
    return (contents, [handle])


-- no longer used
fileContent :: FilePath -> IO String
fileContent filename = do
    (c, h) <- contentHandle filename
    return c


mountDevice :: FilePath -> FilePath -> Bool -> IO ExitCode
mountDevice dev path writable = do
    e1 <- tryCommand ("mkdir " ++ path)
    case e1 of
        ExitSuccess -> do
            if writable
            then do
                e2 <- tryCommand ("mount -o rw " ++ dev ++ " " ++ path)
                return e2
            else do
                e2 <- tryCommand ("mount -o ro " ++ dev ++ " " ++ path)
                return e2
        ExitFailure a -> do
            return e1


umountDevice :: FilePath -> IO ExitCode
umountDevice path = do
    e <- tryCommand ("umount " ++ path)
    case e of
        ExitSuccess -> do
            removeEmptyDirectory path
            return e
        ExitFailure a -> do
            return e


-- path to content files
defaultStaticPrefix :: FilePath -> FilePath -> String
defaultStaticPrefix base path = (base ++ "/static/" ++ path)
