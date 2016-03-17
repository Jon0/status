module File where


import Control.Exception
import System.IO
import System.Directory


-- return hostname
getHostname :: IO String
getHostname = fileContent "/etc/hostname"


showDirectory :: FilePath -> IO [String]
showDirectory path = do
    content <- getDirectoryContents path
    return $ content


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
