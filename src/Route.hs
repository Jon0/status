module Route where

import System.IO
import Device
import File
import Html
import Template


data RouteData = RouteData { url :: String }


element :: Int -> [a] -> Maybe a
element index array
    | index < length array = Just $ array !! index
    | otherwise = Nothing


response_header :: String -> String
response_header content = ("HTTP/1.1 200 OK\nContent-Length: " ++ (show (length content)) ++ "\n\n")


getLocation :: String -> IO String
getLocation path = do
    content <- getPage path
    return (response_header content ++ content)


-- read kernel partition info
deviceTable :: IO String
deviceTable = do
    dir <- showDirectory "/"
    dev <- updateDevices
    mnt <- updateMounts
    return $ (toHtmlTable (toDeviceTable dev) ++ toHtmlTable (toMountTable mnt))


-- use the mount location of the filesystem
packageTable :: String -> IO String
packageTable path = do
    dat <- loadPackageData path "statfile"
    return $ toHtmlTable (toStorageTable dat)





-- use the mount location of the filesystem
deviceInfo :: String -> IO String
deviceInfo path = do
    dev <- updateDevices
    case (findDeviceName dev path) of
        Just d -> do
            return formStr
        Nothing -> do
            return "No such device"


-- match routes by regex from a file
readRoute :: String -> IO RouteData
readRoute url = do
    filedata <- fileContent "routes"
    return $ RouteData url


matchPattern :: String -> IO String
matchPattern s = case s of
    ('/':[]) -> deviceTable
    ('/':'p':'k':'g':'/':path) -> packageTable path
    ('/':'d':'e':'v':'/':path) -> deviceInfo path
    otherwise -> do return "Error"


getPage :: String -> IO String
getPage path = do
    name <- getHostname
    content <- matchPattern path
    return $ createPage name content


-- respond to HTTP get
getResponse :: Handle -> IO ()
getResponse hdl = do
    inpStr <- hGetLine hdl
    print $ words inpStr
    case element 1 (words inpStr) of
        Nothing -> do
            hPutStrLn hdl "HTTP/1.1 404 Not Found";
        Just a -> do
            response <- getLocation a
            hPutStrLn hdl response


postResponse :: Handle -> IO ()
postResponse hdl = do return ()


errorResponse :: Handle -> IO ()
errorResponse hdl = do return ()


replyFn :: Handle -> IO ()
replyFn hdl = getResponse hdl
