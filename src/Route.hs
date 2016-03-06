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


-- use the mount location of the filesystem
packageTable :: String -> IO String
packageTable path = do
    dat <- loadPackageData path "statfile"
    return $ toHtmlTable (toStorageTable dat)


-- use the mount location of the filesystem
deviceInfo :: String -> String -> IO String
deviceInfo path query = do
    dev <- updateDevices
    case (findDeviceName dev path) of
        Just d -> do
            if (length query) > 0
            then do
                mountDevice d "/srv/storage"
            else do
                putStrLn $ ""
            return ("<h3>" ++ (strId d) ++ "</h3>" ++ formStr)
        Nothing -> do
            return "No such device"


-- match routes by regex from a file
readRoute :: String -> IO RouteData
readRoute url = do
    filedata <- fileContent "routes"
    return $ RouteData url


-- read kernel partition info
deviceTable :: IO String
deviceTable = do
    dir <- showDirectory "/"
    dev <- updateDevices
    mnt <- updateMounts
    return $ (toHtmlTable (toDeviceTable dev) ++ toHtmlTable (toMountTable mnt))

matchPattern :: String -> String -> IO String
matchPattern str query = case str of
    ('/':[]) -> deviceTable
    ('/':'p':'k':'g':'/':path) -> packageTable path
    ('/':'d':'e':'v':'/':path) -> deviceInfo path query
    otherwise -> do return "Error"


-- forms the html page content
getPage :: String -> String -> IO String
getPage path query = do
    name <- getHostname
    content <- matchPattern path query
    return $ createPage name content


response_header :: String -> String
response_header content = ("HTTP/1.1 200 OK\nContent-Length: " ++ (show (length content)) ++ "\n\n")


getLocation :: String -> String -> IO String
getLocation path query = do
    content <- getPage path query
    return (response_header content ++ content)


httpLine :: [String] -> IO String
httpLine (verb:path:version:[]) =
    let (file, query) = break (=='?') path in do
        response <- getLocation file query
        return response
httpLine _ = do
    return "HTTP/1.1 404 Not Found"


-- respond to HTTP get
getResponse :: Handle -> IO ()
getResponse hdl = do
    inpStr <- hGetLine hdl
    response <- httpLine (words inpStr)
    print $ words inpStr
    hPutStrLn hdl response


postResponse :: Handle -> IO ()
postResponse hdl = do return ()


errorResponse :: Handle -> IO ()
errorResponse hdl = do return ()


replyFn :: Handle -> IO ()
replyFn hdl = getResponse hdl
