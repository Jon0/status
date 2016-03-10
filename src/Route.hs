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


getAllPackages :: [Mount] -> IO [Package]
getAllPackages [] = do
    return []
getAllPackages (m:mnts) = do
    dat <- loadPackageData (mntPath m) "statfile"
    rpks <- getAllPackages mnts
    return $ (pkgData dat) ++ rpks


-- open statfile in each device
showAllPackages :: [Mount] -> IO String
showAllPackages mnts = do
    pkg <- getAllPackages mnts
    return $ toHtmlTable (storageToHtml pkg)


-- filter a particular name
showPackage :: [Mount] -> String -> IO String
showPackage mnts name = do
    pkg <- getAllPackages mnts
    return $ toHtmlTable (storageToHtml pkg)


-- use all known devices and find by name
packageTable :: String -> IO String
packageTable name = do
    mnts <- mountsByPath "/srv/storage"
    if (length name) == 0
    then do
        html <- showAllPackages (deviceMounts mnts)
        return html
    else do
        html <- showPackage (deviceMounts mnts) name
        return html

-- use the mount location of the filesystem
mountPackageTable :: FilePath -> IO String
mountPackageTable path = do
    dat <- loadPackageData path "statfile"
    return $ toHtmlTable (toStorageTable dat)


-- mounts and umounts devices
queryAction :: Device -> String -> IO ()
queryAction dev query =
    if (length query) > 0
    then
        if (last query) == '1'
        then do
            mountDevice dev ("/srv/storage/" ++ (strId dev))
        else do
            umountDevice ("/srv/storage/" ++ (strId dev))
    else do
        return ()


-- use the mount location of the filesystem
deviceInfo :: String -> String -> IO String
deviceInfo path query = do
    dev <- updateDevices
    case (findDeviceName dev path) of
        Nothing -> do
            return "No such device"
        Just d -> do
            queryAction d query
            mnt <- updateMounts
            let mnt_name = ("/dev/" ++ strId d) in
                case (findMountName mnt mnt_name) of
                    Just m -> do
                        pkgs <- mountPackageTable (mntPath m)
                        return ("<h3>" ++ mnt_name ++ " (" ++ (mntPath m) ++ ")</h3>" ++ formStr ++ pkgs)
                    Nothing -> do
                        return ("<h3>" ++ mnt_name ++ " (U)</h3>" ++ formStr)


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


responseHeader :: String -> String
responseHeader content = ("HTTP/1.1 200 OK\nContent-Length: " ++ (show (length content)) ++ "\n\n")


getLocation :: String -> String -> IO String
getLocation path query = do
    content <- getPage path query
    return (responseHeader content ++ content)


httpLine :: [String] -> IO String
httpLine (verb:path:version:[]) =
    let (file, query) = break (=='?') path in do
        response <- getLocation file query
        return response
httpLine _ = do
    return "HTTP/1.1 404 Not Found\n\n"


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
