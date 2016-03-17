module Route where

import System.IO
import Device
import Document
import File
import Html
import Http
import Package
import Template



debugPageHandler :: HttpRequest -> IO HttpResponse
debugPageHandler request = do
    html <- pageWithHostName (showRequest request)
    return $ generalResponse html


-- a data file containing route information
data RouteData = RouteData { routes :: [String] }


data MainPage = MainPage { mainItems :: [String] }

instance RouteType MainPage where
    routeName main = "/"
    routeKey main = []
    routeMap main = RouteNode mainPageMap
    updateType strs = do
        return $ MainPage []


mainPageHtml :: IO String
mainPageHtml = do
    dir <- showDirectory "/"
    dev <- updatePartitions
    mnt <- updateMounts
    html <- pageWithHostName (toHtmlTable (toPartitionTable dev) ++ toHtmlTable (toMountTable mnt))
    return html


mainPageHandler :: HttpRequest -> IO HttpResponse
mainPageHandler request = do
    html <- mainPageHtml
    return $ generalResponse html


mainPageMap :: String -> Maybe RouteItem
mainPageMap path
    | path == "" = Just $ RouteLeaf mainPageHandler
    | path == "dev" = Just $ RouteNode devicePageMap
    | path == "pkg" = Just $ RouteNode packagePageMap
    | otherwise = Just $ RouteLeaf debugPageHandler


-- pages for connected devices
data DevicePage = DevicePage { deviceItems :: [Device] }

instance RouteType DevicePage where
    routeName dev = "dev"
    routeKey dev = []
    routeMap dev = RouteNode devicePageMap
    updateType strs = do
        return $ DevicePage []


-- mounts and umounts devices
queryAction :: Partition -> String -> IO ()
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
partitionPackageTable :: FilePath -> IO String
partitionPackageTable path = do
    dat <- loadPackageData path "statfile"
    return $ toHtmlTable (toStorageTable dat)


partitionInfoPage :: Partition -> IO String
partitionInfoPage p = do
    mnt <- updateMounts
    let mnt_name = ("/dev/" ++ strId p) in
        case (findMountName mnt mnt_name) of
            Just m -> do
                pkgs <- partitionPackageTable (mntPath m)
                return ("<h3>" ++ mnt_name ++ " (" ++ (mntPath m) ++ ")</h3>" ++ formStr ++ pkgs)
            Nothing -> do
                return ("<h3>" ++ mnt_name ++ " (U)</h3>" ++ formStr)


partitionUrlToName :: String -> String
partitionUrlToName url =
    let strs = urlSplitString url in
        if length strs < 2
        then ""
        else strs !! 1


-- use the mount location of the filesystem
devicePageBody :: String -> String -> IO String
devicePageBody path query = do
    dev <- updatePartitions
    case (findPartitionName dev path) of
        Nothing -> do
            blks <- listBlock ["kname", "pkname", "maj:min", "fstype", "size", "mountpoint", "label", "uuid", "state", "model", "serial", "vendor"]
            return $ toHtmlTable (linesToHtml blks)
        Just p -> do
            queryAction p query
            html <- partitionInfoPage p
            return html


devicePageHandler :: HttpRequest -> IO HttpResponse
devicePageHandler request = do
    body <- devicePageBody (partitionUrlToName (urlString request)) (query request)
    html <- pageWithHostName body
    return $ generalResponse html


-- does not know which devices exist
devicePageMap :: String -> Maybe RouteItem
devicePageMap path = Just $ RouteLeaf devicePageHandler


-- show available packages
data PackagePage = PackagePage { packageItems :: [String] }


-- open statfile in each device
showAllPackages :: [Package] -> String
showAllPackages pkg = toHtmlTable (storageToHtml pkg)


-- filter a particular name
showPackage :: [Package] -> String -> String
showPackage pkg name = toHtmlTable (storageToHtml pkg)


-- use all known devices and find by name
packageTable :: String -> IO String
packageTable name = do
    mnts <- mountsByPath "/srv/storage"
    pkgs <- getAllPackages (deviceMounts mnts)
    if (length name) == 0
    then do
        return $ showAllPackages pkgs
    else do
        return $ showPackage pkgs name


packagePageHandler :: HttpRequest -> IO HttpResponse
packagePageHandler request = do
    body <- packageTable (urlString request)
    html <- pageWithHostName body
    return $ generalResponse html


-- does not know which packages exist
packagePageMap :: String -> Maybe RouteItem
packagePageMap path = Just $ RouteLeaf packagePageHandler





-- read and reply to a request, given a socket handle
httpGetHandler :: RouteItem -> Handle -> IO ()
httpGetHandler routes hdl = do
    inpStr <- hGetLine hdl
    print $ words inpStr
    case (firstHeaderLine (words inpStr)) of
        Nothing -> do
            hPutStrLn hdl ((makeResponseLine 400) ++ "\n\n")
        Just r -> case requestMatch routes r of
            Nothing -> do
                hPutStrLn hdl ((makeResponseLine 404) ++ "\n\n")
            Just out -> do
                response <- out
                hPutStrLn hdl (responseString response)


postResponse :: Handle -> IO ()
postResponse hdl = do return ()


errorResponse :: Handle -> IO ()
errorResponse hdl = do return ()


routeBase :: MainPage
routeBase = MainPage []


replyFn :: Handle -> IO ()
replyFn hdl = httpGetHandler (routeMap routeBase) hdl


-- match routes by regex from a file
readRoute :: String -> IO RouteData
readRoute url = do
    filedata <- fileContent "routes"
    return $ RouteData [url]
