module Route where

import System.IO
import Device
import Document
import File
import Html
import Http
import List
import Package
import System
import Template
import Util


debugPageHandler :: HttpRequest -> IO HttpResponse
debugPageHandler request = do
    html <- pageWithHostName [labelHtml (showRequest request)]
    return $ generalResponse (toHtml html)


filePageHandler :: HttpRequest -> IO HttpResponse
filePageHandler request =
    let filepath = defaultPrefix (elemOrEmpty 1 (urlSplit request)) in do
        file <- fileContent filepath
        return $ generalResponse file


-- a data file containing route information
data RouteData = RouteData { routes :: [String] }


data MainPage = MainPage { mainItems :: [String] }

instance RouteType MainPage where
    routeName main = "/"
    routeKey main = []
    routeMap main = RouteNode mainPageMap
    updateType strs = do
        return $ MainPage []


mainPageHandler :: HttpRequest -> IO HttpResponse
mainPageHandler request = do
    dev <- listBlockDevices
    html <- pageWithHostName ([(createHtmlHeading 1 "Devices")] ++ [(renderList dev)])
    return $ generalResponse (toHtml html)


mainPageMap :: String -> Maybe RouteItem
mainPageMap path
    | path == "" = Just $ RouteLeaf mainPageHandler
    | path == "dev" = Just $ RouteNode devicePageMap
    | path == "pkg" = Just $ RouteNode packagePageMap
    | path == "swc" = Just $ RouteLeaf filePageHandler
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
queryAction :: FilePath -> Partition -> String -> IO ()
queryAction dir dev query =
    if (length query) > 0
    then
        if (last query) == '1'
        then do
            mountDevice ("/dev/" ++ (strId dev)) (defaultMountPoint dir dev)
        else do
            umountDevice (defaultMountPoint dir dev)
    else do
        return ()


-- use the mount location of the filesystem
partitionPackageTable :: FilePath -> IO [HtmlContent]
partitionPackageTable path = do
    dat <- loadPackageData path "statfile"
    return [(createHtmlTable (toStorageTable dat))]


partitionInfoPage :: Partition -> IO [HtmlContent]
partitionInfoPage p = do
    mnt <- updateMounts
    let mnt_name = ("/dev/" ++ strId p) in
        case (findMountName mnt mnt_name) of
            Just m -> do
                pkgs <- partitionPackageTable (mntPath m)
                return (title ++ formStr ++ pkgs) where
                    title = [(createHtmlHeading 3 (mnt_name ++ " (" ++ (mntPath m) ++ ")"))]
            Nothing -> do
                return ([(createHtmlHeading 3 mnt_name)] ++ formStr)


partitionUrlToName :: String -> String
partitionUrlToName url = elemOrEmpty 1 (urlSplitString url)


-- use the mount location of the filesystem
devicePageBody :: FilePath -> String -> String -> IO [HtmlContent]
devicePageBody dir path query = do
    dev <- updatePartitions
    case (findPartitionName dev path) of
        Nothing -> do
            blks <- listBlock ["kname", "pkname", "maj:min", "fstype", "size", "mountpoint", "label", "uuid", "state", "model", "serial", "vendor"]
            return [(createHtmlTable (linesToHtml blks))]
        Just p -> do
            queryAction dir p query
            html <- partitionInfoPage p
            return html


devicePageHandler :: HttpRequest -> IO HttpResponse
devicePageHandler request = do
    body <- devicePageBody "/srv" (partitionUrlToName (urlString request)) (query request)
    html <- pageWithHostName body
    return $ generalResponse (toHtml html)


-- does not know which devices exist
devicePageMap :: String -> Maybe RouteItem
devicePageMap path = Just $ RouteLeaf devicePageHandler


-- show available packages
data PackagePage = PackagePage { packageItems :: [String] }


-- open statfile in each device
showAllPackages :: [Package] -> [HtmlContent]
showAllPackages pkg = ([createHtmlTable (storageToHtml pkg)])


-- filter a particular name
showPackage :: [Package] -> String -> [HtmlContent]
showPackage pkg name = ([createHtmlTable (storageToHtml pkg)])


-- use all known devices and find by name
packageTable :: FilePath -> String -> IO [HtmlContent]
packageTable dir name = do
    mnts <- mountsByPath (mountPointDir dir)
    pkgs <- getAllPackages (deviceMounts mnts)
    if (length name) == 0
    then do
        return $ showAllPackages pkgs
    else do
        return $ showPackage pkgs name


packagePageHandler :: HttpRequest -> IO HttpResponse
packagePageHandler request = do
    body <- packageTable "/srv" (urlString request)
    html <- pageWithHostName body
    return $ generalResponse (toHtml html)


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
