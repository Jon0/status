module Route where

import System.IO
import Config
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


data MainPage = MainPage {
    mainItems :: [String],
    devPage :: DevicePage
}

instance RouteType MainPage where
    routeName main = "/"
    routeKey main = []
    routeMap main = RouteNode (mainPageMap main)


createMainPage :: Config -> IO MainPage
createMainPage cfg = do
    devp <- createDevicePage cfg
    return $ MainPage [] devp


mainPageHandler :: HttpRequest -> IO HttpResponse
mainPageHandler request = do
    dev <- listBlockDevices
    html <- pageWithHostName ([(createHtmlHeading 1 "Devices")] ++ [(renderList dev)])
    return $ generalResponse (toHtml html)


mainPageMap :: MainPage -> String -> Maybe RouteItem
mainPageMap mainpage path
    | path == "" = Just $ RouteLeaf mainPageHandler
    | path == "dev" = Just $ routeMap (devPage mainpage) --RouteNode devicePageMap
    | path == "pkg" = Just $ RouteNode packagePageMap
    | path == "swc" = Just $ RouteLeaf filePageHandler
    | otherwise = Just $ RouteLeaf debugPageHandler


-- pages for connected devices
data DevicePage = DevicePage {
    deviceDir :: FilePath,
    partItems :: [Partition]
}

instance RouteType DevicePage where
    routeName dev = "dev"
    routeKey dev = []
    routeMap dev = RouteNode (devicePageMap dev)


createDevicePage :: Config -> IO DevicePage
createDevicePage cfg = do
    parts <- updatePartitions
    return $ DevicePage (contentPath cfg) parts


-- mounts and umounts devices
queryAction :: Partition -> FilePath -> String -> IO ()
queryAction dev dir query =
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


-- use the mount location of the filesystem
partitionInfoPage :: Partition -> IO [HtmlContent]
partitionInfoPage p = do
    mnt <- partToMountMaybe p
    case mnt of
        Just m -> do
            pkgs <- partitionPackageTable (mntPath m)
            return (title ++ [mountDeviceForm] ++ pkgs) where
                title = [(createHtmlHeading 3 ((strId p) ++ " (" ++ (mntPath m) ++ ")"))]
        Nothing -> do
            return ([(createHtmlHeading 3 (strId p))] ++ [mountDeviceForm])



devicePageHandler :: Partition -> FilePath -> HttpRequest -> IO HttpResponse
devicePageHandler part mountpath request = do
    queryAction part mountpath (query request)
    body <- partitionInfoPage part
    html <- pageWithHostName body
    return $ generalResponse (toHtml html)



partitionFileQuery :: DirectoryUrl -> String -> IO ()
partitionFileQuery d s = do
    putStrLn ((showDirectoryUrl d) ++ " -> " ++ s)
    if s == "?sort=1"
    then do
        pkgs <- generatePackages (fsLocation d)
        createPackageDatabase ((duMount d) ++ "/statfile") pkgs
    else do
        return ()


-- use the mount location of the filesystem
partitionFilePage :: Partition -> (String, String) -> String -> IO [HtmlContent]
partitionFilePage part (pre, dir) query = do
    mnt <- partToMountMaybe part
    case mnt of
        Just m -> let du = (DirectoryUrl (mntPath m) pre dir) in do
            partitionFileQuery du query
            content <- dirTemplate du
            return content
        Nothing -> do
            return ([(createHtmlHeading 3 ((strId part) ++ " is not mounted"))])



-- requests for file contents
deviceFilePageHandler :: Partition -> FilePath -> HttpRequest -> IO HttpResponse
deviceFilePageHandler part subdir request = do
    body <- partitionFilePage part (breakRequest request 3) (query request)
    html <- pageWithHostName body
    return $ generalResponse (toHtml html)


-- shows output of lsblk
noDevicePageHandler :: HttpRequest -> IO HttpResponse
noDevicePageHandler request = do
    blks <- listBlock ["kname", "pkname", "maj:min", "fstype", "size", "mountpoint", "label", "uuid", "state", "model", "serial", "vendor"]
    html <- pageWithHostName [(createHtmlTable (linesToHtml blks))]
    return $ generalResponse (toHtml html)



partitionPageMap :: Partition -> FilePath -> String -> Maybe RouteItem
partitionPageMap part mountpath path
        | path == "files" = Just $ RouteLeaf (deviceFilePageHandler part mountpath)
        | otherwise = Just $ RouteLeaf (devicePageHandler part mountpath)


-- does not know which devices exist
devicePageMap :: DevicePage -> String -> Maybe RouteItem
devicePageMap devpage path =
    case findPartitionName (partItems devpage) path of
        Just p -> Just $ RouteNode (partitionPageMap p (deviceDir devpage))
        Nothing -> Just $ RouteLeaf noDevicePageHandler


-- show available packages
data PackagePage = PackagePage { packageItems :: [String] }

createPackagePage :: Config -> IO PackagePage
createPackagePage cfg = do
    return $ PackagePage []


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
    then let title = ("Packages (" ++ (show (length pkgs)) ++ ")") in do
        return $ (createHtmlHeading 1 title) : (showAllPackages pkgs)
    else do
        return $ (createHtmlHeading 1 name) : (showPackage pkgs name)


packagePageHandler :: HttpRequest -> IO HttpResponse
packagePageHandler request = do
    body <- packageTable "/srv" (subUrl 1 request)
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


replyFn :: Config -> Handle -> IO ()
replyFn cfg hdl = do
    mainpage <- createMainPage cfg
    httpGetHandler (routeMap mainpage) hdl


-- match routes by regex from a file
readRoute :: String -> IO RouteData
readRoute url = do
    filedata <- fileContent "routes"
    return $ RouteData [url]
