module Route where

import System.Directory
import System.IO
import Config
import Content
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



debugPageHandler :: HttpRequest -> IO HttpResponseHandler
debugPageHandler request = do
    html <- pageWithHostName [labelHtml (showRequest request)]
    return $ HttpResponseHandler (generalResponse (toHtml html)) emptyStreamSet


filePageHandler :: FilePath -> HttpRequest -> IO HttpResponseHandler
filePageHandler base request =
    let filepath = defaultStaticPrefix base (elemOrEmpty 1 (urlSplit request)) in do
        (newSet, file) <- contentOpen emptyStreamSet filepath
        case file of
            Nothing -> do
                return $ HttpResponseHandler (generalResponse "Not found") newSet
            Just dat -> do
                return $ HttpResponseHandler (streamResponse (createStreamTransfer dat (Just $ showFileMime filepath))) newSet


-- a data file containing route information
data RouteData = RouteData { routes :: [String] }


data MainPage = MainPage {
    mainItems :: [String],
    srvDir :: FilePath,
    devPage :: DevicePage
}

instance RouteType MainPage where
    routeName main = "/"
    routeKey main = []
    routeMap main = RouteNode (mainPageMap main)


createMainPage :: Config -> IO MainPage
createMainPage cfg = do
    devp <- createDevicePage cfg
    return $ MainPage [] (contentPath cfg) devp


mainPageHandler :: HttpRequest -> IO HttpResponseHandler
mainPageHandler request = do
    (newSet, devices) <- renderBlockDevices emptyStreamSet
    html <- pageWithHostName [(staticImage "main.svg" "64"), (createHtmlHeading 1 "Devices"), devices]
    return $ HttpResponseHandler (generalResponse (toHtml html)) newSet


mainPageMap :: MainPage -> String -> Maybe RouteItem
mainPageMap m path
    | path == "" = Just $ RouteLeaf mainPageHandler
    | path == "dev" = Just $ routeMap (devPage m)
    | path == "pkg" = Just $ RouteNode packagePageMap
    | path == "swc" = Just $ RouteLeaf (filePageHandler (srvDir m))
    | otherwise = Just $ RouteLeaf debugPageHandler


-- pages for connected devices
data DevicePage = DevicePage {
    deviceDir :: FilePath,
    partItems :: [Partition],
    partStreams :: StreamSet
}

instance RouteType DevicePage where
    routeName dev = "dev"
    routeKey dev = []
    routeMap dev = RouteNode (devicePageMap dev)


createDevicePage :: Config -> IO DevicePage
createDevicePage cfg = do
    (newSet, mHdl) <- updatePartitions emptyStreamSet
    case mHdl of
        Nothing -> do
            return $ DevicePage (contentPath cfg) [] newSet
        Just parts -> do
            return $ DevicePage (contentPath cfg) parts newSet


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
partitionInfoPage :: Partition -> IO (StreamSet, [HtmlContent])
partitionInfoPage p = do
    (newSet, mnt) <- partToMountMaybe emptyStreamSet p
    case mnt of
        Just m -> do
            pkgs <- partitionPackageTable (mntPath m)
            mntInfo <- renderPartitionInfo p m
            return (newSet, (mntInfo ++ pkgs))
        Nothing -> do
            return (newSet, ([(createHtmlHeading 3 (strId p))] ++ [mountDeviceForm]))


devicePageHandler :: Partition -> FilePath -> HttpRequest -> IO HttpResponseHandler
devicePageHandler part mountpath request = do
    queryAction part mountpath (query request)
    (set, body) <- partitionInfoPage part
    html <- pageWithHostName body
    return $ HttpResponseHandler (generalResponse (toHtml html)) set


-- generate file database
partitionFileQuery :: DirectoryUrl -> String -> IO ()
partitionFileQuery d s = do
    putStrLn ((showDirectoryUrl d) ++ " -> " ++ s)
    if s == "?sort=1"
    then do
        pkgs <- generatePackages (fsLocation d)
        createPackageDatabase ((duMount d) ++ "/statfile") pkgs
    else do
        return ()


-- return generated directory view or a file
partitionFileToContent :: StreamSet -> Partition -> DirectoryUrl -> String -> IO (StreamSet, StreamTransfer)
partitionFileToContent set part du query = do
    isDir <- doesDirectoryExist (fsLocation du)
    if isDir
    then do
        partitionFileQuery du query
        content <- dirTemplate du
        html <- pageWithHostName content
        return (set, (createStringTransfer (toHtml html)))
    else do
        (newSet, ct) <- contentOpen set (fsLocation du)
        case ct of
            Nothing -> do
                return (newSet, (createStringTransfer ((fsLocation du) ++ " not found")))
            Just stream -> do
                return (newSet, (createStreamTransfer stream (Just $ showFileMime (fsLocation du))))


-- requests for file contents
deviceFilePageHandler :: Partition -> FilePath -> HttpRequest -> IO HttpResponseHandler
deviceFilePageHandler part subdir request = do
    (setA, mnt) <- partToMountMaybe emptyStreamSet part
    case mnt of
        Just m -> let (u, d) = (breakRequest request 3) in
                    let du = (DirectoryUrl (mntPath m) u d) in do
                        (setB, str) <- partitionFileToContent setA part du (query request)
                        return $ HttpResponseHandler (streamResponse str) setB
        Nothing -> do
            html <- pageWithHostName [(createHtmlHeading 3 ((strId part) ++ " is not mounted"))]
            return $ HttpResponseHandler (generalResponse (toHtml html)) setA


-- shows output of lsblk
noDevicePageHandler :: HttpRequest -> IO HttpResponseHandler
noDevicePageHandler request = do
    blks <- listBlock ["kname", "pkname", "maj:min", "fstype", "size", "mountpoint", "label", "uuid", "state", "model", "serial", "vendor"]
    html <- pageWithHostName [(createHtmlTable (linesToHtml blks))]
    return $ HttpResponseHandler (generalResponse (toHtml html)) emptyStreamSet



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


-- use all known devices and find by name
packageTable :: FilePath -> String -> IO [HtmlContent]
packageTable dir name = do
    mnts <- mountsByPath (mountPointDir dir)
    let cts = (map containerHeader (deviceMounts mnts)) in do
        stores <- mapM containerData cts
        if (length name) == 0
        then let title = ("Packages (" ++ (show (length stores)) ++ " Devices)") in do
            return $ (createHtmlHeading 1 title) : (renderAllPackages stores)
        else do
            pkgStores <- getPkgContainers cts name
            return $ (createHtmlHeading 1 name) : (renderPackage pkgStores name)


packagePageHandler :: HttpRequest -> IO HttpResponseHandler
packagePageHandler request = do
    body <- packageTable "/srv" (subUrl 1 request)
    html <- pageWithHostName body
    return $ HttpResponseHandler (generalResponse (toHtml html)) emptyStreamSet


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
                sendAllResponse hdl (responseData response)
                contentCloseAll (responseSource response)


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
