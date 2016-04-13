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


-- opens and returns a file
filePageResponse :: FilePath -> IO HttpResponseHandler
filePageResponse filepath = do
    putStrLn ("Opening " ++ filepath)
    (newSet, file) <- contentOpen emptyStreamSet filepath
    case file of
        Nothing -> do
            return $ HttpResponseHandler (generalResponse "Not found") newSet
        Just dat -> do
            return $ HttpResponseHandler (streamResponse (createStreamTransfer dat (Just $ showFileMime filepath))) newSet


filePageHandler :: FilePath -> HttpRequest -> IO HttpResponseHandler
filePageHandler base request = filePageResponse (defaultStaticPrefix base (elemOrEmpty 1 (urlSplit request)))


-- a data file containing route information
data RouteData = RouteData { routes :: [String] }

-- requires reading all files before matching routes
data MainPage = MainPage {
    mainItems :: [String],
    srvDir :: FilePath,
    devPage :: DevicePage,
    pkgPage :: PackagePage
}

instance RouteType MainPage where
    routeName main = "/"
    routeKey main = []
    routeMap main = RouteNode (mainPageMap main)


createMainPage :: Config -> IO MainPage
createMainPage cfg = do
    devp <- createDevicePage cfg
    pkgp <- createPackagePage cfg
    return $ MainPage [] (contentPath cfg) devp pkgp


mainPageHandler :: HttpRequest -> IO HttpResponseHandler
mainPageHandler request = do
    (newSet, devices) <- renderBlockDevices emptyStreamSet
    html <- pageWithHostName [(staticImage "main.svg" "64"), (createHtmlHeading 1 "Devices"), devices]
    return $ HttpResponseHandler (generalResponse (toHtml html)) newSet


mainPageMap :: MainPage -> String -> Maybe RouteItem
mainPageMap m path
    | path == "" = Just $ RouteLeaf mainPageHandler
    | path == "dev" = Just $ routeMap (devPage m)
    | path == "pkg" = Just $ routeMap (pkgPage m)
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
data PackagePage = PackagePage {
    packageItems :: [String],
    containerList :: [ContainerHeader],
    storageList :: [Storage]
}

instance RouteType PackagePage where
    routeName pkg = "pkg"
    routeKey pkg = []
    routeMap pkg = RouteNode (packagePageMap pkg)


createPackagePage :: Config -> IO PackagePage
createPackagePage cfg = do
    ch <- allContainerDevices (mountPointDir (contentPath cfg))
    stores <- mapM containerAllPkg ch
    return $ PackagePage [] ch stores


packagePageItemHandler :: [Storage] -> Package -> PackageFile -> HttpRequest -> IO HttpResponseHandler
packagePageItemHandler [] _ _ _ = do
    html <- pageWithHostName [(labelHtml "Not Found")]
    return $ HttpResponseHandler (generalResponse (toHtml html)) emptyStreamSet
packagePageItemHandler (x:xs) pkg file request = filePageResponse (packageFileLocation x pkg file)


packagePageHandler :: [Storage] -> Package -> HttpRequest -> IO HttpResponseHandler
packagePageHandler stores pkg request = do
    html <- pageWithHostName ((createHtmlHeading 1 (pkgName pkg)) : (renderPackage stores (pkgName pkg)))
    return $ HttpResponseHandler (generalResponse (toHtml html)) emptyStreamSet


packageBasePageHandler :: [Storage] -> HttpRequest -> IO HttpResponseHandler
packageBasePageHandler stores request =
    let title = ("Packages (" ++ (show (length stores)) ++ " Devices)") in do
        html <- pageWithHostName ((createHtmlHeading 1 title) : (renderAllPackages stores))
        return $ HttpResponseHandler (generalResponse (toHtml html)) emptyStreamSet


-- pass only storage which include this package
packageItemPageMap :: [Storage] -> Package -> String -> Maybe RouteItem
packageItemPageMap s p "" = Just $ RouteLeaf (packagePageHandler s p)
packageItemPageMap s p name =
    case findPackageFileName p name of
        Nothing -> Nothing
        Just f -> Just $ RouteLeaf (packagePageItemHandler s p f)


-- does not know which packages exist
packagePageMap :: PackagePage -> String -> Maybe RouteItem
packagePageMap pkgp "" = Just $ RouteLeaf (packageBasePageHandler (storageList pkgp))
packagePageMap pkgp name =
    case findPackageName (concatMap pkgData (storageList pkgp)) name of
        Nothing -> Nothing
        Just p -> Just $ RouteNode (packageItemPageMap (filterStores (storageList pkgp) (pkgName p)) p)


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
