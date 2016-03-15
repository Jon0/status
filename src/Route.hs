module Route where

import System.IO
import Device
import Document
import File
import Html
import Http
import Package
import Template


-- a data file containing route information
data RouteData = RouteData { routes :: [String] }


-- routes ending in /
--   lead to a general list response

data DeviceList = DeviceList { deviceItems :: [Device] }


instance RouteType DeviceList where
    --routeName :: r -> String
    routeName dev = "name"
    --routeKey :: r -> [String]
    routeKey dev = []
    --routeMap :: r -> RouteItem

    --updateType :: [String] -> IO r



data PackagePage = PackagePage { packageItems :: [String] }





data MainPage = MainPage { mainItems :: [String] }

instance RouteType MainPage where
    routeName main = "/"
    routeKey main = []
    routeMap main = RouteNode mainPageMap
    updateType strs = do
        return $ MainPage []


mainPageMap :: String -> Maybe RouteItem
mainPageMap path = Nothing



mainSub :: String -> Maybe RouteItem
mainSub s = Nothing



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

-- use the mount location of the filesystem
mountPackageTable :: FilePath -> IO String
mountPackageTable path = do
    dat <- loadPackageData path "statfile"
    return $ toHtmlTable (toStorageTable dat)


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
deviceInfo :: String -> String -> IO String
deviceInfo path query = do
    dev <- updatePartitions
    case (findPartitionName dev path) of
        Nothing -> do
            blks <- listBlock ["kname", "pkname", "maj:min", "fstype", "size", "mountpoint", "label", "uuid", "state", "model", "serial", "vendor"]
            return $ toHtmlTable (linesToHtml blks)
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
    return $ RouteData [url]


-- read kernel partition info
deviceTable :: IO String
deviceTable = do
    dir <- showDirectory "/"
    dev <- updatePartitions
    mnt <- updateMounts
    return $ (toHtmlTable (toPartitionTable dev) ++ toHtmlTable (toMountTable mnt))

-- change to HttpRequest -> IO HttpResponse?
matchPattern :: String -> String -> IO String
matchPattern str query = case str of
    ('/':[]) -> deviceTable
    ('/':'p':'k':'g':'/':path) -> packageTable path
    ('/':'d':'e':'v':'/':path) -> deviceInfo path query
    otherwise -> do return "Error"

-- unused
-- forms the html page content
getPage :: String -> String -> IO String
getPage path query = do
    name <- getHostname
    content <- matchPattern path query
    return $ createPage name content


responseHeader :: String -> String
responseHeader content = ("HTTP/1.1 200 OK\nContent-Length: " ++ (show (length content)) ++ "\n\n")

-- should return HttpResponse
getLocation :: String -> String -> IO String
getLocation path query = do
    content <- getPage path query
    return (responseHeader content ++ content)




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


-- unused
httpLine :: [String] -> IO String
httpLine (verb:path:version:[]) =
    let (file, query) = break (=='?') path in do
        response <- getLocation file query
        return response
httpLine _ = do
    return $ (makeResponseLine 400) ++ "\n\n"



-- unused respond to HTTP get
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
