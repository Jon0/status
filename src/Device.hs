module Device where


import Data.Maybe
import System.Process
import Text.Read
import Document
import File
import Html
import Package


-- a single block device
data Device = Device { majorId :: Int, minorId :: Int, blocks :: Int, strId :: String }

instance Table Device where
    -- order: major, minor, blocks, name
    readLine (a:b:c:d:[]) = case (readMaybe a :: Maybe Int, readMaybe b :: Maybe Int, readMaybe c :: Maybe Int) of
        (Just ra, Just rb, Just rc) -> Just $ Device ra rb rc d
        otherwise -> Nothing
    readLine _ = Nothing

    --showLine :: rowType -> [String]
    showLine ds = [(show (majorId ds)), (show (minorId ds)), (show (blocks ds)), (strId ds)]


-- filepath where a device is mounted
data Mount = Mount { deviceName :: String, mntPath :: FilePath }

instance Table Mount where
    readLine (a:b:xs) = Just $ Mount a b
    showLine m = [(deviceName m), (mntPath m)]


-- list of all mounts
data MountPath = MountPath { mountPath :: FilePath, deviceMounts :: [Mount] }


--find device by name
findDeviceName :: [Device] -> String -> Maybe Device
findDeviceName [] _ = Nothing
findDeviceName (x:xs) s =
    if (strId x) == s
    then Just x
    else findDeviceName xs s


-- currently not used
deviceInfoPath :: Device -> FilePath
deviceInfoPath ds = ("/sys/dev/block/" ++ (show (majorId ds)) ++ ":" ++ (show (minorId ds)))


-- transform the device table data
devMaps :: Int -> String -> String
devMaps 3 str = htmlTag "href" str
devMaps _ str = str

devToHtml :: [Device] -> [[HtmlContent]]
devToHtml tb = linesToHtml (showTableMap devMaps tb)

toDeviceTable :: [Device] -> [[HtmlContent]]
toDeviceTable ds = [[labelHtml "maj", labelHtml "min", labelHtml "blocks", labelHtml "name"]] ++ (devToHtml ds)



-- mounts to table
toMountTable :: [Mount] -> [[HtmlContent]]
toMountTable ms = linesToHtml (showTable ms)


-- finding a mount by name
findMountName :: [Mount] -> String -> Maybe Mount
findMountName [] _ = Nothing
findMountName (x:xs) s =
    if (deviceName x) == s
    then Just x
    else findMountName xs s


-- update using partitions file
updateDevices :: IO [Device]
updateDevices = readTable "/proc/partitions"


updateMounts :: IO [Mount]
updateMounts = readTable "/proc/mounts"


dirToMount :: (FilePath, String) -> Maybe Mount
dirToMount (path, name) =
    if (head name) == '.'
    then
        Nothing
    else
        Just $ Mount name (path ++ "/" ++ name)


-- assume each subdirectory is a device
mountsByPath :: FilePath -> IO MountPath
mountsByPath path = do
    dirs <- showDirectory path
    return $ MountPath path (mapMaybe dirToMount (zip (cycle [path]) dirs))


-- command line actions
tryCommand :: String -> IO ()
tryCommand cmd = do
    putStrLn cmd
    ps <- runCommand cmd
    code <- waitForProcess ps
    putStrLn $ show code


mountDevice :: Device -> FilePath -> IO ()
mountDevice d p = do
    tryCommand ("mkdir " ++ p)
    tryCommand ("mount /dev/" ++ strId d ++ " " ++ p)


umountDevice :: FilePath -> IO ()
umountDevice p = tryCommand ("umount " ++ p)
