module Device where


import Data.Maybe
import System.Process
import Text.Read
import Document
import File
import Html


-- a single block device
data Device = Device { majorId :: Int, minorId :: Int, blocks :: Int, strId :: String }

instance DocNode Device where
    createHtml dev style = do
        return $ HtmlContent (HtmlTable {tableContent = [[]]})


instance Table Device where
    --readLine :: [String] -> Maybe rowType
    readLine str = Nothing

    --showLine :: rowType -> [String]
    showLine row = []


-- filepath where a device is mounted
data Mount = Mount { deviceName :: String, mntPath :: FilePath }

-- list of all mounts
data MountPath = MountPath { mountPath :: FilePath, deviceMounts :: [Mount] }


concatMaybe :: [Maybe a] -> [a]
concatMaybe (x:xs) = case x of
    Just a -> a : (concatMaybe xs)
    Nothing -> (concatMaybe xs)


-- convert file arrays of lines and words
toFileTable :: String -> [[String]]
toFileTable str = map words (lines str)


-- order: major, minor, blocks, name
toDevice :: [String] -> Maybe Device
toDevice (a:b:c:d:[]) = case (readMaybe a :: Maybe Int, readMaybe b :: Maybe Int, readMaybe c :: Maybe Int) of
    (Just ra, Just rb, Just rc) -> Just $ Device ra rb rc d
    otherwise -> Nothing
toDevice _ = Nothing

toDeviceArray :: [[String]] -> [Device]
toDeviceArray tb = mapMaybe toDevice tb


--find device by name
findDeviceName :: [Device] -> String -> Maybe Device
findDeviceName [] _ = Nothing
findDeviceName (x:xs) s =
    if (strId x) == s
    then Just x
    else findDeviceName xs s


-- devices into html table objects
toDeviceStrings :: Device -> [HtmlContent]
toDeviceStrings ds = [labelHtml (show (majorId ds)), labelHtml (show (minorId ds)), labelHtml (show (blocks ds)), labelHtml (strId ds)]


toDeviceTable :: [Device] -> [[HtmlContent]]
toDeviceTable ds = [[labelHtml "maj", labelHtml "min", labelHtml "blocks", labelHtml "name"]] ++ (map toDeviceStrings ds)


deviceInfoPath :: Device -> FilePath
deviceInfoPath ds = ("/sys/dev/block/" ++ (show (majorId ds)) ++ ":" ++ (show (minorId ds)))


toMount :: [String] -> Mount
toMount (a:b:xs) = Mount a b

-- raw file to mounts
toMountArray :: [[String]] -> [Mount]
toMountArray dat = map toMount dat


toMountStrings :: Mount -> [HtmlContent]
toMountStrings m = [labelHtml (deviceName m), labelHtml (mntPath m)]

-- mounts to table
toMountTable :: [Mount] -> [[HtmlContent]]
toMountTable ms = (map toMountStrings ms)


-- finding a mount by name
findMountName :: [Mount] -> String -> Maybe Mount
findMountName [] _ = Nothing
findMountName (x:xs) s =
    if (deviceName x) == s
    then Just x
    else findMountName xs s


-- update using partitions file
updateDevices :: IO [Device]
updateDevices = do
    prt <- fileContent "/proc/partitions"
    return $ toDeviceArray (toFileTable prt)


updateMounts :: IO [Mount]
updateMounts = do
    mnt <- fileContent "/proc/mounts"
    return $ toMountArray (toFileTable mnt)


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
