module Device where


import Data.Maybe
import System.Process
import Text.Read
import File


data Device = Device { majorId :: Int, minorId :: Int, blocks :: Int, strId :: String }
data Mount = Mount { deviceName :: String, mntPath :: FilePath }


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
toDeviceStrings :: Device -> [String]
toDeviceStrings ds = [(show (majorId ds)), (show (minorId ds)), (show (blocks ds)), strId ds]


toDeviceTable :: [Device] -> [[String]]
toDeviceTable ds = [["maj", "min", "blocks", "name"]] ++ (map toDeviceStrings ds)


deviceInfoPath :: Device -> FilePath
deviceInfoPath ds = ("/sys/dev/block/" ++ (show (majorId ds)) ++ ":" ++ (show (minorId ds)))


toMount :: [String] -> Mount
toMount (a:b:xs) = Mount a b

-- raw file to mounts
toMountArray :: [[String]] -> [Mount]
toMountArray dat = map toMount dat


toMountStrings :: Mount -> [String]
toMountStrings m = [(deviceName m), (mntPath m)]

-- mounts to table
toMountTable :: [Mount] -> [[String]]
toMountTable ms = (map toMountStrings ms)


updateDevices :: IO [Device]
updateDevices = do
    prt <- fileContent "/proc/partitions"
    return $ toDeviceArray (toFileTable prt)


updateMounts :: IO [Mount]
updateMounts = do
    mnt <- fileContent "/proc/mounts"
    return $ toMountArray (toFileTable mnt)


-- finding a mount by name
findMountName :: [Mount] -> String -> Maybe Mount
findMountName [] _ = Nothing
findMountName (x:xs) s =
    if (deviceName x) == s
    then Just x
    else findMountName xs s


mountDevice :: Device -> FilePath -> IO ()
mountDevice d p =
    let cmd = ("mount /dev/" ++ strId d ++ " " ++ p) in do
        putStrLn cmd
        ps <- runCommand cmd
        code <- waitForProcess ps
        putStrLn $ show code


umountDevice :: FilePath -> IO ()
umountDevice p =
    let cmd = ("umount " ++ p) in do
        putStrLn cmd
        ps <- runCommand cmd
        code <- waitForProcess ps
        putStrLn $ show code
