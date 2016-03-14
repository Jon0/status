module Device where

import Data.List
import Data.Maybe
import System.Process
import Text.Read
import Document
import File
import Html
import Package
import Util

-- a single block device
data Partition = Partition { majorId :: Int, minorId :: Int, blocks :: Int, strId :: String }

instance Table Partition where
    -- order: major, minor, blocks, name
    readLine (a:b:c:d:[]) = case (readMaybe a :: Maybe Int, readMaybe b :: Maybe Int, readMaybe c :: Maybe Int) of
        (Just ra, Just rb, Just rc) -> Just $ Partition ra rb rc d
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


data ManagedMount = ManagedMount { autoMount :: Bool, fsType :: String, fsName :: String }


-- collected relevant data about one device
data Device = Device { devPart :: Partition, devMount :: Maybe ManagedMount }


--find device by name
findPartitionName :: [Partition] -> String -> Maybe Partition
findPartitionName [] _ = Nothing
findPartitionName (x:xs) s =
    if (strId x) == s
    then Just x
    else findPartitionName xs s


-- currently not used
partitionInfoPath :: Partition -> FilePath
partitionInfoPath ds = ("/sys/dev/block/" ++ (show (majorId ds)) ++ ":" ++ (show (minorId ds)))


-- transform the device table data
partMaps :: Int -> String -> String
partMaps 3 str = htmlTagOpt "a" [("href=\"/dev/" ++ str ++ "\"")] str
partMaps _ str = str

partToHtml :: [Partition] -> [[HtmlContent]]
partToHtml tb = linesToHtml (showTableMap partMaps tb)

toPartitionTable :: [Partition] -> [[HtmlContent]]
toPartitionTable ds = [[labelHtml "maj", labelHtml "min", labelHtml "blocks", labelHtml "name"]] ++ (partToHtml ds)



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


-- get a complete list of packages
getAllPackages :: [Mount] -> IO [Package]
getAllPackages [] = do
    return []
getAllPackages (m:mnts) = do
    dat <- loadPackageData (mntPath m) "statfile"
    rpks <- getAllPackages mnts
    return $ (pkgData dat) ++ rpks


-- update using partitions file
updatePartitions :: IO [Partition]
updatePartitions = readTable "/proc/partitions"


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


listBlock :: [String] -> IO [[String]]
listBlock items = do
    result <- readProcess "lsblk" ["-r", ("-o" ++ (intercalate "," items))] ""
    return $ map allWords (lines result)



mountDevice :: Partition -> FilePath -> IO ()
mountDevice d p = do
    tryCommand ("mkdir " ++ p)
    tryCommand ("mount /dev/" ++ strId d ++ " " ++ p)


umountDevice :: FilePath -> IO ()
umountDevice p = tryCommand ("umount " ++ p)
