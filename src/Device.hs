module Device where

import Data.Maybe
import System.IO
import Text.Read
import Config
import Content
import Document
import File
import Html
import Package
import Template


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


data PartOwner = PartOwner { partItem :: Partition, partOwner :: Maybe Partition }


-- filepath where a device is mounted
data Mount = Mount { deviceName :: String, mntPath :: FilePath }

instance Table Mount where
    readLine (a:b:xs) = Just $ Mount a b
    showLine m = [(deviceName m), (mntPath m)]


-- list of all mounts
data MountPath = MountPath { mountPath :: FilePath, deviceMounts :: [Mount] }


data ManagedMount = ManagedMount { autoMount :: Bool, devName :: String }


-- collected relevant data about one device
data Device = Device {
    kName :: String,
    pName :: String,
    fsType :: String,
    size :: String,
    mPoint :: String,
    modelStr :: String,
    uuId :: String
}

instance Renderable Device where
    renderAll dev = [(staticImage "hdd.svg"), (labelHtml (modelStr dev)), (labelHtml (fsType dev)), (labelHtml (size dev)), (labelHtml (mPoint dev)), (deviceLink dev)]
    renderRow dev = deviceLink dev
    staticUrl dev = Just $ ("/dev/" ++ (kName dev))


deviceLink :: Device -> HtmlContent
deviceLink dev = generalHref "Open" ("/dev/" ++ (kName dev))



parseBlockDevice :: [String] -> Maybe Device
parseBlockDevice (kn:pk:fs:sz:mt:md:uu:[]) =
    Just $ Device kn pk fs sz mt md uu
parseBlockDevice _ = Nothing


listBlockDevices :: StreamSet -> IO (StreamSet, [Device])
listBlockDevices set = do
    (newSet, parts) <- updatePartitions set
    blks <- listBlock ["kname", "pkname", "fstype", "size", "mountpoint", "model", "uuid"]
    return $ (newSet, mapMaybe parseBlockDevice (tail blks))



mountPointDir :: FilePath -> FilePath
mountPointDir dir = dir ++ "/storage"


defaultMountPoint :: FilePath -> Partition -> FilePath
defaultMountPoint dir part = ((mountPointDir dir) ++ "/" ++  (strId part))


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


-- transform the device table data to include links
partMaps :: Int -> String -> String
partMaps 3 str = toHtml (generalHref str ("/dev/" ++ str))
partMaps _ str = str

partToHtml :: [Partition] -> [[HtmlContent]]
partToHtml tb = linesToHtml (showTableMap partMaps tb)

toPartitionTable :: [Partition] -> [[HtmlContent]]
toPartitionTable ds = [[labelHtml "maj", labelHtml "min", labelHtml "blocks", labelHtml "name"]] ++ (partToHtml ds)


partToMountMaybe :: StreamSet -> Partition -> IO (StreamSet, Maybe Mount)
partToMountMaybe set part = do
    (newSet, mHdl) <- updateMounts set
    case mHdl of
        Nothing -> do
            return (newSet, Nothing)
        Just mnt -> do
            let mnt_name = ("/dev/" ++ strId part) in do
                return $ (newSet, findMountName mnt mnt_name)


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
    putStrLn ((mntPath m) ++ (show (length (pkgData dat))))
    rpks <- getAllPackages mnts
    return $ (pkgData dat) ++ rpks


-- update using partitions file
updatePartitions :: StreamSet -> IO (StreamSet, Maybe [Partition])
updatePartitions set = readTableFile set "/proc/partitions"


updateMounts :: StreamSet -> IO (StreamSet, Maybe [Mount])
updateMounts set = readTableFile set "/proc/mounts"


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
