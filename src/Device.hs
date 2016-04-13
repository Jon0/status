module Device where

import Data.Maybe
import System.IO
import Text.Read
import Config
import Content
import Document
import File
import Html
import List
import Package
import Template
import Util


-- Header data for block devices
-- Elements are either devices or partitions
data Partition = Partition { majorId :: Int, minorId :: Int, blocks :: Int, strId :: String }

instance Table Partition where
    -- order: major, minor, blocks, name
    readLine (a:b:c:d:[]) = case (readMaybe a :: Maybe Int, readMaybe b :: Maybe Int, readMaybe c :: Maybe Int) of
        (Just ra, Just rb, Just rc) -> Just $ Partition ra rb rc d
        otherwise -> Nothing
    readLine _ = Nothing

    --showLine :: rowType -> [String]
    showLine ds = [(show (majorId ds)), (show (minorId ds)), (show (blocks ds)), (strId ds)]


-- a drive containing partitions
data PartitionSet = PartitionSet {
    devKName :: String,
    devVendor :: String,
    devModel :: String,
    devSerial :: String,
    devContent :: [PartitionStat]
}

instance Renderable PartitionSet where
    renderAll dev = [(staticImage "hdd.svg" "48")] ++ (map renderRow (devContent dev))
    renderRow dev = (labelHtml (devKName dev))
    staticUrl dev = Just $ ("/dev/" ++ (devKName dev))


-- collect full relevant data about one partition
data PartitionStat = PartitionStat {
    kName :: String,
    pName :: String,
    fsType :: String,
    size :: String,
    mPoint :: String,
    uuId :: String
}

-- collect packages contained by this device
instance Container PartitionStat where
    containerHeader p = ContainerHeader (kName p) (loadPackageData (mPoint p) "statfile")


instance Renderable PartitionStat where
    renderAll dev = [(staticImage "hdd.svg" "48"), (labelHtml (kName dev)), (labelHtml (fsType dev)), (labelHtml (size dev)), (labelHtml (mPoint dev)), (deviceLink dev)]
    renderRow dev = deviceLink dev
    staticUrl dev = Just $ ("/dev/" ++ (kName dev))


renderPartitionInfo :: Partition -> Mount -> IO [HtmlContent]
renderPartitionInfo p m = do
    return (title ++ [mountDeviceForm] ++ filelink) where
        title = [(createHtmlHeading 3 ((strId p) ++ " (" ++ (mntPath m) ++ ")"))]
        filelink = [(generalHref "Browse Files" ("/dev/" ++ (strId p) ++ "/files"))]


data PartOwner = PartOwner { partItem :: Partition, partOwner :: Maybe Partition }


-- filepath where a device is mounted
data Mount = Mount { deviceName :: String, mntPath :: FilePath }

-- collect packages contained by this device
instance Container Mount where
    containerHeader m = ContainerHeader (deviceName m) (loadPackageData (mntPath m) "statfile")


instance Table Mount where
    readLine (a:b:xs) = Just $ Mount a b
    showLine m = [(deviceName m), (mntPath m)]


relativeToMount :: Mount -> FilePath -> FilePath
relativeToMount m p = stringRemove ((mntPath m) ++ "/") p


-- list of all mounts
data MountPath = MountPath { mountPath :: FilePath, deviceMounts :: [Mount] }

-- auto mount devices by matching uuid
data ManagedMount = ManagedMount { autoMount :: Bool, devUuid :: String }


partitionStatMount :: PartitionStat -> Maybe Mount
partitionStatMount p =
    if (null (mPoint p))
    then Nothing
    else Just $ Mount (kName p) (mPoint p)


deviceLink :: PartitionStat -> HtmlContent
deviceLink dev = generalHref "Open" ("/dev/" ++ (kName dev))



parseBlockDevice :: [String] -> Maybe PartitionStat
parseBlockDevice (kn:pk:fs:sz:mt:uu:[]) =
    Just $ PartitionStat kn pk fs sz mt uu
parseBlockDevice _ = Nothing


listBlockDevices :: StreamSet -> IO (StreamSet, [PartitionStat])
listBlockDevices set = do
    (newSet, parts) <- updatePartitions set
    blks <- listBlock ["kname", "pkname", "fstype", "size", "mountpoint", "uuid"]
    return $ (newSet, mapMaybe parseBlockDevice (tail blks))


renderBlockDevices :: StreamSet -> IO (StreamSet, HtmlContent)
renderBlockDevices set = do
    (newSet, dev) <- listBlockDevices set
    return $ (newSet, (renderListDiv dev))


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
findMountName m n = findElement (\x -> (deviceName x == n)) m


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


-- get all available containers
-- only finds devices mounted at /srv/storage
allContainerDevices :: FilePath -> IO [ContainerHeader]
allContainerDevices path = do
    mnts <- mountsByPath path
    return (map containerHeader (deviceMounts mnts))
