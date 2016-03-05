module Device where

import Data.Maybe
import System.Process
import Text.Read
import File

data Device = Device { majorId :: Int, minorId :: Int, blocks :: Int, strId :: String }
data Mount = Mount { path :: FilePath }

concatMaybe :: [Maybe a] -> [a]
concatMaybe (x:xs) = case x of
    Just a -> a : (concatMaybe xs)
    Nothing -> (concatMaybe xs)


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


toDeviceStrings :: Device -> [String]
toDeviceStrings ds = [(show (majorId ds)), (show (minorId ds)), (show (blocks ds)), strId ds]

toDeviceTable :: [Device] -> [[String]]
toDeviceTable ds = [["maj", "min", "blocks", "name"]] ++ (map toDeviceStrings ds)


updateDevices :: IO [Device]
updateDevices = do
    prt <- fileContent "/proc/partitions"
    mnt <- fileContent "/proc/mounts"
    return $ toDeviceArray (toFileTable prt)





mountDevice :: Device -> FilePath -> IO ()
mountDevice d p = do
    hdl <- runCommand ("mount /dev/" ++ strId d ++ " " ++ p)
    print "test"
