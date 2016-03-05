module File where

import System.IO
import System.Directory
import Data.List


data Package = Package { pkgName :: String, pkgFiles :: [FilePath] }
data Storage = Storage { mountPoint :: FilePath, pkgData :: [Package] }


--tz contents = do
--    let list = lines contents
--        return $ zipWith () [0..] list


lineToPackage :: String -> Package
lineToPackage line = let (w:ws) = (words line) in
    Package w ws


packageToStrings :: Package -> [String]
packageToStrings p = (pkgName p) : (pkgFiles p)


storageToStrings :: Storage -> [[String]]
storageToStrings st = (map packageToStrings (pkgData st))


loadPackageData :: String -> String -> IO Storage
loadPackageData mount datafile = do
    content <- fileContent (mount ++ "/" ++ datafile)
    return $ Storage mount (map lineToPackage (lines content))


toStorageTable :: Storage -> [[String]]
toStorageTable st = [[(mountPoint st), "files"]] ++ (storageToStrings st)


fileContent :: FilePath -> IO String
fileContent filename = do
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    return contents


getHostname :: IO String
getHostname = fileContent "/etc/hostname"


showDirectory :: FilePath -> IO [String]
showDirectory path = do
    content <- getDirectoryContents path
    return $ content
