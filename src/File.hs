module File where

import System.IO
import System.Directory
import Data.List


data Package = Package { pkgName :: String, pkgFiles :: [FilePath] }
data Storage = Storage { mountPoint :: FilePath, pkgData :: Package }


--tz contents = do
--    let list = lines contents
--        return $ zipWith () [0..] list


lineToStorage :: String -> Storage
lineToStorage line = let w = (words line) in
    Storage (w !! 0) (Package (w !! 1) [])

storageToStrings :: Storage -> [String]
storageToStrings st = [(mountPoint st), (pkgName (pkgData st))]


loadPackageData :: String -> String -> IO [Storage]
loadPackageData mount datafile = do
    content <- fileContent (mount ++ "/" ++ datafile)
    return $ map lineToStorage (lines content)


toStorageTable :: [Storage] -> [[String]]
toStorageTable st = map storageToStrings st


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
