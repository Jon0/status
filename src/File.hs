module File where


import System.IO
import System.Directory
import Data.List
import Control.Exception
import Document
import Html


data Package = Package { pkgName :: String, pkgFiles :: [FilePath] }

instance DocNode Package where
    createHtml dev style = do
        return $ HtmlContent (HtmlTable {tableContent = [[]]})


data Storage = Storage { mountPoint :: FilePath, pkgData :: [Package] }

instance DocNode Storage where
    createHtml dev style = do
        return $ HtmlContent (HtmlTable {tableContent = [[]]})


--tz contents = do
--    let list = lines contents
--        return $ zipWith () [0..] list


-- parsing functions
lineToPackage :: String -> Package
lineToPackage line = let (w:ws) = (words line) in
    Package w ws


pathsToHtml :: FilePath -> HtmlContent
pathsToHtml p = HtmlContent (Heading 3 p)


packageToHtml :: Package -> [HtmlContent]
packageToHtml p = HtmlContent (Heading 3 (pkgName p)) : (map pathsToHtml (pkgFiles p))


storageToHtml :: [Package] -> [[HtmlContent]]
storageToHtml ps = (map packageToHtml ps)


storageSize :: Storage -> String
storageSize st = ((show (length (pkgData st))) ++ " files")


storageTableHeader :: Storage -> [HtmlContent]
storageTableHeader st = [HtmlContent (Heading 3 (mountPoint st)), HtmlContent (Heading 3 ("(" ++ (storageSize st)++ ")" ))]

toStorageTable :: Storage -> [[HtmlContent]]
toStorageTable st =  (storageTableHeader st) : (storageToHtml (pkgData st))


-- get packages stored on a single device
loadPackageData :: String -> String -> IO Storage
loadPackageData mount datafile = do
    content <- fileContent (mount ++ "/" ++ datafile)
    return $ Storage mount (map lineToPackage (lines content))



findPackageName :: [Package] -> String -> Maybe Package
findPackageName [] _ = Nothing
findPackageName (p:pkgs) name =
    if (pkgName p) == name
    then
        Just p
    else
        findPackageName pkgs name



findStores :: [Storage] -> String -> [Package]
findStores [] _ = []
findStores (d:devs) name =
    case (findPackageName (pkgData d) name) of
        Nothing -> findStores devs name
        Just p -> (p : findStores devs name)


-- return hostname
getHostname :: IO String
getHostname = fileContent "/etc/hostname"


showDirectory :: FilePath -> IO [String]
showDirectory path = do
    content <- getDirectoryContents path
    return $ content


-- does the file get closed?
fileContent :: FilePath -> IO String
fileContent filename = do
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    return contents


fileContent2 :: FilePath -> IO (Maybe String)
fileContent2 filename = do
    str <- bracket (openFile filename ReadMode)
            (hClose)
            (\hdl -> do contents <- hGetContents hdl; return contents)
    return $ Just str
