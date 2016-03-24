module Package where


import Data.List
import Data.Maybe
import System.Directory
import System.FilePath
import Document
import File
import Html
import Util


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


strLinesToFiles :: String -> [FilePath]
strLinesToFiles (']':str) = lines str
strLinesToFiles _ = []


strPartToPackage :: String -> [Package]
strPartToPackage "" = []
strPartToPackage str =
        let (name, pkgs) = break (==']') str in
            [(Package name (strLinesToFiles pkgs))]


strToPackages :: String -> [Package]
strToPackages "" = []
strToPackages str =
    let (a, b) = break (=='[') str in
        if null a
        then (strToPackages (tail b))
        else ((strPartToPackage a) ++ (strToPackages b))


-- get packages stored on a single device
loadPackageData :: String -> String -> IO Storage
loadPackageData mount datafile = do
    content <- fileContent (mount ++ "/" ++ datafile)
    return $ Storage mount (strToPackages content)



packageToStr :: Package -> String
packageToStr pkg = ("[" ++ (pkgName pkg) ++ "]\n" ++ (intercalate "\n" (pkgFiles pkg)) ++ "\n\n")


-- save to file
createPackageDatabase :: FilePath -> [Package] -> IO ()
createPackageDatabase path pkgs = do
    writeFile path (concat (map packageToStr pkgs))



pathsToHtml :: FilePath -> HtmlContent
pathsToHtml p = HtmlContent (Heading 3 p)


packageToHtml :: Package -> [HtmlContent]
packageToHtml p = [(createLabel (pkgName p)), (createLabel (show (length (pkgFiles p))))]


storageToHtml :: [Package] -> [[HtmlContent]]
storageToHtml ps = (map packageToHtml ps)


storageSize :: Storage -> String
storageSize st = ((show (length (pkgData st))) ++ " files")


storageTableHeader :: Storage -> [HtmlContent]
storageTableHeader st = [HtmlContent (Heading 3 (mountPoint st)), HtmlContent (Heading 3 ("(" ++ (storageSize st)++ ")" ))]


toStorageTable :: Storage -> [[HtmlContent]]
toStorageTable st =  (storageTableHeader st) : (storageToHtml (pkgData st))





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



-- generate package data with mountpoint and subdirectory name
generatePackage :: FilePath -> IO Package
generatePackage path = do
    files <- allSubFiles path
    return $ Package (takeFileName path) files


-- one package per subdirectory
generatePackages :: FilePath -> IO [Package]
generatePackages path = do
    items <- showDirectory path
    pkgs <- mapM generatePackage (prefixSet (path ++ "/") items)
    return pkgs
