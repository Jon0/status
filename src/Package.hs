module Package where


import Data.List
import Data.Maybe
import System.Directory
import Document
import File
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


strLinesToFiles :: String -> [FilePath]
strLinesToFiles (']':str) = lines str
strLinesToFiles _ = []


strPartToPackage :: String -> [Package]
strPartToPackage ('[':str) =
        let (name, pkgs) = break (==']') str in
            [(Package name (strLinesToFiles pkgs))]
strPartToPackage _ = []


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
packageToHtml p = HtmlContent (Heading 3 (pkgName p)) : (map pathsToHtml (pkgFiles p))


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
generatePackage :: FilePath -> FilePath -> IO (Maybe Package)
generatePackage prefix path = do
    isDir <- doesDirectoryExist (prefix ++ "/" ++ path)
    if isDir
    then do
        files <- allFileContents prefix [path]
        return $ Just (Package path files)
    else do
        return Nothing



generatePackageItems :: FilePath -> [FilePath] -> IO [Package]
generatePackageItems prefix (x:xs) = do
    pkg <- generatePackage prefix x
    rest <- generatePackageItems prefix xs
    case pkg of
        Just p -> do
            return ([p] ++ rest)
        Nothing -> do
            return rest


generatePackages :: FilePath -> IO [Package]
generatePackages path = do
    items <- showDirectory path
    pkgs <- generatePackageItems path items
    return pkgs
