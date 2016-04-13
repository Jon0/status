module Package where


import Data.List
import Data.Maybe
import System.Directory
import System.FilePath
import Text.Read
import Document
import File
import Html
import List
import Template
import Util


-- packages are stored in containers
-- generalising types of devices
data ContainerHeader = ContainerHeader {
    containerId :: String,
    containerAllPkg :: IO Storage
}

containerOnePkg :: ContainerHeader -> String -> IO (Maybe Package)
containerOnePkg ct name = do
    stores <- containerAllPkg ct
    return $ findPackageName (pkgData stores) name


containersOnePkg :: [ContainerHeader] -> String -> IO (Maybe Package)
containersOnePkg ct name = do
    stores <- mapM containerAllPkg ct
    return $ findPackageName (concatMap pkgData stores) name


class Container c where
    containerHeader :: c -> ContainerHeader


-- filter stores containing a package name
filterStores :: [Storage] -> String -> [Storage]
filterStores [] _ = []
filterStores (d:devs) name =
    case (findPackageName (pkgData d) name) of
        Nothing -> filterStores devs name
        Just p -> (d : filterStores devs name)


getPkgContainers :: [ContainerHeader] -> String -> IO [Storage]
getPkgContainers cs name = do
    stores <- mapM containerAllPkg cs
    return $ filterStores stores name


data PackageFile = PackageFile {
    relativePath :: FilePath,
    fileMime :: String,
    fileHash :: Integer,
    fileSize :: Integer,
    fileShow :: Bool
}


instance Table PackageFile where
    readLine (n:t:h:s:v:[]) = case (readMaybe h :: Maybe Integer, readMaybe s :: Maybe Integer, readMaybe v :: Maybe Bool) of
        (Just rh, Just rs, Just rv) -> Just $ PackageFile n t rh rs rv
        otherwise -> Nothing
    readLine _ = Nothing

    showLine f = [(relativePath f), (fileMime f), (show (fileHash f)), (show (fileSize f)), (show (fileShow f))]


instance Renderable (Package, PackageFile) where
    renderAll (p, f) = [(renderableHref (relativePath f) (p, f)), (labelHtml (fileMime f)), (labelHtml (show (fileHash f))), (labelHtml (show (fileSize f)))]
    renderRow (p, f) = createDiv "pkg" (renderAll (p, f))
    staticUrl (p, f) = Just ("/pkg/" ++ (pkgName p) ++ "/" ++ (relativePath f))


data Package = Package {
    pkgName :: String,
    pkgFiles :: [PackageFile]
}

instance Renderable Package where
    renderAll p = [(staticImage "box.svg" "48"), (labelHtml (pkgName p)), (renderZipDiv p (pkgFiles p))]
    renderRow p = createDiv "pkg" (renderAll p)
    staticUrl p = Just $ ("/pkg/" ++ (pkgName p))


data Storage = Storage {
    mountPoint :: FilePath,
    pkgData :: [Package]
}

instance Renderable Storage where
    renderAll s = [(labelHtml ("Location: " ++ (mountPoint s)))]
    renderRow s = createDiv "pkg" (renderAll s)
    staticUrl s = Nothing


renderPackageFile :: [Storage] -> Package -> PackageFile -> [HtmlContent]
renderPackageFile stores pkg file = renderAll (pkg, file)


-- open statfile in each device
renderAllPackages :: [Storage] -> [HtmlContent]
renderAllPackages s = ([createHtmlTable (storageToHtml (concatMap pkgData s))])


-- filter a particular name
renderPackage :: [Storage] -> String -> [HtmlContent]
renderPackage s name =
    case (findPackageName (concatMap pkgData s) name) of
        Just p -> (renderAll p) ++ [(renderListDiv s)]
        Nothing -> [(labelHtml ("Cannot Find " ++ name))]


-- return all contained unique mime types
pkgAppendMimeTypes :: [String] -> [PackageFile] -> [String]
pkgAppendMimeTypes ts [] = ts
pkgAppendMimeTypes ts (f:fs) =
    if (null (fileMime f))
    then pkgAppendMimeTypes ts fs
    else pkgAppendMimeTypes (appendUnique ts (fileMime f)) fs


pkgMimeTypes :: Package -> [String]
pkgMimeTypes p = pkgAppendMimeTypes [] (pkgFiles p)


--tz contents = do
--    let list = lines contents
--        return $ zipWith () [0..] list


strLinesToFiles :: String -> [PackageFile]
strLinesToFiles ('>':str) = mapMaybe readLine (map (wordDelim (==';')) (lines str))
strLinesToFiles _ = []


strPartToPackage :: String -> [Package]
strPartToPackage "" = []
strPartToPackage str =
        let (name, pkgs) = break (=='>') str in
            [(Package name (strLinesToFiles pkgs))]


strToPackages :: String -> [Package]
strToPackages "" = []
strToPackages str =
    let (a, b) = break (=='<') str in
        if null a
        then (strToPackages (tail b))
        else ((strPartToPackage a) ++ (strToPackages b))


-- get packages stored on a single device
loadPackageData :: String -> String -> IO Storage
loadPackageData mount datafile = do
    content <- fileContent (mount ++ "/" ++ datafile)
    return $ Storage mount (strToPackages content)


-- use table fn
-- writing package data to files
packageFileToStr :: PackageFile -> String
packageFileToStr f = intercalate ";" (showLine f)


writePackageHead :: Package -> String
writePackageHead pkg = "<" ++ (pkgName pkg) ++ ">\n"


writePackageBody :: Package -> String
writePackageBody pkg =  (intercalate "\n" (map packageFileToStr (pkgFiles pkg)) ++ "\n")


packageToStr :: Package -> String
packageToStr pkg = ((writePackageHead pkg) ++ (writePackageBody pkg) ++ "\n")


-- save to file
createPackageDatabase :: FilePath -> [Package] -> IO ()
createPackageDatabase path pkgs = do
    writeFile path (concat (map packageToStr pkgs))



pathsToHtml :: FilePath -> HtmlContent
pathsToHtml p = HtmlContent (Heading 3 p)


packageToHtml :: Package -> [HtmlContent]
packageToHtml p = [(staticImage "box.svg" "48"), (renderableHref (pkgName p) p), (createLabel (show (pkgMimeTypes p)))]


storageToHtml :: [Package] -> [[HtmlContent]]
storageToHtml ps = (map packageToHtml ps)


storageSize :: Storage -> String
storageSize st = ((show (length (pkgData st))) ++ " files")


storageTableHeader :: Storage -> [HtmlContent]
storageTableHeader st = [HtmlContent (Heading 3 (mountPoint st)), HtmlContent (Heading 3 ("(" ++ (storageSize st)++ ")" ))]


toStorageTable :: Storage -> [[HtmlContent]]
toStorageTable st =  (storageTableHeader st) : (storageToHtml (pkgData st))


-- finds a package by name
findPackageName :: [Package] -> String -> Maybe Package
findPackageName p n = findElement (\x -> (pkgName x == n)) p

-- finds item in a package
findPackageFileName :: Package -> String -> Maybe PackageFile
findPackageFileName p n = findElement (\x -> (relativePath x == n)) (pkgFiles p)


findStores :: [Storage] -> String -> [Package]
findStores [] _ = []
findStores (d:devs) name =
    case (findPackageName (pkgData d) name) of
        Nothing -> findStores devs name
        Just p -> (p : findStores devs name)


replaceBrackets :: Char -> Char
replaceBrackets '<' = '-'
replaceBrackets '>' = '-'
replaceBrackets c = c


pathToPackageName :: FilePath -> String
pathToPackageName path = map replaceBrackets (takeFileName path)


generatePackageFile :: FilePath -> IO PackageFile
generatePackageFile p = do
    hs <- (readFileHash p)
    sz <- (readFileSize p)
    return $ PackageFile p (showFileMime p) hs sz True

-- generate package data with mountpoint and subdirectory name
generatePackage :: FilePath -> IO Package
generatePackage path = do
    files <- allSubFiles path
    pkgFiles <- mapM generatePackageFile files
    return $ Package (pathToPackageName path) pkgFiles


-- one package per subdirectory
generatePackages :: FilePath -> IO [Package]
generatePackages path = do
    items <- showDirectory path
    pkgs <- mapM generatePackage (prefixSet (path ++ "/") items)
    return pkgs
