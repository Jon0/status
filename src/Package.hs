module Package where

import qualified Data.ByteString.Char8
import Crypto.Hash
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
    fileHash :: Digest MD5,
    fileSize :: Integer,
    fileShow :: Bool
}

instance Eq PackageFile where
    (==) a b = ((relativePath a) == (relativePath b)) && ((fileHash a) == (fileHash b))


parseMd5 :: String -> Maybe (Digest MD5)
parseMd5 s = digestFromByteString (Data.ByteString.Char8.pack s)


instance Table PackageFile where
    readLine (n:t:h:s:v:[]) = case (parseMd5 h :: Maybe (Digest MD5), readMaybe s :: Maybe Integer, readMaybe v :: Maybe Bool) of
        (Just rh, Just rs, Just rv) -> Just $ PackageFile n t rh rs rv
        otherwise -> Nothing
    readLine _ = Nothing

    showLine f = [(relativePath f), (fileMime f), (show (fileHash f)), (show (fileSize f)), (show (fileShow f))]

    keyOrder a b = compare (relativePath a) (relativePath b)


instance Renderable (Package, PackageFile) where
    renderAll (p, f) = [(renderableHref (relativePath f) (p, f)), (labelHtml (fileMime f)), (labelHtml (show (fileHash f))), (labelHtml (showFileSize (fileSize f)))]
    renderRow (p, f) = createDiv "pkg" (renderAll (p, f))
    staticUrl (p, f) = Just ("/pkg/" ++ (pkgName p) ++ "/" ++ (relativePath f))


data Package = Package {
    pkgName :: String,
    pkgFiles :: [PackageFile]
}

instance Eq Package where
    (==) a b = (pkgName a) == (pkgName b)


instance Renderable Package where
    renderAll p = [(staticImage "box.svg" "48"), (labelHtml (pkgName p)), (renderZipDiv p (pkgFiles p))]
    renderRow p = createDiv "pkg" (renderAll p)
    staticUrl p = Just $ ("/pkg/" ++ (pkgName p))


data Storage = Storage {
    mountPoint :: FilePath,
    pkgPathPairs :: [(FilePath, Package)]
}

pkgData :: Storage -> [Package]
pkgData s = let (a, b) = unzip (pkgPathPairs s) in b

instance Show Storage where
    show s = (mountPoint s)


instance Renderable Storage where
    renderAll s = [(labelHtml ("Location: " ++ (mountPoint s)))]
    renderRow s = createDiv "pkg" (renderAll s)
    staticUrl s = Nothing


packageFileLocation :: Storage -> Package -> PackageFile -> Maybe FilePath
packageFileLocation store pkg file =
    case findElement (\(x, y) -> (pkgName y) == (pkgName pkg)) (pkgPathPairs store) of
        Nothing -> Nothing
        Just (a, b) -> Just ((mountPoint store) ++ "/" ++ a ++ "/" ++ (relativePath file))


renderPackageFile :: [Storage] -> Package -> PackageFile -> [HtmlContent]
renderPackageFile stores pkg file = renderAll (pkg, file)


-- open statfile in each device
renderAllPackages :: [Storage] -> [HtmlContent]
renderAllPackages s = ([createHtmlTable (storageDevToHtml packageDevOrdering s)])


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


strPartPathName :: [String] -> (FilePath, String)
strPartPathName [] = ("", "")
strPartPathName (x:[]) = (x, x)
strPartPathName (xa:xb:xs) = (xa, xb)

strPartToPackage :: String -> [(FilePath, Package)]
strPartToPackage "" = []
strPartToPackage str =
        let (heading, pkgs) = break (=='>') str in
            let (dirpath, name) = strPartPathName (wordDelim (==';') heading) in
                [(dirpath, (Package name (strLinesToFiles pkgs)))]


strToPackages :: String -> [(FilePath, Package)]
strToPackages "" = []
strToPackages str =
    let (a, b) = break (=='<') str in
        if null a
        then (strToPackages (tail b))
        else ((strPartToPackage a) ++ (strToPackages b))


-- get packages stored on a single device
loadPackageData :: String -> String -> IO Storage
loadPackageData mount statfile = do
    content <- fileContent (mount ++ "/" ++ statfile)
    return $ Storage mount (strToPackages content)


-- use table fn
-- writing package data to files
packageFileToStr :: PackageFile -> String
packageFileToStr f = intercalate ";" (showLine f)


writePackageHead :: (FilePath, Package) -> String
writePackageHead (path, pkg) = "<" ++ path ++ ";" ++ (pkgName pkg) ++ ">\n"


writePackageBody :: (FilePath, Package) -> String
writePackageBody (path, pkg) =  (intercalate "\n" (map packageFileToStr (pkgFiles pkg)) ++ "\n")


packageToStr :: (FilePath, Package) -> String
packageToStr pkg = ((writePackageHead pkg) ++ (writePackageBody pkg) ++ "\n")


-- save to file
createPackageDatabase :: FilePath -> [(FilePath, Package)] -> IO ()
createPackageDatabase path pkgs = do
    writeFile path (concat (map packageToStr pkgs))



pathsToHtml :: FilePath -> HtmlContent
pathsToHtml p = HtmlContent (Heading 3 p)


packageToHtml :: Package -> [HtmlContent]
packageToHtml p = [(staticImage "box.svg" "48"), (renderableHref (pkgName p) p), (createLabel (show (pkgMimeTypes p)))]


storageToHtml :: [Package] -> [[HtmlContent]]
storageToHtml ps = (map packageToHtml ps)


packageDevOrdering :: ([Storage], Package) -> ([Storage], Package) -> Ordering
packageDevOrdering (sa, pa) (sb, pb) = compare (pkgName pa) (pkgName pb)


-- include extra device information
packageDevToHtml :: ([Storage], Package) -> [HtmlContent]
packageDevToHtml (s, p) = [(staticImage "box.svg" "48"), (renderableHref (pkgName p) p), (createLabel (show (pkgMimeTypes p))), (createLabel (show s))]


storageDevToHtml :: (([Storage], Package) -> ([Storage], Package) -> Ordering) -> [Storage] -> [[HtmlContent]]
storageDevToHtml order stores = (map packageDevToHtml (sortBy order (uniqueMap pkgData stores)))


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


generatePackageFile :: (FilePath, FilePath) -> IO PackageFile
generatePackageFile (full, rel) = do
    hs <- (readFileHash full)
    sz <- (readFileSize full)
    case filePathMimeMaybe rel of
        Just mt -> do
            return $ PackageFile rel mt hs sz True
        Nothing -> do
            return $ PackageFile rel [] hs sz True

-- generate package data with mountpoint and subdirectory name
generatePackage :: (FilePath, FilePath) -> IO (FilePath, Package)
generatePackage (mount, path) = do
    files <- allSubFiles (mount ++ "/" ++ path)
    pkgFiles <- mapM generatePackageFile (zipPrefix (mount ++ "/" ++ path ++ "/") files)
    return $ (path, (Package (pathToPackageName path) pkgFiles))


-- one package per subdirectory
generatePackages :: FilePath -> FilePath -> IO [(FilePath, Package)]
generatePackages mount path = do
    items <- showDirectory (mount ++ "/" ++ path)
    pkgs <- mapM generatePackage (zip (cycle [(mount ++ "/")]) (prefixSet (path ++ "/") items))
    return pkgs
