module Package where


import Data.List
import Data.Maybe
import System.Directory
import System.FilePath
import Text.Read
import Document
import File
import Html
import Template
import Util


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


data Package = Package {
    pkgName :: String,
    pkgFiles :: [PackageFile]
}

instance Renderable Package where
    renderAll p = [(staticImage "box.svg" "48"), (labelHtml (pkgName p))]
    renderRow p = createDiv "pkg" (renderAll p)
    staticUrl p = Just $ ("/pkg/" ++ (pkgName p))


data Storage = Storage { mountPoint :: FilePath, pkgData :: [Package] }

instance DocNode Storage where
    createHtml dev style = do
        return $ HtmlContent (HtmlTable {tableContent = [[]]})



-- open statfile in each device
renderAllPackages :: [Package] -> [HtmlContent]
renderAllPackages pkg = ([createHtmlTable (storageToHtml pkg)])


-- filter a particular name
renderPackage :: [Package] -> String -> [HtmlContent]
renderPackage pkg name = ([createHtmlTable (storageToHtml pkg)])




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
packageToHtml p = [(staticImage "box.svg" "48"), (generalHref (pkgName p) ("/pkg/" ++ (pkgName p))), (createLabel (show (pkgMimeTypes p)))]


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
