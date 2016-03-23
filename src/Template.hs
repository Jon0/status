module Template where

import File
import Html

data DirectoryUrl = DirectoryUrl {
    duMount :: FilePath,
    duWebRoot :: FilePath,
    duDirectory :: FilePath
}


noTrailingSlash :: FilePath -> FilePath
noTrailingSlash p =
    if last p == '/'
    then init p
    else p


webLocation :: DirectoryUrl -> FilePath
webLocation d = noTrailingSlash $ (duWebRoot d) ++ (duDirectory d)


fsLocation :: DirectoryUrl -> FilePath
fsLocation d = noTrailingSlash $ (duMount d) ++ (duDirectory d)


dirStatusView :: FilePath -> FilePath -> [HtmlContent]
dirStatusView ml dir = [title, br, mount, br, items] where
    title = createHtmlHeading 1 dir
    mount = createLabel ("Location: " ++ ml)
    br = createBreak
    items = generalForm "" [(generalButton "Sort" "sort" "1")]



statToTableRow :: FilePath -> FileStat -> [HtmlContent]
statToTableRow webloc stat = [name, isDir] where
    name = labelHtml $ htmlTagOpt "a" [(htmlRef (webloc ++ "/" ++ (fileName stat)))] (fileName stat)
    isDir = createLabel (show (isDirectory stat))


-- create a file table
dirContentView :: DirectoryUrl -> IO [HtmlContent]
dirContentView d = do
    allFiles <- directoryContent (fsLocation d)
    let items = createHtmlTable $ map (statToTableRow (webLocation d)) allFiles in do
        return [title, items] where
            title = createHtmlHeading 1 "Content"


dirTemplate :: FilePath -> FilePath -> FilePath -> IO [HtmlContent]
dirTemplate mount url dir = do
    content <- dirContentView (DirectoryUrl mount url dir)
    return (status ++ [br] ++ content) where
        status = dirStatusView mount dir
        br = createBreak


generalForm :: String -> [HtmlContent] -> HtmlContent
generalForm a e = createForm [("action", a), ("method", "get")] e


generalButton :: String -> String -> String -> HtmlContent
generalButton lb n v = createButton lb [("name", n), ("value", v)]


generalTextFeild :: String -> String -> [HtmlContent]
generalTextFeild label name = [(createLabel label), (createInput [("type", "text"), ("name", name)]), (createBreak)]


mountDeviceForm :: HtmlContent
mountDeviceForm = generalForm "" [(generalButton "Unmount" "mount" "0"), (generalButton "Mount" "mount" "1")]


fileUploadForm :: HtmlContent
fileUploadForm = generalForm "upload" [(createInput [("type", "file"), ("name", "testupload")]), (createInput [("type", "submit")])]


generalTextForm :: String -> HtmlContent
generalTextForm name = generalForm name (generalTextFeild name name)
