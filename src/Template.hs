module Template where

import File
import Html



dirStatusView :: FilePath -> FilePath -> [HtmlContent]
dirStatusView ml dir = [title, br, mount, br, items] where
    title = createHtmlHeading 1 dir
    mount = createLabel ("Location: " ++ ml)
    br = createBreak
    items = generalForm "" [(generalButton "Sort" "sort" "1")]



statToTableRow :: FilePath -> FileStat -> [HtmlContent]
statToTableRow urlprefix stat = [name, isDir] where
    name = labelHtml $ htmlTagOpt "a" [(htmlRef (urlprefix ++ (fileName stat)))] (fileName stat)
    isDir = createLabel (show (isDirectory stat))


-- create a file table
dirContentView :: FilePath -> FilePath -> FilePath -> IO [HtmlContent]
dirContentView mount url dir = do
    allFiles <- directoryContent (mount ++ dir)
    let items = createHtmlTable $ map (statToTableRow url) allFiles in do
        return [title, items] where
            title = createHtmlHeading 1 "Content"


dirTemplate :: FilePath -> FilePath -> FilePath -> IO [HtmlContent]
dirTemplate mount url dir = do
    content <- dirContentView mount url dir
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
