module Template where

import Document
import File
import Html
import Http


-- defualt page rendering
class Templatable t where
    tmplResponder :: (Renderable r) => t -> r -> HttpRequest -> IO HttpResponseHandler


dirStatusView :: DirectoryUrl -> [HtmlContent]
dirStatusView d = [title, br, mount, br, items] where
    title = createHtmlHeading 1 (duDirectory d)
    mount = createLabel ("Location: " ++ (duMount d))
    br = createBreak
    items = generalForm "" [(generalButton "Sort" "sort" "1")]


statToTableRow :: DirectoryUrl -> FileStat -> [HtmlContent]
statToTableRow d stat = [name, isDir, mimeType] where
    name = labelHtml $ htmlTagOpt "a" [(htmlRef (fullWebPath d stat))] (fileName stat)
    isDir = createLabel (show (isDirectory stat))
    mimeType = createLabel t where
        t = showFileMime (fullFsPath d stat)


-- create a file table
dirContentView :: DirectoryUrl -> IO [HtmlContent]
dirContentView d = do
    allFiles <- directoryContent (fsLocation d)
    let items = createHtmlTable $ map (statToTableRow d) allFiles in do
        return [title, items] where
            title = createHtmlHeading 1 "Content"


dirTemplate :: DirectoryUrl -> IO [HtmlContent]
dirTemplate d = do
    content <- dirContentView d
    return (status ++ [br] ++ content) where
        status = dirStatusView d
        br = createBreak


generalHref :: String -> String -> HtmlContent
generalHref label link = labelHtml $ htmlTagOpt "a" [(htmlRef link)] label


generalForm :: String -> [HtmlContent] -> HtmlContent
generalForm a e = createForm [("action", a), ("method", "get")] e


generalButton :: String -> String -> String -> HtmlContent
generalButton lb n v = createButton lb [("name", n), ("value", v)]


generalTextFeild :: String -> String -> [HtmlContent]
generalTextFeild label name = [(createLabel label), (createInput [("type", "text"), ("name", name)]), (createBreak)]


mountDeviceForm :: HtmlContent
mountDeviceForm = generalForm "" [(generalButton "Unmount" "mount" "u"), (generalButton "Mount" "mount" "rw")]


fileUploadForm :: HtmlContent
fileUploadForm = generalForm "upload" [(createInput [("type", "file"), ("name", "testupload")]), (createInput [("type", "submit")])]


generalTextForm :: String -> HtmlContent
generalTextForm name = generalForm name (generalTextFeild name name)
