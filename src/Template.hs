module Template where

import File
import Html


dirToLabel :: String -> HtmlContent
dirToLabel text = createHtmlHeading 3 text


dirTemplate :: FilePath -> FilePath -> FilePath -> IO [HtmlContent]
dirTemplate mount url dir = do
    return [(createHtmlHeading 3 (mount ++ ", " ++ url ++ ", " ++ dir))]

    --allFiles <- allSubFiles path
    --return $ map dirToLabel allFiles


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
