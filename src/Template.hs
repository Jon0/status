module Template where

import File
import Html


dirToLabel :: String -> HtmlContent
dirToLabel text = createHtmlHeading 3 text


dirTemplate :: FilePath -> IO [HtmlContent]
dirTemplate path = do
    allFiles <- allSubFiles path
    return $ map dirToLabel allFiles


-- example form
formStr :: [HtmlContent]
formStr = [(labelHtml testFormString)]


testFormString :: String
testFormString = unlines [
    "<form action=\"\" method=\"get\">",
    "<button name=\"mount\" value=\"0\">Unmount</button>",
    "<button name=\"mount\" value=\"1\">Mount</button>",
    "</form>"
    ]


fileFormString :: String
fileFormString = unlines [
    "<form action=\"uploader\">",
    "<input type=\"file\" name=\"testupload\">",
    "<input type=\"submit\">",
    "</form>"
    ]
