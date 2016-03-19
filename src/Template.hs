module Template where

import Html


data Template = Static String | Dynamic String


template_file :: FilePath -> Template
template_file path = Static "data"


tmpl :: String -> String
tmpl s = s


nameToLabel :: String -> String
nameToLabel name = ("<h3>" ++ name ++ "</h3>")


dirTemplate :: [String] -> String
dirTemplate items = concat $ map nameToLabel items


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
