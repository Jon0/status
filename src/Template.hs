module Template where


data Template = Static String | Dynamic String


template_file :: FilePath -> Template
template_file path = Static "data"


tmpl :: String -> String
tmpl s = s


nameToLabel :: String -> String
nameToLabel name = ("<h3>" ++ name ++ "</h3>")


dirTemplate :: [String] -> String
dirTemplate items = concat $ map nameToLabel items


toHtmlTable :: [[String]] -> String
toHtmlTable tb = ("<table>" ++ (concat $ map toTableRow tb) ++ "</table>")


toTableRow :: [String] -> String
toTableRow line
    | length line > 0 = ("<tr>" ++ (concat (map toTableItem line)) ++ "</tr>")
    | otherwise = ""


toTableItem :: String -> String
toTableItem name = ("<td>" ++ name ++ "</td>")


-- example form
formStr = unlines [
    "<form action=\"\" method=\"get\">",
    "<button name=\"mount\" value=\"0\">Unmount</button>",
    "<button name=\"mount\" value=\"1\">Mount</button>",
    "</form>"
    ]
