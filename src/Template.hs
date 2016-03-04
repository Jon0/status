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
