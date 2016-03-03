module Template where


data Template = Static String | Dynamic String

temlate_file :: FilePath -> Template
template_file path = Static "data"

tmpl :: String -> String
tmpl s = s
