module Html where

class Element e where
    toHtml :: e -> String

-- generate :: String -> [Element]


data Heading = Hd Int String

instance Element Heading where
    toHtml (Hd s h) = let st = show s in
        ("<h" ++ st ++ ">" ++ h ++ "</h" ++ st ++ ">")


showElem :: (Element e) => [e] -> String
showElem es = concat $ map toHtml es


createHead :: String -> String
createHead s = ("<head><title>" ++ s ++ "</title></head>")

createBody :: String -> String
createBody s = "<body>" ++ s ++ "</body>"

createPage :: String -> String
createPage s = let e = [Hd 4 "Test", Hd 4 "123"] in
    "<!DOCTYPE html><html>" ++ createHead s ++ createBody (showElem e) ++ "</html>"
